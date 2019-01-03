# -*- coding: utf-8 -*-
"""
Created on Wed Oct 25 22:45:43 2017

@author: ladhikari

Compute PF ratio (or SF ratio) within surgery start+24hrs
"""

import pandas as pd
import os, errno
import csv
from collections import defaultdict
from datetime import datetime, timedelta
import numpy as np
from functools import partial

# this function will remove the the file with the argument name
def silentremove(filename):
    try:
        os.remove(filename)
    except OSError as e:
        if e.errno != errno.ENOENT:
            raise

# add a new Y/N column to the given vital dataset if the observation happens between op_start and op_start+24hrs
def isOPday(FILENAME_vital, FILENAME_vital_OpDayYN, FILENAME_vital_OpDay):
    
    # create a dictionary for accounts with surgery start and stop times
    dic = defaultdict(dict)
    with open(FILENAME_opTime, 'r') as file:
        reader = csv.DictReader(file)
        for row in reader:
            # combine date and time into one variable
            if row['st_date'] and row['st_time'] and row['end_date'] and row['end_time'] and row['acc']:  # check if all cells are not null 
                st_datetime = row['st_date'] +' '+ row['st_time']
                end_datetime = row['end_date'] +' '+ row['end_time']
                st_datetime = datetime.strptime(st_datetime, '%m/%d/%Y %I:%M:%S %p')
                end_datetime = datetime.strptime(end_datetime, '%m/%d/%Y %I:%M:%S %p')
                
                # add start and end surgery times to the dictionary called dic
                if not row['acc'] in dic:
                    dic[row['acc']] = [(st_datetime, end_datetime)]
                else:
                    dic[row['acc']].append((st_datetime, end_datetime))
    
    # remove existing file        
    silentremove(FILENAME_vital_OpDayYN)
    #names=['index','time_stamp', 'obser','acc','OpDay_YN']
    # read the filtered labs file
    labs_df = pd.read_csv(FILENAME_vital, index_col=False, iterator=True, chunksize=20000)
    for df in labs_df:
        df.columns = ['index','time_stamp', 'obser','acc']
        #df['interOp_y_n'] = df.apply(lambda row: 'yes' if ((str(row['acc']) in dic) and (pd.to_datetime(row['coll_datetime']).date() == min(dic[str(row['acc'])])[0].date()))  else 'no', axis=1)
        #df['interOp_y_n'] = df.apply(lambda row: 'yes' if ((str(row['acc']) in dic) and ((pd.to_datetime(row['coll_datetime']) <= min(dic[str(row['acc'])])[1]) and (pd.to_datetime(row['coll_datetime']) >= min(dic[str(row['acc'])])[0]))) else 'no', axis=1)
        df['OpDay_YN'] = df.apply(lambda row: 'yes' if ((str(row['acc']) in dic) and ((pd.to_datetime(row['time_stamp']) <= min(dic[str(row['acc'])])[0]+timedelta(days=1)) and (pd.to_datetime(row['time_stamp']) >= min(dic[str(row['acc'])])[0]))) else 'no', axis=1)
        df.to_csv(FILENAME_vital_OpDayYN, mode='a',header=False, index=False)
    print ('Done, isIntraOp_day!')

    df_YN = pd.read_csv(FILENAME_vital_OpDayYN, names=['index','time_stamp', 'obser','acc','OpDay_YN'], index_col=False, iterator=True, chunksize=20000)
    writeHeader = True
    for df in df_YN:
        df = df[df['OpDay_YN'] == 'yes']
        if writeHeader:
            df.to_csv(FILENAME_vital_OpDay, mode='a',header=True, index=False)
            writeHeader = False
        else:
            df.to_csv(FILENAME_vital_OpDay, mode='a',header=False, index=False)
    print('Done filtering!')
    
def vital_dic(FILENAME_vital_OpDay):
    dic = defaultdict(dict)
    with open(FILENAME_vital_OpDay, 'r') as file:
        reader = csv.DictReader(file)
        for row in reader:
            if row['acc'] and row['obser'] and float(row['obser']) != 0.0:  # check if all cells are not null 
                if not row['acc'] in dic:
                    dic[row['acc']] = [row['obser']]
                else:
                    dic[row['acc']].append(row['obser'])
    return dic
    

# compute SF-ratio
def compute_SF_ratio(x, dic1_SPO2, dic2_FIO2):
    #return len(dic1_SPO2[str(int(x['acc']))])
    #return min([(float(SPO2)/float(FIO2)) for SPO2 in dic1_SPO2[str(x['acc'])] for FIO2 in dic2_FIO2[str(x['acc'])]])
    if str(int(x['acc'])) in dic1_SPO2 and str(int(x['acc'])) in dic2_FIO2:
        # compute the SF ratio and return the minimum of the all combiniations:
        SF = [(float(SPO2)/(float(FIO2)/100.0)) for SPO2 in dic1_SPO2[str(int(x['acc']))] for FIO2 in dic2_FIO2[str(int(x['acc']))]]
        if len(SF) != 0:
            return min(SF)
        else:
            return -1  #if something is wrong 
    else:
        return np.NaN  # cannot compute SF ratio
    

# compute PF-ratio
def compute_PF_ratio(x, dic1_PO2A, dic2_FIO2):
    if str(int(x['acc'])) in dic1_PO2A and str(int(x['acc'])) in dic2_FIO2:
        # compute the SF ratio and return the minimum of the all combiniations:
        PF = [(float(PO2A)/(float(FIO2)/100.0)) for PO2A in dic1_PO2A[str(int(x['acc']))] for FIO2 in dic2_FIO2[str(int(x['acc']))]]
        if len(PF) != 0:
            return min(PF)
        else:
            return -1  #if something is wrong 
    else:
        return np.NaN  # cannot compute SF ratio
    

# use SF -> PF formula to find missing PF values
def refillNaNbyFormula(row):
    if np.isnan(row['PF-ratio']) and ~np.isnan(row['SF-ratio']):
        return (row['SF-ratio'] - 64)/0.84
    else:
        return row['PF-ratio']

if __name__ == '__main__':
    
    #for ubuntu:
    # read
    FILENAME_opTime = "/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Labs/old_data/monitor_key_st_end.csv"
    FILENAME_SPO2 = "/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/IntraOp/Clean_data/SpO2_cleaned_data.csv"
    
    #FILENAME_FIO2 = "/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/IntraOp/codes/fio2_all.csv"
    
    # Lasi ran paul's code to clean the FIO2: 
    FILENAME_FIO2  = "/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/IntraOp/Clean_data/fio2_cleaned.csv"
    FILENAME_PO2A = "/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/IntraOp/codes/po2a_all.csv"
    FILENAME_intraop  = "/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model/old_codesAndData/Input_Data_For_Gam.csv"
    
    
#    #for windows:
#    # read
#    FILENAME_opTime = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Labs/old_data/monitor_key_st_end.csv"
#    FILENAME_SPO2 = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/IntraOp/Clean_data/SpO2_cleaned_data.csv"
#    
#    FILENAME_FIO2 = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/IntraOp/codes/fio2_all.csv"
#    FILENAME_intraop    = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model/old_codesAndData/Input_Data_For_Gam.csv"
    
    #write
    FILENAME_SPO2_OpDayYN = "SPO2_OpdayYN.csv"
    FILENAME_SPO2_OpDay = "SPO2_OpDay.csv"
    
    FILENAME_FIO2_OpDayYN = "FIO2_OpdayYN.csv"
    FILENAME_FIO2_OpDay = "FIO2_OpDay.csv"
    
    FILENAME_PO2A_OpDayYN = "PO2A_OpdayYN.csv"
    FILENAME_PO2A_OpDay = "PO2A_OpDay.csv"
    
    FILENAME_final_ratios = "PF-SF_final.csv"
    
    # generate clean data files on surgery day
    #isOPday(FILENAME_SPO2, FILENAME_SPO2_OpDayYN, FILENAME_SPO2_OpDay)
    #isOPday(FILENAME_FIO2, FILENAME_FIO2_OpDayYN, FILENAME_FIO2_OpDay)
    #isOPday(FILENAME_PO2A, FILENAME_PO2A_OpDayYN, FILENAME_PO2A_OpDay)
    
    # create dictonary for SPO2:
    dic_SPO2 = vital_dic(FILENAME_SPO2_OpDay)
    
    # now create dictonary for FIO2:
    dic_FIO2 = vital_dic(FILENAME_FIO2_OpDay)
    
    # create dictonary for PO2A:
    dic_PO2A = vital_dic(FILENAME_PO2A_OpDay)
    
    
    ##### compute SF ratio = SPO2/FIO2 for every pair and pick the minimum
    df_Acc = pd.read_csv(FILENAME_intraop, index_col=False) # read final acc from the time-series dataset
    SF = df_Acc.apply(partial(compute_SF_ratio,dic1_SPO2=dic_SPO2, dic2_FIO2=dic_FIO2), axis=1)
    SF.name='SF-ratio'
    df_PF_SF = pd.concat([df_Acc['acc'], SF], axis=1)
    
    ###### compute PF ratio = PAO2/FIO2 for evry pair and pick the min
    PF = df_Acc.apply(partial(compute_PF_ratio,dic1_PO2A=dic_PO2A, dic2_FIO2=dic_FIO2), axis=1)
    PF.name='PF-ratio'
    df_PF_SF = pd.concat([df_PF_SF, PF], axis=1)
    
    #Replace missing PF-ratios using the formula: (SF - 64)/0.84
    df_PF_SF['PF-ratio_modified'] = df_PF_SF.apply(refillNaNbyFormula, axis=1)
    
    #write the datafrme:
    df_PF_SF.to_csv(FILENAME_final_ratios, index=False)
