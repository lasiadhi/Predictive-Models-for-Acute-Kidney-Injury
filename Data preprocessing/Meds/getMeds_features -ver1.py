# -*- coding: utf-8 -*-
"""
Created on Wed Aug 09 08:58:45 2017

@author: ladhikari

This python 2.7 code will generate meds features for intraOp model. In particular, here we consider only 5 drug groups: pressors, 
diuretic, estimated blood loss, urine output, and blood products + other fluids.

Output: Is 1 or 0 during the surgery for pressors and diuretic? For all others, we output sum in ml. 
"""

import pandas as pd
import os, errno
import csv
from collections import defaultdict
from datetime import datetime
import numpy as np

# this function will remove the the file with the argument name
def silentremove(filename):
    try:
        os.remove(filename)
    except OSError as e:
        if e.errno != errno.ENOENT:
            raise
            


# this fuction will add an additional column to the cleaned meds file to indicate whether it is an intraOP or not 
def isIntraOp_drug():
    
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
    silentremove(FILENAME_cleaned_medsWithYN)
                    
    # read the filtered labs file
    meds_df = pd.read_csv(FILENAME_cleaned_meds, index_col=False, iterator=True, chunksize=20000, header=0)
    writeHeader= True
    for df in meds_df:
        #print df
        df['interOp_y_n'] = df.apply(lambda row: 'yes' if ((str(row['acc']) in dic) and (pd.to_datetime(row['dt'], format='%d%b%y:%H:%M:%S') <= min(dic[str(row['acc'])])[1]) and (pd.to_datetime(row['dt'], format='%d%b%y:%H:%M:%S') >= min(dic[str(row['acc'])])[0])) else 'no', axis=1)
        
        # change drug_group to fluid for all fluids except blood_product
        df.loc[(df['drug_group'] != 'blood_product') & (df['fluid_gr'] == 'fluid'), 'drug_group'] = 'fluid'
        
        if writeHeader is True:
            df.to_csv(FILENAME_cleaned_medsWithYN, mode='a',header=True, index=False)
            writeHeader= False
        else:
            df.to_csv(FILENAME_cleaned_medsWithYN, mode='a',header=False, index=False)
    print ('Done isIntraOp!')

    #meds_df.columns = ['drug','unit','source','acc','ID','dose','dt','drug_subgroup','drug_group','fluid','interOp_y_n']



# read intraOp meds and extract features for accounts
def extract_meds_binary_features():
    # read the drugs with intraOp Y or N
    meds_df = pd.read_csv(FILENAME_cleaned_medsWithYN, usecols=['acc','drug_group','dose','interOp_y_n'], index_col=False)
    
    # Filter out 'interOp_y_n' == 'no'
    meds_df = meds_df[meds_df['interOp_y_n'] == 'yes']
    
    # creat a pivote table to count number of drugs for each acc
    # meds_dfP = pd.pivot_table(meds_df, index=['acc'], columns='drug_group', values='dose', aggfunc=[lambda x: 0 if all(np.isnan(x)) else 1]) # why NaN is not replaced by 0?
    meds_dfP = pd.pivot_table(meds_df, index=['acc'], columns='drug_group', values='dose', aggfunc=[np.nansum]) 
    ## np.sum(d1['estimated blood loss'].isnull())
    
    meds_dfP = meds_dfP['nansum']
    meds_dfP = meds_dfP[~meds_dfP['urine output'].isnull()]  # drop all accounts with missing urine output. Bec it is uncertain. 
    meds_dfP['diuretic'] = (meds_dfP['diuretic']>0).astype(int)   # make diuretic a binary varaiable 
    meds_dfP['pressors'] = (meds_dfP['pressors']>0).astype(int)   # make pressors a binary variable
    
    #meds_dfP = meds_dfP.fillna(0)  # replace all remaining nans by 0 
    
    # rename column names with relevent unit label
    meds_dfP.columns = ['blood_product_ml','diuretic','estimated_blood_loss_ml','fluid_ml','pressors','urine_output_ml']
#    
#    # remove existing file        
    silentremove(FILENAME_medFeatures)
#        
#    # write the meds_dfP to disk
    meds_dfP.to_csv(FILENAME_medFeatures)
#    print 'Meds feature extraction done!'







# this is the main function that calls all the above functions
if __name__ == '__main__':
    
    FILENAME_cleaned_meds = 'S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Meds/Cleaned_data/meds_stackedData_fromSAS.csv'
    FILENAME_opTime = 'S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/IntraOp/Raw_intraOp_data/monitor_key_st_end.csv'
    FILENAME_cleaned_medsWithYN ='S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Meds/Cleaned_data/drug_gp_model_var_YN.csv'
    FILENAME_medFeatures = 'S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Meds/Cleaned_data/meds_features.csv'
    
    
    #isIntraOp_drug()
    dd = extract_meds_binary_features()
    

