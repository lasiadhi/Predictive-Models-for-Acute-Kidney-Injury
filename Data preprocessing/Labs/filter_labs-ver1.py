# -*- coding: utf-8 -*-
"""
Created on Mon Jul 31 22:44:02 2017

@author: ladhikari (python 2.7)

In this file, we first extract important labs and label them as intraOp 'yes' or not depending on the collect_date and time (we check this datetime with the first surgery time stamps). Then we extract features (min, max, mean, var, count) for 
important 22 lab tests 

Also, we are adding abrnormal lab features in this update. 
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

# this function will select important labs from the preOp lab file
def filter_labs():
    silentremove(FILENAME_filteredLabs)
    mycols = ['acc','Admission_Date','lab_test','raw_lab_result','coll_time','lab_result','coll_date','var_category','testname','testtype']
    preOp_df = pd.read_csv(FILENAME_preOp, sep=',', iterator=True, chunksize=10000, usecols=mycols) 
    for df in preOp_df:
        df['lab_test'] = df['lab_test'].astype(str)
        labTests = ['HCT','HGB','MCH','MCHC','MCV','RBC','RDW','WBC','MPV','PLT','HGBBG','COHBA','O2CTA','O2SATA','PO2A','HCO3A','METHBA','PCO2A','PHA','LAWB','HCTWB','FIO2']
        df= df[df['lab_test'].isin(labTests)]
        
        # combine date and time
        datetime = df['coll_date'] +' '+ df['coll_time']
        datetime.name ='coll_datetime'
        
        df = pd.concat([df, datetime], axis=1)
        df['coll_datetime'] = pd.to_datetime(df['coll_datetime']).dt.strftime('%m/%d/%Y %H:%M:%S')  # should be in 24hrs format
        df.to_csv(FILENAME_filteredLabs, mode='a',header=False, index=False)
    print 'Done filtering labs!'
                


# this fuction will add an additional column to the filtered lab file to indicate whether it is an intraOP or not 
def isIntraOp_lab():
    
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
    silentremove(FILENAME_filterLabsWithYN)
                    
    # read the filtered labs file
    labs_df = pd.read_csv(FILENAME_filteredLabs, names=['acc','Admission_Date','lab_test','raw_lab_result','coll_time','lab_result','coll_date','var_category','testname','testtype','coll_datetime','interOp_y_n'], index_col=False, iterator=True, chunksize=20000)
    for df in labs_df:
       
        df['interOp_y_n'] = df.apply(lambda row: 'yes' if ((str(row['acc']) in dic) and (pd.to_datetime(row['coll_datetime']) <= min(dic[str(row['acc'])])[1]) and (pd.to_datetime(row['coll_datetime']) >= min(dic[str(row['acc'])])[0])) else 'no', axis=1)
        df.to_csv(FILENAME_filterLabsWithYN, mode='a',header=False, index=False)
    print 'Done isIntraOp!'



def readIntraOp_labs():
    selected_labs_df = pd.read_csv(FILENAME_filterLabsWithYN, names=['acc','Admission_Date','lab_test','raw_lab_result','coll_time','lab_result','coll_date','var_category','testname','testtype','coll_datetime','interOp_y_n'], index_col=False, iterator=True, chunksize=20000)
    for df in selected_labs_df:
        df = df[df['interOp_y_n'] == 'yes']
        df.to_csv(FILENAME_intraOpLabs, mode='a',header=False, index=False)
    print('Done filtering!')
    
def extract_lab_features():
    df = pd.read_csv(FILENAME_intraOpLabs, names=['acc','Admission_Date','lab_test','raw_lab_result','coll_time','lab_result','coll_date','var_category','testname','testtype','coll_datetime','interOp_y_n'])
    df['lab_result'] = df['lab_result'].astype(float)
    #print df.head(10)
    
    #dfP = pd.pivot_table(df, index=['acc'], columns='lab_test', values='lab_result', aggfunc=[lambda x: np.var(x, ddof=1) if len(x)>1 else 0])
    
    ################### compute normal percentatges for each lab based on the loinc normal ranges ######################################
    # COHBA:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=0.0, x<1.5))/float(np.size(x)) * 100])
    dfP_COHBA_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_COHBA_abPerc = dfP_COHBA_abPerc.loc[:,'COHBA_abPerc'].fillna(0)  # fill all NANs by 0 
    
    # HCO3A:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=21.0, x<=31.0))/float(np.size(x)) * 100])
    dfP_HCO3A_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_HCO3A_abPerc = dfP_HCO3A_abPerc.loc[:,'HCO3A_abPerc'].fillna(0)  # fill all NANs by 0 

    # HCT:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=39.0, x<=51.0))/float(np.size(x)) * 100])
    dfP_HCT_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_HCT_abPerc = dfP_HCT_abPerc.loc[:,'HCT_abPerc'].fillna(0)  # fill all NANs by 0 
    
    # HGB:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=12.0, x<=16.0))/float(np.size(x)) * 100])
    dfP_HGB_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_HGB_abPerc = dfP_HGB_abPerc.loc[:,'HGB_abPerc'].fillna(0)  # fill all NANs by 0 
    
    # HGBBG:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=12.1, x<=17.0))/float(np.size(x)) * 100])
    dfP_HGBBG_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_HGBBG_abPerc = dfP_HGBBG_abPerc.loc[:,'HGBBG_abPerc'].fillna(0)  # fill all NANs by 0 
    
    # MCH:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=22.0, x<=27.0))/float(np.size(x)) * 100])
    dfP_MCH_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_MCH_abPerc = dfP_MCH_abPerc.loc[:,'MCH_abPerc'].fillna(0)  # fill all NANs by 0 
    
    # MCHC:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=33.0, x<=35.0))/float(np.size(x)) * 100])
    dfP_MCHC_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_MCHC_abPerc = dfP_MCHC_abPerc.loc[:,'MCHC_abPerc'].fillna(0)  # fill all NANs by 0 
    
    # MCV:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=80.0, x<=96.0))/float(np.size(x)) * 100])
    dfP_MCV_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_MCV_abPerc = dfP_MCV_abPerc.loc[:,'MCV_abPerc'].fillna(0)  # fill all NANs by 0 
    
    # METHBA:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=0.0, x<=2.0))/float(np.size(x)) * 100])
    dfP_METHBA_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_METHBA_abPerc = dfP_METHBA_abPerc.loc[:,'METHBA_abPerc'].fillna(0)  # fill all NANs by 0

    # PCO2A:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=35.0, x<=45.0))/float(np.size(x)) * 100])
    dfP_PCO2A_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_PCO2A_abPerc = dfP_PCO2A_abPerc.loc[:,'PCO2A_abPerc'].fillna(0)  # fill all NANs by 0
    
    # PHA:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=7.35, x<=7.45))/float(np.size(x)) * 100])
    dfP_PHA_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_PHA_abPerc = dfP_PHA_abPerc.loc[:,'PHA_abPerc'].fillna(0)  # fill all NANs by 0
    
    # PLT:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=172.0, x<=450.0))/float(np.size(x)) * 100])
    dfP_PLT_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_PLT_abPerc = dfP_PLT_abPerc.loc[:,'PLT_abPerc'].fillna(0)  # fill all NANs by 0
    
    # PO2A:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=80.0, x<=100.0))/float(np.size(x)) * 100])
    dfP_PO2A_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_PO2A_abPerc = dfP_PO2A_abPerc.loc[:,'PO2A_abPerc'].fillna(0)  # fill all NANs by 0
    
    # WBC:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=4.4, x<=11.3))/float(np.size(x)) * 100])
    dfP_WBC_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_WBC_abPerc = dfP_WBC_abPerc.loc[:,'WBC_abPerc'].fillna(0)  # fill all NANs by 0
    
    
    ###### following ranges are from other internet references ###############
    
    # FIO2:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=0.0, x<=0.7))/float(np.size(x)) * 100])
    dfP_FIO2_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_FIO2_abPerc = dfP_FIO2_abPerc.loc[:,'FIO2_abPerc'].fillna(0)  # fill all NANs by 0
    
    # HCTWB:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=39.0, x<=51.0))/float(np.size(x)) * 100])
    dfP_HCTWB_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_HCTWB_abPerc = dfP_HCTWB_abPerc.loc[:,'HCTWB_abPerc'].fillna(0)  # fill all NANs by 0 
    
    # LAWB:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=0.5, x<=2.0))/float(np.size(x)) * 100])
    dfP_LAWB_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_LAWB_abPerc = dfP_LAWB_abPerc.loc[:,'LAWB_abPerc'].fillna(0)  # fill all NANs by 0   
    
    # MPV:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=7.5, x<=11.5))/float(np.size(x)) * 100])
    dfP_MPV_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_MPV_abPerc = dfP_MPV_abPerc.loc[:,'MPV_abPerc'].fillna(0)  # fill all NANs by 0 
    
    # O2CTA:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=15.0, x<=23.0))/float(np.size(x)) * 100])
    dfP_O2CTA_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_O2CTA_abPerc = dfP_O2CTA_abPerc.loc[:,'O2CTA_abPerc'].fillna(0)  # fill all NANs by 0   

    # O2SATA:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=95.0, x<=98.0))/float(np.size(x)) * 100])
    dfP_O2SATA_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_O2SATA_abPerc = dfP_O2SATA_abPerc.loc[:,'O2SATA_abPerc'].fillna(0)  # fill all NANs by 0

    # RBC:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=3.90, x<=5.72))/float(np.size(x)) * 100])
    dfP_RBC_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_RBC_abPerc = dfP_RBC_abPerc.loc[:,'RBC_abPerc'].fillna(0)  # fill all NANs by 0
    
    # RDW:
    dfP = pd.pivot_table(df, index='acc', values='lab_result', columns='lab_test', aggfunc=[lambda x: np.sum(np.logical_and(x>=11.5, x<=14.5))/float(np.size(x)) * 100])
    dfP_RDW_abPerc = dfP.loc[:,'<lambda>'].add_suffix('_abPerc')
    dfP_RDW_abPerc = dfP_RDW_abPerc.loc[:,'RDW_abPerc'].fillna(0)  # fill all NANs by 0
    

    
    dfP = pd.concat((dfP_COHBA_abPerc, dfP_HCO3A_abPerc, dfP_HCT_abPerc, dfP_HGB_abPerc, dfP_HGBBG_abPerc, dfP_MCH_abPerc, dfP_MCHC_abPerc, dfP_MCV_abPerc, dfP_METHBA_abPerc, dfP_PCO2A_abPerc, dfP_PHA_abPerc, dfP_PLT_abPerc, dfP_PO2A_abPerc, dfP_FIO2_abPerc, dfP_HCTWB_abPerc, dfP_LAWB_abPerc,dfP_MPV_abPerc, dfP_O2CTA_abPerc, dfP_O2SATA_abPerc, dfP_RBC_abPerc, dfP_RDW_abPerc,dfP_WBC_abPerc), axis=1)
    
    
    # computing other features:
    dfP_stat = pd.pivot_table(df, index=['acc'], columns='lab_test', values='lab_result', aggfunc=[np.min,np.nanmean,np.max, lambda x: np.var(x, ddof=1) if len(x)>1 else 0, np.size])
    dfP_min = dfP_stat.loc[:,'amin'].add_suffix('_min')
    dfP_mean = dfP_stat.loc[:,'nanmean'].add_suffix('_mean')
    dfP_max = dfP_stat.loc[:,'amax'].add_suffix('_max')
    dfP_var = dfP_stat.loc[:,'<lambda>'].add_suffix('_var')
    dfP_count = dfP_stat.loc[:,'size'].add_suffix('_count')
    dfP = pd.concat((dfP_min, dfP_mean, dfP_max, dfP_var, dfP_count, dfP), axis=1)
    
    # write the dfP to disk
    dfP.to_csv(FILENAME_labFeatures)
    print 'Feature extraction done!'
    #return dfP
    

# this is the main function that calls all the above functions
if __name__ == '__main__':
    FILENAME_preOp = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Labs/old_data/preop_labs.csv"
    FILENAME_opTime = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Labs/old_data/monitor_key_st_end.csv"
    FILENAME_filteredLabs = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Labs/cleaned_data/selected_labs.csv"
    FILENAME_filterLabsWithYN = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Labs/cleaned_data/selected_labs_interOpYN.csv"
    FILENAME_intraOpLabs = 'S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Labs/cleaned_data/intraOp_labs.csv'
    FILENAME_labFeatures = 'S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Labs/cleaned_data/lab_features.csv'
    
    #filter_labs()
    #isIntraOp_lab()
    #readIntraOp_labs()
    extract_lab_features()
