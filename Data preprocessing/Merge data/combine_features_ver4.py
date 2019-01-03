# -*- coding: utf-8 -*-
"""
Created on Thu Aug 10 09:46:24 2017

@author: ladhikari , Python 3.6

In this code, we combine all intraOp features (MAC, MAP, HR), lab features, and meds features with AKI outcome variable. 

Fill missing values from the lab features from the surgery day (24hrs) starting from the surgery start time. If still missing, use median imputation. 

Add PF-ratio as a new feature and drop FIO2 and PO2A from the dataset. 

Modified the code with more outlier detection for more columns. 
"""


import pandas as pd
import os, errno
import csv
from collections import defaultdict
from datetime import datetime,time
import numpy as np


def silentremove(filename):
    try:
        os.remove(filename)
    except OSError as e:
        if e.errno != errno.ENOENT:
            raise

#calculates median deviation of an array - copied from preOp model codes
def median_deviation(vect):
    vect=pd.DataFrame(vect)
    vect = vect.astype(float)
    #vect = vect[~pd.isnull(vect)]     # commented by Lasith
    vect = vect.dropna()  
    med = np.nanmedian(vect)
    return np.nanmedian(np.abs(vect - med))

#finds out the indexes of outlier samples	 - copied from preOp model codes
def outlier_detect_fill_missing(df):
    
    tot_lab_col = len(df.columns)
    for i in range(0, tot_lab_col):  # for each lab data col
        
        vect = df.iloc[:,i]
        median_dev=median_deviation(vect)
        x_bar=np.nanmean(np.array(vect))
        vect=np.array(vect)
        vect_array=pd.DataFrame(vect)
        #vect_array = vect_array[~pd.isnull(vect_array)]   #???? no need?
        
        #find values corresponding to 0.01 quantile and 0.99 quantile
        minval=vect_array.quantile(q=0.01,axis=0)
        maxval=vect_array.quantile(q=0.99,axis=0)
        #find indexes having values less than 0.01 quantile and greater than 0.99 quantile values.
        ind_l=vect_array[vect_array[0]<minval[0]].index.tolist()
        #print ind_l
        ind_u=vect_array[vect_array[0]>maxval[0]].index.tolist()
        #print ind_u
        ind=vect_array[np.isnan(vect)].index.tolist()   # indices for Nan locations
        #out_ind=[]
        if(len(ind)>0):
            #replaces na values with mean value/median of the column 
            x_median = np.nanmedian(vect)
            vect_array.iloc[ind]=x_median   # replace x_median by x_bar for mean imputation
        #calculates z score for each value of the column 
        zvalue=pd.DataFrame(abs(0.6745*(vect-x_bar)))
        zvalue=(zvalue/median_dev)
        #checks high probability by checking z score
        ind1=(zvalue[zvalue[0]>5]).index.tolist()
        #print ind1
        ind=np.intersect1d(ind1,ind_l,assume_unique=True)
        new_vect=vect_array.copy()
        if(len(ind)>0):
            new_vect.iloc[ind]=np.reshape(np.random.uniform(vect_array.quantile(q=0.005,axis=0),vect_array.quantile(q=0.05,axis=0),len(ind)),(-1,1))
            # assign values between 0.005 quantile and 0.05 quantile of vect_array to new_vect for 
            # indexes having values less than 0.01 quantile and Z_score>5
            #out_ind=ind
        ind=np.intersect1d(ind1,ind_u,assume_unique=True)
        if(len(ind)>0):
            new_vect.iloc[ind]=np.reshape(np.random.uniform(vect_array.quantile(q=0.95,axis=0),vect_array.quantile(q=0.995,axis=0),len(ind)),(-1,1))
            # assign values between 0.95 quantile and 0.995 quantile of vect_array to new_vect for 
            # indexes having values greater than 0.99 quantile and Z_score>5
            #out_ind=np.concatenate((out_ind,ind),axis=0)
        ##df.iloc[:,i] = df.iloc[:,i].reset_index(drop=True)
        df.iloc[:,i] = new_vect[0].values
        #less=np.reshape(np.random.uniform(vect_array.quantile(q=0.005,axis=0),vect_array.quantile(q=0.05,axis=0),1),(-1,1))
        #greater=np.reshape(np.random.uniform(vect_array.quantile(q=0.95,axis=0),vect_array.quantile(q=0.995,axis=0),1),(-1,1))
        #df=pd.DataFrame({'Feature':colname,'mean':x_bar, 'median':median_dev,'maxval':maxval,'minval':minval,'less':less[0],'greater':greater[0]})
        #df.to_csv(str(colname)+".csv")
    
    # round columns with counts to nearest integer
    #df.iloc[:,89:111] = df.iloc[:,89:111].round(0)
    
    return df


def combine_meds_with_AKI(FILENAME_intraop, FILENAME_meds):
    
    # read intraOp time-series dataset to dataframes
    intraOp_df  = pd.read_csv(FILENAME_intraop, index_col=False)
    intraOp_df  = intraOp_df.drop(intraOp_df.columns[0], axis=1)  #drop unnamed index col
    intraOp_df = intraOp_df.drop('outcome', axis=1)   # drop old outcome column 
    
    #outlier detection:
    intraOp_df.iloc[:,1:] = outlier_detect_fill_missing(intraOp_df.iloc[:,1:])
    

    med_df      = pd.read_csv(FILENAME_meds,index_col=False)
    # Drop all accounts/rows with atleast one NAN
    #med_df      =  med_df.dropna(axis=0, how='any')
    
    ## Imputing missing values:
    # replace NANs in bllod products by 0 by asumming they did not receive.
    med_df['blood_product_ml'].fillna(0, inplace=True)
    
    # Missing EBL values are replaced by the median, i.e., 150ml. 
    med_df['estimated_blood_loss_ml'].fillna(np.nanmedian(med_df['estimated_blood_loss_ml']), inplace=True)
    
    # Missing fluids are replaced by 0; assume they did not receive
    med_df['fluid_ml'].fillna(0, inplace=True)
    
    #outlier detection:
    med_df.iloc[:,1:] = outlier_detect_fill_missing(med_df.iloc[:,1:])
    
    # inner join meds with intraop
    intraOp_med_df = pd.merge(intraOp_df, med_df, on='acc', how='inner')
    
    return intraOp_med_df

        
    
    
def combine_labs_with_AKI(df):    
    
    use_all_cols = 1
    
    if use_all_cols == 1:
        # use all lab features in the dataset
        lab_df  = pd.read_csv(FILENAME_labs,index_col=False) 
    else:
        #only using subset of lab features based on their frequency
        lab_df  = pd.read_csv(FILENAME_labs, usecols=['acc','HCTWB_min','HCTWB_mean','HCTWB_max','HCTWB_var','HCTWB_count','LAWB_min','LAWB_mean','LAWB_max','LAWB_var','LAWB_count']) 
    
    #drop following labs assuming we have PF-ratio as a feature:
    lab_df.drop(['PO2A_min','PO2A_mean','PO2A_max','PO2A_var','PO2A_count','PO2A_abPerc','FIO2_min','FIO2_mean','FIO2_max','FIO2_var','FIO2_count','FIO2_abPerc'], axis = 1, inplace = True)
    
    ############## fill missing values during the surgery by the values/features during the surgery day
    # read lab features from surgery day:
    lab_df_surgeDay = pd.read_csv(FILENAME_SurgDay_labs,index_col=0)
    # filter out acc based on lab_df datset:
    lab_df2 = lab_df.set_index('acc')  # make acc the index
    lab_df_day_val = lab_df_surgeDay[lab_df_surgeDay.index.isin(lab_df2.index)]  #pick same acc in the lab_df2 from lab_df_surgeDay
    lab_df = lab_df2.where(~np.isnan(lab_df2), other=lab_df_day_val)  # replace all nans in lab_df2 by lab_df_surgeDay
    lab_df.reset_index(level=0, inplace=True) # set index col:acc as the first col of the lab_df
    
    ## impute missing labs (mean/median) and outlier detection
    lab_df.iloc[:,1:] = outlier_detect_fill_missing(lab_df.iloc[:,1:])
    lab_df.iloc[:,81:101] = lab_df.iloc[:,81:101].round(0)  #round all count features to nearest int
    
    # inner join labs with intraop_meds
    all_df = pd.merge(df, lab_df, on='acc', how='inner')   # keep accounts only in both intrOp timeseries and labs
    
    return all_df

def combine_PF_ratio(df):
    df_PF  = pd.read_csv(FILENAME_PF, usecols=['acc','PF-ratio_modified'], index_col=False)
    # filter nans and negatives in the PF col
    df_PF = df_PF[(~np.isnan(df_PF['PF-ratio_modified'])) & (df_PF['PF-ratio_modified'] >= 0.0)]
    df_PF.columns = ['acc', 'PF-ratio']
    
    ## outlier detection
    df_PF['PF-ratio'] = outlier_detect_fill_missing(pd.DataFrame(df_PF['PF-ratio']))
    
    all_df = pd.merge(df, df_PF, on='acc', how='inner')
    return all_df


def create_surgery_time_dic():
    
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
    
    return dic

def add_surgery_time_feature(df):
    
    # create a new col for surgery time (min)
    df['acc'] = df['acc'].astype('str')
    dic = create_surgery_time_dic()
    for index, row in df.iterrows():
        start_time = pd.to_datetime((min(dic[str(row['acc'])])[0]))
        end_time = pd.to_datetime(min(dic[str(row['acc'])])[1])
        time_min = (end_time - start_time).seconds/60
        df.loc[df['acc']==row['acc'], 'surg_time_min']= time_min
        
        # if start_time is between 6pm and 6am ==> 1; otherwise 0
        #pm_6 = pd.datetime.strptime('18:00', '%H:%M').time()
        pm_6 = time(18,00)
        #am_6 = pd.datetime.strptime('6:00', '%H:%M').time()
        am_6 = time(6,00)
        
        if (start_time.time() >= pm_6) or (start_time.time() <= am_6):
            df.loc[df['acc']==row['acc'],'isNight_surg'] = 1
        else:
            df.loc[df['acc']==row['acc'],'isNight_surg'] = 0
        #df['isNight_surg'] = df.apply(lambda row: 1 if (pm_6 <= start_time.time()) and (am_6 >= start_time.time()) else 0, axis=1)

    ## outlier detection
    df['surg_time_min'] = outlier_detect_fill_missing(pd.DataFrame(df['surg_time_min']))
    
    return df


    

def add_anesthesia_type(df):
    # create a dic with anes_type for each opeartion for all acc
    dic = defaultdict(dict)
    with open(FILENAME_anes, 'r') as file:
        reader = csv.DictReader(file)
        for row in reader:
            start_time = pd.to_datetime(row['st_dt'], format='%d%b%y:%H:%M:%S')
            if not row['acc'] in dic or not start_time in dic[row['acc']]:
                dic[row['acc']][start_time] = [(row['anes_type'])]
            else:
                dic[row['acc']][start_time].append((row['anes_type']))
         
    # add anes_type to each acc in the final df
    for index, row in df.iterrows():
        # pick the anes_type from the first surgery for given acc
        df.loc[df['acc']==row['acc'], 'anes_type'] = dic[row['acc']][min(dic[row['acc']].keys())][0]
    
    return df
    
def add_AKI_outcome(df):
    
    ## AKI 3day
    aki_df  = pd.read_csv(FILENAME_AKI3_outcomes,index_col=False)
    # create acc and AKI outcome map via a dictionary
    aki_3day_dic = dict(zip(aki_df.acc, aki_df.aki3day))
    
    df['aki3day'] = df['acc'].astype('uint64').map(aki_3day_dic)
    
    ## AKI 7day
    aki_df  = pd.read_csv(FILENAME_AKI7_outcomes,index_col=False)
    # create acc and AKI outcome map via a dictionary
    aki_7day_dic = dict(zip(aki_df.acc, aki_df.aki7day))
    
    df['aki7day'] = df['acc'].astype('uint64').map(aki_7day_dic)
    
    ## AKI over all
    aki_df  = pd.read_csv(FILENAME_AKIall_outcomes,index_col=False)
    # create acc and AKI outcome map via a dictionary
    aki_all_dic = dict(zip(aki_df.acc, aki_df.aki_overall))
    
    df['aki_overall'] = df['acc'].astype('uint64').map(aki_all_dic)
    
    return df
    

# this is the main function that calls all the above functions
if __name__ == '__main__':
    
    
    ### For Ubuntu:
    os.chdir('/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith')
    FILENAME_intraop    = "Model/old_codesAndData/Input_Data_For_Gam.csv"  #intraop statistical features from Shivam and Paul
    FILENAME_meds       = "Meds/Cleaned_data/meds_features.csv"
    FILENAME_labs       = "Labs/cleaned_data/lab_features.csv"
    FILENAME_opTime     = "Labs/old_data/monitor_key_st_end.csv"
    FILENAME_anes       = "Time_series/Raw_intraOp_data/monitor_st_end_anes_type.csv"
    FILENAME_AKI3_outcomes = "AKI_Outcome/declare_aki_outcome_in_3days.csv"
    FILENAME_AKI7_outcomes = "AKI_Outcome/declare_aki_outcome_in_7days.csv"
    FILENAME_AKIall_outcomes = "AKI_Outcome/declare_aki_outcome_overall.csv"
    FILENAME_SurgDay_labs = "Labs/cleaned_data/lab_features_on_surgDay.csv"
    FILENAME_PF = "Time_series/Clean_data/PF-SF_final.csv"
    # write:
    FILENAME_all_features = "Model/Data/features_to_model_withPF_moreClean.csv"
    
    ### For Windows:
#    FILENAME_intraop    = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model/old_codesAndData/Input_Data_For_Gam.csv"
#    FILENAME_meds       = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Meds/Cleaned_data/meds_features.csv"
#    FILENAME_labs       = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Labs/cleaned_data/lab_features.csv"
#    FILENAME_opTime     = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Labs/old_data/monitor_key_st_end.csv"
#    FILENAME_anes       = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/IntraOp/Raw_intraOp_data/monitor_st_end_anes_type.csv"
#    FILENAME_AKI3_outcomes = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/AKI_Outcome/declare_aki_outcome_in_3days.csv"
#    FILENAME_AKI7_outcomes = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/AKI_Outcome/declare_aki_outcome_in_7days.csv"
#    FILENAME_AKIall_outcomes = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/AKI_Outcome/declare_aki_outcome_overall.csv"
#    FILENAME_SurgDay_labs = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Labs/cleaned_data/lab_features_on_surgDay.csv"
#    FILENAME_PF = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/IntraOp/Clean_data/PF-SF_final.csv"
   
    #write filename:
    #FILENAME_all_features = "S:/2016_223 IDEALIST/ANALYTIC CORE/2 MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model/Data/features_to_model_withPF.csv"
        
    
    #Call following functions to merge different features:
    int_meds_df = combine_meds_with_AKI(FILENAME_intraop, FILENAME_meds)
    
    int_meds_labs_df = combine_labs_with_AKI(int_meds_df)
    
    int_meds_labs_df = combine_PF_ratio(int_meds_labs_df)
    
    int_meds_labs_df = add_surgery_time_feature(int_meds_labs_df)
    
    int_meds_labs_df = add_anesthesia_type(int_meds_labs_df)
    
    int_meds_labs_df = add_AKI_outcome(int_meds_labs_df)
    
    # drop any account with missing values. Such as missing outcome from AKI-phenotyping 
    int_meds_labs_df =  int_meds_labs_df.dropna(axis=0, how='any')
    
    #silentremove(FILENAME_all_features)
    int_meds_labs_df.to_csv(FILENAME_all_features, index=False)