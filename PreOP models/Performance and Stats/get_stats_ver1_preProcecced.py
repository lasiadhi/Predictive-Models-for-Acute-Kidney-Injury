#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov  6 16:34:46 2017

@author: ladhikari

This code generates basic descriptive statistics for preprocessed preOp dataset (only accounts in the intraOp final dataset).
Inputs: Dataset as a csv file, a dicionary that explains dataset features and data types
Output: 1) Stats for continous varaibales as a csv file. 2) Stats for categorical variables as a csv file
"""

import pandas as pd
import os

# create var to var full name dictionary
def get_dic(df_var, var1, var2):
    dic = dict(zip(df_var[var1], df_var[var2]))
    return dic

# compute median, 25th, and 75th percentiles for continous variables
def compute_median_25_75perc(df, isNormal):
    if isNormal:
        means = round(df.mean(),2)
        sds   = round(df.std(),2)
        df_all = pd.concat([means, sds], axis=1)
        ss = df_all.apply(lambda row: str(row[0])+' ('+str(row[1])+')', axis=1)
    else:
        medians = round(df.median(),2)
        per_25  = round(df.quantile(q=0.25),2)
        per_75  = round(df.quantile(q=0.75),2) 
        df_all = pd.concat([medians, per_25, per_75], axis=1)
        ss = df_all.apply(lambda row: str(row[0])+' ('+str(row[0.25])+','+str(row[0.75])+')', axis=1)
    return ss


# generate a table for continous variable stats
def generate_stats_for_continous(df_features, my_dic, outcome=None, isNormal=False):    
    # filter out keys in the dic with cat
    dic_with_num = {k:v for k,v in my_dic.items() if v == 'num'} # generate the dic
    if (outcome == None):
        df_new = df_features[list(dic_with_num.keys())]
        df_return = compute_median_25_75perc(df_new, isNormal)
    else:
        df_return = pd.DataFrame()
        for outcome_i in df_features[outcome].cat.categories:
            df = df_features[df_features[outcome]==outcome_i]
            df = df[list(dic_with_num.keys())]
            df_stat = compute_median_25_75perc(df, isNormal)
            df_stat.rename(outcome_i, inplace=True)
            df_return = pd.concat([df_return, df_stat], axis=1)
            
    return df_return

# compute stats for cat variables: count and percentages 
def compute_count_perc_NEW(dd, all_outcomes=[]):

    total = dd.iloc[:,0].count()
    df_all = pd.DataFrame(dd.iloc[:,0].value_counts().reset_index())
    df_all.loc[-1] = [dd.columns[0],total]
    df_all['perc'] = df_all.apply(lambda row: round(row[dd.columns[0]]/total*100,2), axis=1)
    df_all.sort_index(inplace=True)  # sort to get the col name as the first row
    df_all['count_perc_overall']=df_all.apply(lambda row: str(int(row[dd.columns[0]])) +' ('+ str(row['perc']) +' %)', axis=1)
    df_all = df_all.iloc[:,[0,3]]
    if (len(all_outcomes) != 0):
        for outcome_i in all_outcomes:
            df_i = dd[dd[dd.columns[1]]==outcome_i]
            total = df_i.iloc[:,0].count()
            df_i = pd.DataFrame(df_i.iloc[:,0].value_counts().reset_index())
            df_i.loc[-1] = [df_i.columns[1],total]
            df_i['perc'] = df_i.apply(lambda row: round(row[dd.columns[0]]/total*100,2), axis=1)
            df_i.sort_index(inplace=True)
            df_i['count_perc_'+ str(outcome_i)]=df_i.apply(lambda row: str(int(row[dd.columns[0]])) +' ('+ str(row['perc']) +' %)', axis=1)
            df_i = df_i.iloc[:,[0,3]]
            df_all = pd.merge(df_all, df_i, how='left', on='index')
    return df_all


# generate a stat tabale for catigorical variables
def generate_stats_for_cat_NEW(df_features, my_dic, outcome=None):
    dic_with_cat = {k:v for k,v in my_dic.items() if v == 'cat'} # generate the dic
    df_return = pd.DataFrame()
    if (outcome == None):
        df = df_features[list(dic_with_cat.keys())]
        df= df.apply(lambda x: x.astype('category'))
        for col in df.columns:
            df_col = compute_count_perc_NEW(pd.DataFrame(df[col]))
            df_return = pd.concat([df_return, df_col], axis=0)
    else:
        all_outcomes = df_features[outcome].cat.categories
        df = df_features[list(dic_with_cat.keys())]
        df = df.apply(lambda x: x.astype('category'))
        for col in df.columns:
            df_col = compute_count_perc_NEW(pd.concat([df[col], df_features[outcome]], axis=1), all_outcomes)
            df_return = pd.concat([df_return, df_col], axis=0)
        
    return df_return
    


if __name__ == '__main__':
    
    #os.chdir('/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith')  #for Ubuntu
    os.chdir('S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith')   # for Windows
    
    #FILENAME_intraOpFeatures = 'Model/Data/features_to_model_withPF.csv'   # Change the path to the dataset
        # load already splitted data with aki outcome
        
    FILENAME_data = "PreOp_model/Results/AKI_overall/esrd_dropped_final_features_model/akiOverall_PreOp_trainBy2038_predScore_for_wholeCohort_esrd_Dropped.csv"

    df_features = pd.read_csv(FILENAME_data, index_col=0)
    
    
    FILENAME_intraOp_var_names = 'PreOp_model/Data/preOp_feature_list_toGetStats_ver1.csv'     # Change the path to the feature dictionary
    
    df_var  = pd.read_csv(FILENAME_intraOp_var_names, index_col=False)     # read the feature dictionary
    #df_features  = pd.read_csv(FILENAME_intraOpFeatures, index_col=False)  # read the datset to dataframe
    df_features.outcome = df_features.outcome.astype('category', categories=[0,1])
    
    my_dic = get_dic(df_var, 'variable', 'preProcessed_variable_type')  # generate python dictionary based on our feature dictionary. Change the column headers as necessary. 
    
    
    ###################compute descriptives for continous variables ################################
    dd_all = generate_stats_for_continous(df_features, my_dic, outcome=None, isNormal=False)   # compute overall stats
    dd_all.rename('overall', inplace=True)
    dd_for_outcome = generate_stats_for_continous(df_features, my_dic, outcome='outcome', isNormal=False)  # compute descriptives for each outcome seperately. Change the outcome label name here. 
    df = pd.concat([dd_all, dd_for_outcome], axis=1)     #combine all results coloumn together. 
    
    # Run the following two lines to change the variable names to meaningfuln names
    my_dic_full_name = get_dic(df_var, 'variable', 'full_name')       # create a python dictionary
    df['full_var_name'] = df.index.to_series().map(my_dic_full_name)  # add column with acc
    df = df[['full_var_name', 'overall',0, 1]]  # uncomment and modify here to modify column order and header names
    
    # uncomment the following line to write stats for continous variables to the disk
    df.to_csv('preOp_preProcessed_stats_for_akiOverall_continous_vars_MEDIAN.csv', index=True)
    #df.to_csv('preOp_preProcessed_stats_for_AKIOverall_continous_vars_MEAN_SD.csv', index=True)
    
    ###################### compute descriptives for categorical variables ##########################
    
    df_cat = generate_stats_for_cat_NEW(df_features, my_dic, outcome='outcome')  # compute stats for each outcome category
    
    # uncomment the following line to write stats for cat variables to the disk
    df_cat.to_csv('preOp_preProcessed_stats_for_akiOverall_CAT_vars.csv', index=False)  
    
    