#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Nov  6 16:34:46 2017

@author: ladhikari

This code generates basic descriptive statistics for the given dataset - AKI-Overall
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
def compute_count_perc(df_columns):
    ss = pd.DataFrame()
    for col in df_columns:
        df = pd.DataFrame(df_columns[col].value_counts().reset_index()) 
        cc = df_columns[col].count()
        df.loc[-1] = [col,cc]
        df['perc'] = df.apply(lambda row: round(row[col]/cc*100,2), axis=1)
        df.sort_index()
        df.set_index('index', inplace=True)
        df_temp = df.apply(lambda row: str(int(row[col])) +' ('+ str(row['perc']) +' %)', axis=1)
        ss = pd.concat([ss, df_temp])
    return ss

# generate a stat tabale for catigorical variables
def generate_stats_for_cat(df_features, my_dic, outcome=None):
    dic_with_cat = {k:v for k,v in my_dic.items() if v == 'cat'} # generate the dic
    if (outcome == None):
        df_new = df_features[list(dic_with_cat.keys())]
        df_new = df_new.apply(lambda x: x.astype('category'))
        df_return = compute_count_perc(df_new)
        #df_return.rename('all', inplace=True)
    else:
        df_return = pd.DataFrame()
        for outcome_i in df_features[outcome].cat.categories:
            df = df_features[df_features[outcome]==outcome_i]
            df = df[list(dic_with_cat.keys())]
            df = df.apply(lambda x: x.astype('category'))
            df = compute_count_perc(df)
            df.columns = [outcome_i]
            df_return = pd.concat([df_return, df], axis=1)
        
    return df_return
    


if __name__ == '__main__':
    
    os.chdir('/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/IntraOp_model-DECLARE/')  #for Ubuntu
    #os.chdir('S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith')   # for Windows
    
    #FILENAME_intraOpFeatures = 'Model/Data/features_to_model_withPF.csv'   # Change the path to the dataset
        # load already splitted data with aki outcome
    FILENAME_X_train = "Model/Data/akiOverall_drop_esrd1_patients/akiOverall_X_train.csv"
    FILENAME_X_test = "Model/Data/akiOverall_drop_esrd1_patients/akiOverall_X_test.csv"
    FILENAME_y_train = "Model/Data/akiOverall_drop_esrd1_patients/akiOverall_y_train.csv"
    FILENAME_y_test = "Model/Data/akiOverall_drop_esrd1_patients/akiOverall_y_test.csv"
    X_train = pd.read_csv(FILENAME_X_train, index_col=0)
    X_test = pd.read_csv(FILENAME_X_test, index_col=0)
    y_train = pd.read_csv(FILENAME_y_train, index_col=0).iloc[:,0]
    y_test = pd.read_csv(FILENAME_y_test, index_col=0).iloc[:,0]
    
    # merge all
    X_df = pd.concat([X_train, X_test])
    y_df = pd.concat([y_train, y_test])
    df_features = X_df.join(y_df)
    
    
    FILENAME_intraOp_var_names = 'Model/Data/all_intraop_features.csv'     # Change the path to the feature dictionary
    
    df_var  = pd.read_csv(FILENAME_intraOp_var_names, index_col=False)     # read the feature dictionary
    #df_features  = pd.read_csv(FILENAME_intraOpFeatures, index_col=False)  # read the datset to dataframe
    df_features.aki_overall = df_features.aki_overall.astype('category', categories=[0,1])
    
    my_dic = get_dic(df_var, 'variable', 'variable_type')  # generate python dictionary based on our feature dictionary. Change the column headers as necessary. 
    my_dic.pop('PF_ratio')
    my_dic.update({'PF-ratio':'num'})
    
    ###################compute descriptives for continous variables ################################
    dd_all = generate_stats_for_continous(df_features, my_dic, outcome=None, isNormal=True)   # compute overall stats
    dd_all.rename('overall', inplace=True)
    dd_for_outcome = generate_stats_for_continous(df_features, my_dic, outcome='aki_overall',isNormal=True)  # compute descriptives for each outcome seperately. Change the outcome label name here. 
    df = pd.concat([dd_all, dd_for_outcome], axis=1)     #combine all results coloumn together. 
    
    # Run the following two lines to change the variable names to meaningfuln names
    my_dic_full_name = get_dic(df_var, 'variable', 'full_name')       # create a python dictionary
    df['full_var_name'] = df.index.to_series().map(my_dic_full_name)  # add column with acc
    df = df[['full_var_name', 'overall',0, 1]]  # uncomment and modify here to modify column order and header names
    
    # uncomment the following line to write stats for continous variables to the disk
    #df.to_csv('intraOp_stats_for_continous_vars_akiOverall.csv', index=True)
    
    ###################### compute descriptives for categorical variables ##########################
    
    df_cat_all = generate_stats_for_cat(df_features, my_dic, outcome=None)  #compute overall stats
    df_cat = generate_stats_for_cat(df_features, my_dic, outcome='aki_overall')  # compute stats for each outcome category
    df_final = pd.concat([df_cat_all, df_cat], axis=1)   # combine all results to one dataframe
    df_final.columns = ['overall'] + df_cat.columns.tolist()
    
    # uncomment the following line to write stats for cat variables to the disk
    #df_final.to_csv('intraOp_stats_for_cat_vars_akiOverall.csv', index=True)  
    
    