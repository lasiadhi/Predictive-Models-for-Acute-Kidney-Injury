#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov  1 16:11:07 2017

@author: ladhikari

This script will compare preOp AKI predictions with the intraOp predictions 
"""

import pandas as pd
import os, errno
import csv
from collections import defaultdict
from datetime import datetime,time
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt

# combine intraOp predicted probabilities with the preOp predicted probs
def generate_data():
    
    # read datasets:
    df_intra  = pd.read_csv(FILENAME_intraOp_results, index_col=False)
    df_pre  = pd.read_csv(FILENAME_preOp_results, index_col=False)
    
    #merge data:
    df = pd.merge(df_intra, df_pre, how='inner', left_on='acc', right_on='data_test$acc')
    df = df[['acc', 'true_AKI_outcome', 'pred_from_intraOp', 'kdigo_corrpred']]
    df.columns = ['acc', 'true_AKI3_outcome', 'pred_from_intraOp', 'pred_from_preOp']
    df.sort_values(by=['acc'], inplace=True)
    df.reset_index(drop=True, inplace=True)
    return df
    
def categorize(df, cut_off1_pre, cut_off2_pre, cut_off1_intra, cut_off2_intra):
#    df['categoty_intraOp'] = pd.cut(df['pred_from_intraOp'], labels=["Low", "Medium", "High"],bins=[0, cut_off1_intra, cut_off2_intra, 1])
#    df['categoty_preOp'] = pd.cut(df['pred_from_preOp'], labels=["Low", "Medium", "High"],bins=[0, cut_off1_pre, cut_off2_pre, 1])
    df['categoty_intraOp'] = pd.cut(df['pred_from_intraOp'], labels=["Low", "High"],bins=[0, cut_off1_intra, 1])
    df['categoty_preOp'] = pd.cut(df['pred_from_preOp'], labels=["Low", "High"],bins=[0, cut_off1_pre, 1])
    return df
    
def plotting(df):
    
    fig_dims = (10, 8)
    fig, ax = plt.subplots()
    fig.set_size_inches(fig_dims)
    
    sns.set(style="whitegrid")
    sns.set(color_codes=True)
    #g = sns.factorplot(x="categoty_preOp", y="categoty_intraOp", hue="true_AKI3_outcome", data=df, size=6, kind="bar")
    df = df[df['true_AKI3_outcome']==1]
    g = sns.swarmplot(ax=ax, x="categoty_preOp", y="pred_from_intraOp", hue="categoty_intraOp", data=df, palette="deep", size=7)
    g.set(xlabel='Preoperative risk group',ylabel='IntraOperative risk for patients actual AKI after 3 Days')
    leg = g.get_legend()
    leg.set_title('IntraOp risk groups')
    
    ####fig.savefig('comapre_plot.png')
    #fig.savefig('comaprison_plot.svg', format='svg', dpi=300)
    
    #g.despine(left=True)
    #g.set_ylabels("survival probability")
    

# this is the main function that calls all the above functions
if __name__ == '__main__':
    
    FILENAME_intraOp_results = "/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model/Results/AKI_3Day/aki3Day_prediction_from_intraOP.csv"
    FILENAME_preOp_results = "/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/PreOp_model/Results/AKI_3/aki3Day_preOp_prediction_result.csv"
    
    #write:
    FILENAME_all = "/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/Analysis/Data/categorized_results.csv"
    
    ## Define cut-offs for preOp model:
    cut1_pre = 0.17
    cut2_pre = 0.43
    
    ## Define cut-offs for intraOp model:
    cut1_int = 0.42
    cut2_int = 0.4
    
    df = generate_data()
    df = categorize(df, cut1_pre, cut2_pre, cut1_int, cut2_int)
    
    #df.to_csv(FILENAME_all, index=False)
    plotting(df)
    
    
    
    