#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov  1 16:11:07 2017

@author: ladhikari

This script will compare preOp AKI predictions with the intraOp+preOp probs predictions 
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
    df.columns = ['acc', 'true_AKIOverall_outcome', 'pred_from_intraPreOp', 'pred_from_preOp']
    df.sort_values(by=['acc'], inplace=True)
    df.reset_index(drop=True, inplace=True)
    return df
    
def categorize(df, cut_off1_pre, cut_off2_pre, cut_off1_intra, cut_off2_intra):
#    df['categoty_intraOp'] = pd.cut(df['pred_from_intraOp'], labels=["Low", "Medium", "High"],bins=[0, cut_off1_intra, cut_off2_intra, 1])
#    df['categoty_preOp'] = pd.cut(df['pred_from_preOp'], labels=["Low", "Medium", "High"],bins=[0, cut_off1_pre, cut_off2_pre, 1])
    df['categoty_intraPreOp'] = pd.cut(df['pred_from_intraPreOp'], labels=["Low", "High"],bins=[0, cut_off1_intra, 1])
    df['categoty_preOp'] = pd.cut(df['pred_from_preOp'], labels=["Low", "High"],bins=[0, cut_off1_pre, 1])
    return df
    
def plotting(df):
    
    fig_dims = (10, 8)
    fig, ax = plt.subplots()
    fig.set_size_inches(fig_dims)
    
    sns.set(style="whitegrid")
    sns.set(color_codes=True)
    df = df[df['true_AKIOverall_outcome']==1]
    myPalette   = dict(Low = "#228b22", High = "#ff0000")
    g = sns.swarmplot(ax=ax, x="categoty_preOp", y="pred_from_intraPreOp", hue="categoty_intraPreOp", data=df, palette=myPalette, size=7)
    g.set(xlabel='Preoperative risk group',ylabel='Risk from intraOp+preOp model')
    leg = g.get_legend()
    leg.set_title('AKI risk groups')
    
    return fig
    ####fig.savefig('comapre_plot.png')
    #fig.savefig('comparison_plot_preOpVsintraPreOp_forNoAKI.svg', format='svg', dpi=300)
    
    #g.despine(left=True)
    #g.set_ylabels("survival probability")
    

# this is the main function that calls all the above functions
if __name__ == '__main__':
    
    os.chdir('S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith')
    #os.chdir('/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith')
    
    FILENAME_intraOp_results = "Model/Results/AKI_Overall/IntraOp+PreOp_probs_drop_esrd/akiOverall_prediction_from_intraOP+preOP_probs_from_test873.csv"
    FILENAME_preOp_results = "PreOp_model/Results/AKI_overall/esrd_dropped_final_features_model/akiOverall_preOp_trainBy2038_prediction_result_on873Test_esrd_dropped.csv"
    

    #write:
    FILENAME_all = "categorized_results_preOp+intraPreOp.csv"
    
    ## Define cut-offs for preOp model:
    cut1_pre = 0.5 #0.29
    cut2_pre = 0.0
    
    ## Define cut-offs for intraOp+preOp model:
    cut1_int = 0.444 #0.41
    cut2_int = 0.0
    
    df = generate_data()
    df = categorize(df, cut1_pre, cut2_pre, cut1_int, cut2_int)
    
    #df.to_csv(FILENAME_all, index=False)
    ff = plotting(df)
    #ff.savefig('comparison_plot_preOpVsintraPreOp_forAKI_akiOverall.svg', format='svg', dpi=300)
    
    
    
    