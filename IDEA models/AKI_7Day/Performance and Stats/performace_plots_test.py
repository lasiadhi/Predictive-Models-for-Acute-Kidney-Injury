#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov  2 11:13:21 2017

@author: ladhikari

This code will produce threshold vs performce plots for intraOp results
"""

import pandas as pd
import os, errno
import csv
from collections import defaultdict
from datetime import datetime
import numpy as np
from functools import partial
import matplotlib.pyplot as plt



def plot_performance_measure(list_df,list_labels,cutoffs1,cutoffs2,x=10, y=5.7):
    
    font = {'family': 'serif',
        'color':  'black',
        'weight': 'normal',
        'size': 14,
        }
    
    font_cut = {'family': 'serif',
        'color':  'black',
        'weight': 'normal',
        'size': 12,
        }
    
    #n=len(list_labels)
    #fig,axes = plt.subplots(nrows=n/2, ncols=2, figsize=(x*(2.1), y*(n/2+.2)))
    fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(x,y))
    for df,l,c, c2 in zip(list_df,list_labels,cutoffs1, cutoffs2):
        ax.plot(df['thres'],df['acc'],'b',label='Accuracy')
        ax.plot(df['thres'],df['ppv'],'r',label='Positive Predictive Value (PPV)')
        ax.plot(df['thres'],df['npv'],'g',label='Negative Predictive Value (NPV)')
        ax.plot(df['thres'],df['yod_index'],'c',label='Youden Index')
        #ax.plot([c,c],[0,1],'k:',label='')
        #ax.plot([c2,c2],[0,1],'k:',label='')
        ax.grid()
        ax.legend(loc=8)
        ax.set_xlabel('Threshold',  fontdict=font)
        ax.set_ylabel('Performance',  fontdict=font)
        #ax.set_title(l,  fontdict=font)
        #ax.text(0.31, 0.4, 'Cutoff-1', fontdict=font_cut)
        #ax.text(0.75, 0.7, 'Cutoff-2', fontdict=font_cut)
        ax.grid(color='gray', linestyle='-', linewidth=0.3)
        fig.tight_layout()
    return fig
    

if __name__ == '__main__':
    
    #os.chdir('S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model')
    os.chdir('/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/Model')
    
    # read performace dataset:
    FILENAME_results = 'Results/AKI_7Day/IntraOp+PreOp_probs_drop_esrd/aki7Day_IntraOp+preOP_ROC_AUC_ACC_PPV_NPV_F1_TestData.csv'
    
    df_perf = pd.read_csv(FILENAME_results, index_col=False)
    
    list_df = [df_perf.iloc[0:96,]]
    list_labels = ['Preoprative predictive model performace for AKI-3day outcome']
    cutoffs1 = [0]
    cutoffs2 = [0]
    performance_fig = plot_performance_measure(list_df, list_labels, cutoffs1, cutoffs2)
    #performance_fig.show()
    #performance_fig.savefig('Performance_plot-preOp_AKI3Day.png')
    #performance_fig.savefig('aki7Day_IntraO+preOP_probs_Performance_873TestData.svg', format='svg', dpi=300)