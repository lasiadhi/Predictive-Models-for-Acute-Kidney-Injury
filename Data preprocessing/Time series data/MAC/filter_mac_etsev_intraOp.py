#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec 21 15:07:08 2017

@author: ladhikari

This code filter out MAC values those are out side of the surgery time

"""


import pandas as pd
import os, errno
import csv
from collections import defaultdict
from datetime import datetime



# this function will remove the the file with the argument name
def silentremove(filename):
    try:
        os.remove(filename)
    except OSError as e:
        if e.errno != errno.ENOENT:
            raise
            
            

# this fuction will add an additional column to the filtered lab file to indicate whether it is an intraOP or not 
def isIntraOp_lab():
    
    # create a dictionary for Acc_Noounts with surgery start and stop times
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
    silentremove(FILENAME_selected_TSYN)
                    
    # read the filtered labs file
    TS_df = pd.read_csv(FILENAME_TS, index_col=False, iterator=True, chunksize=20000)
    for df in TS_df:
        df['interOp_y_n'] = df.apply(lambda row: 'yes' if ((str(row['acc']) in dic) and (pd.to_datetime(row['time_stamp']) <= min(dic[str(row['acc'])])[1]) and (pd.to_datetime(row['time_stamp']) >= min(dic[str(row['acc'])])[0])) else 'no', axis=1)
        df.to_csv(FILENAME_selected_TSYN, mode='a',header=False, index=False)
    print ('Done isIntraOp!')
    
    
def readIntraOp_TS():
    selected_TS_df = pd.read_csv(FILENAME_selected_TSYN, names=['no','time_stamp',	'obser','acc','interOp_y_n'], index_col=False, iterator=True, chunksize=20000)
    writeHeader= True
    for df in selected_TS_df:
        df = df[df['interOp_y_n'] == 'yes']
        if writeHeader is True:
            df.to_csv(FILENAME_selected_TS, mode='a',header=True, index=False)
        else:
            df.to_csv(FILENAME_selected_TS, mode='a',header=False, index=False)
    print('Done filtering!')
    
# this is the main function that calls all the above functions
if __name__ == '__main__':
    
    os.chdir('/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith')

    #Read
    FILENAME_opTime = "IntraOp_model-DECLARE/Labs/old_data/monitor_key_st_end.csv"
    FILENAME_TS = "IntraOp_model-DECLARE/Time_series/Clean_data/etsev_cleaned_data.csv"

    #Write
    FILENAME_selected_TS = "etsev_onlyIntraOp.csv"
    FILENAME_selected_TSYN = "etsev_interOpYN.csv"

    
    isIntraOp_lab()
    readIntraOp_TS()
    
    