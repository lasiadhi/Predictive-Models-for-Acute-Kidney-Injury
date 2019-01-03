#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri May 11 09:38:22 2018

@author: ladhikari

Merge etsev and etiso for intraOp
"""


import pandas as pd
import os
import numpy as np

os.chdir('/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith/IntraOp_model-DECLARE/Time_series/Clean_data/IntraOp_clean_data')

FILENAME_etsev = "etsev_onlyIntraOp.csv"
FILENAME_etiso = "etiso_onlyIntraOp.csv"

df_etsev = pd.read_csv(FILENAME_etiso, usecols=['time_stamp','obser','acc'], encoding="latin-1")
df_etiso = pd.read_csv(FILENAME_etiso, usecols=['time_stamp','obser','acc'], encoding="latin-1")

df_etsev['time_stamp'] = pd.to_datetime(df_etsev['time_stamp'], errors = 'coerce')
df_etiso['time_stamp'] = pd.to_datetime(df_etiso['time_stamp'], errors = 'coerce')

# compute MAC:
df_etiso['obser'] = pd.to_numeric(df_etiso['obser'], errors='coerce')/1.17
df_etsev['obser'] = pd.to_numeric(df_etsev['obser'], errors='coerce')/1.8

df_etiso = df_etiso.rename(columns = {'obser':'etiso_obser'})
df_etsev = df_etsev.rename(columns = {'obser':'etsev_obser'})

#Combine etiso and etsev by account and timestamp:
df_MAC = df_etiso.merge(df_etsev, on=['acc','time_stamp'], how='outer')

# add etiso and etsev to compute MAC:
df_MAC['MAC'] = df_MAC['etiso_obser'] + df_MAC['etsev_obser']

#write to disk:
df_MAC[['acc', 'time_stamp','MAC']].to_csv('MAC_clean_intraOp.csv', index=False)
