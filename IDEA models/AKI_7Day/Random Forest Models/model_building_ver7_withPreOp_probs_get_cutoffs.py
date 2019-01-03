# -*- coding: utf-8 -*-
"""
Created on 10/18/2017

@author: ladhikari
Python 3.6.2

Model: IntraOp + PreOp prediction score - AKI 7

Create a model to predict AKI from DECLARE features (MAC, HR, MAP, Labs, Meds, others)
Return the model AUC and prediction scores
Here, We add proOp prediction probabilities as a features to our intraOp model
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import random
import os

random.seed(9001)
from sklearn.model_selection import train_test_split
#from sklearn.cross_validation import train_test_split
from sklearn import metrics
#from sklearn.cross_validation import cross_val_score
from sklearn.model_selection import cross_val_score
from sklearn.metrics import roc_curve, auc
from sklearn.ensemble import RandomForestClassifier
#from pygam import LogisticGAM
from sklearn.feature_selection import SelectFromModel

from sklearn.model_selection import StratifiedKFold
from sklearn.feature_selection import RFECV, SelectKBest, f_classif
from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn import preprocessing

import time  
from sklearn.utils import resample   # for upsample the minor class
#from boruta import BorutaPy    # pip install https://github.com/danielhomola/boruta_py/tarball/0.1.5   or pip install Boruta-0.1.5.tar.gz  after downloading


# A recursive feature elimination with automatic tuning of the 
# number of features selected with cross-validation.
# http://scikit-learn.org/stable/auto_examples/feature_selection/plot_rfe_with_cross_validation.html
def RFE_with_CV():
    # Create the RFE object and compute a cross-validated score.
    #gam = LogisticGAM(spline_order=5)
    clf = RandomForestClassifier(n_estimators=1000, n_jobs=-1, min_samples_leaf=8, random_state=0)
    # The "accuracy" scoring is proportional to the number of correct
    # classifications
    rfecv = RFECV(estimator=clf, step=1, cv=StratifiedKFold(2), scoring='roc_auc')
    rfecv.fit(X_train, y_train)

    print("Optimal number of features : %d" % rfecv.n_features_)
    
    # Plot number of features VS. cross-validation scores
    plt.figure()
    plt.xlabel("Number of features selected")
    plt.ylabel("Cross validation score (nb of correct classifications)")
    plt.plot(range(1, len(rfecv.grid_scores_) + 1), rfecv.grid_scores_)
    plt.show()
    
    #### to get new X_train after RFE:
    # XX = X_train.iloc[:, rfecv.get_support(indices=True)]




def fitGAMClassifier(X_train, X_test):
    
    print ('Fit logistic GAM model: ')
    
    print ('Dimension of the X-train and X_test datasets:')
    print (X_train.shape)
    print (X_test.shape)
    
    gam = LogisticGAM(spline_order=5)
    gam.fit(X_train, y_train)
    predicted_y = gam.predict(X_test)
    probs = gam.predict_proba(X_test)
    
    # generate some evaluation metrics
    print ('Accuracy of the model: ', metrics.accuracy_score(y_test, predicted_y))
    # The accuracy of the logistic model is 0.609133126935.
    print (metrics.classification_report(y_test, predicted_y))
    
    cm = metrics.confusion_matrix(y_test, predicted_y)
    show_confusion_matrix(cm, ['class 0', 'class 1'])
    
    # 10-fold cross validation
    print ('CV for GAM: \n')
    scores = cross_val_score(LogisticGAM(spline_order=5), X, y, scoring='accuracy', cv=10)
    print ('Accuracy of 10 fold CV for GAM model:', scores)
    print ('Mean accuracy of the GAM model:', scores.mean())
    
    return probs



def fitBorutaRF():   #Boruta documentation: https://pypi.python.org/pypi/Boruta/0.1.5
    print ('Feature selection from Boruta RandomForestClassifier: ')
    
    rf = RandomForestClassifier(n_jobs=-1, random_state=0, max_depth=5, class_weight='balanced')   
    
    # define Boruta feature selection method
    feat_selector = BorutaPy(rf, n_estimators='auto', verbose=0, random_state=0)
    
    # find all relevant features 
    feat_selector.fit(X_train.values, y_train.values)
    
    # check selected features: feat_selector.support_
    X_important_train = X_train.iloc[:, feat_selector.support_]
    X_important_test = X_test.iloc[:, feat_selector.support_]
    
    print ("Boruta selected features for the model: ", list(X_important_train.columns))
    # check ranking of features: feat_selector.ranking_
    
    return X_important_train, X_important_test
    
    


def fitRandomForestClassifier_Boruta():
    
    print ('Parameter tunning and feature selection for RandomForestClassifier: ')
    
    #kbest = SelectKBest(f_classif)  # ANOVA F-value between label/feature for classification tasks (F-test captures only linear dependency)
    clf = RandomForestClassifier(n_jobs=-1, random_state=0)   #-1 sets #jobs to the #cores
    
    pipeline = Pipeline([('rf', clf)])
    
    # first run:
#    n_est               = [1300, 1500, 1700, 2000, 2200]
#    min_sample_leaf     = [5, 6, 8, 9, 10, 15]
#    max_features        = ['auto', 'log2', 15, 20, 25, 30, 35]

    n_est               = [2000]
    min_sample_leaf     = [15]
    max_features        = [25]   # between (0, n_features)
    
    param_grid = { 
    'rf__n_estimators': n_est,              # must have two underscores to split rf and the parameter variable
    'rf__min_samples_leaf': min_sample_leaf,
    'rf__max_features' : max_features
    }
    
    # Select imporatant features from Borura RF:
    X_train_impo, X_test_impo = fitBorutaRF()
  
    CV_rfc = GridSearchCV(pipeline, param_grid=param_grid, scoring='roc_auc', cv= 5)
    CV_rfc.fit(X_train_impo, y_train)
    print (CV_rfc.best_params_)   # print the best parameters
    
    # perform preditction using the best RF model
    predicted_y = CV_rfc.predict(X_test_impo)
    probs       = CV_rfc.predict_proba(X_test_impo)

    # generate some evaluation metrics
    print ('Accuracy of the RF model with selected features: ', metrics.accuracy_score(y_test, predicted_y))
    print ('ROC-AUC of the RF model with selected features: ',  metrics.roc_auc_score(y_test, probs[:,1]))   
    
    cm = metrics.confusion_matrix(y_test, predicted_y)
    show_confusion_matrix(cm, ['No AKI', 'AKI'])
    
    # pick important features to other models
    #selector = SelectKBest(f_classif, k= CV_rfc.best_params_['kbest__k']).fit(X_train, y_train)
    #X_important_train = selector.transform(X_train)
    #X_important_test = selector.transform(X_test)
    
#    finalFeatureIndices = CV_rfc.best_estimator_.named_steps['kbest'].get_support(indices=True)
#    X_important_train = X_train.iloc[:, finalFeatureIndices]
#    X_important_test = X_test.iloc[:, finalFeatureIndices]
    
    
#    print ("Selected features for the model: ", list(X_important_train.columns))
    return probs[:,1],X_train_impo,X_test_impo, CV_rfc











def fitRandomForestClassifier(X_train, X_test, y_train, y_test):
    
    print ('Parameter tunning and feature selection for RandomForestClassifier: ')
    
    kbest = SelectKBest(f_classif)  # ANOVA F-value between label/feature for classification tasks (F-test captures only linear dependency)
    
    # Create a RF classifier:
    #-1 sets #jobs to the #cores
    #The “balanced” mode uses the values of y to automatically adjust weights inversely proportional to class frequencies in the input data
    
    clf = RandomForestClassifier(n_jobs=-1, random_state=0, class_weight='balanced')   
    
    pipeline = Pipeline([('kbest', kbest), ('rf', clf)])
    
    # first run:
#    n_est               = [1300, 1500, 1700, 2000]
#    min_sample_leaf     = [8, 9, 10]
#    kbest_vals          = [80, 100, 150, 180, 200, 'all']
#    max_features        = ['auto', 'log2', 15, 20, 25, 30]
    
#    n_est               = [2000]
#    min_sample_leaf     = [11,12,13]
#    kbest_vals          = [110,120]
#    max_features        = [35,40,45]   # (0, n_features)
    
    n_est               = [1700]
    min_sample_leaf     = [9]
    kbest_vals          = [110]
    max_features        = [40]   # (0, n_features)
    
    param_grid = { 
    'rf__n_estimators': n_est,              # must have two underscores to split rf and the parameter variable
    'rf__min_samples_leaf': min_sample_leaf,
    'kbest__k' : kbest_vals,
    'rf__max_features': max_features
    }
    
    
  
    CV_rfc = GridSearchCV(pipeline, param_grid=param_grid, scoring='roc_auc', cv= 5)
    CV_rfc.fit(X_train, y_train)
    print (CV_rfc.best_params_)   # print the best parameters
    
    # perform preditction using the best RF model
    predicted_y = CV_rfc.predict(X_train)
    probs       = CV_rfc.predict_proba(X_train)

    # generate some evaluation metrics
    print ('Accuracy of the RF model with selected features: ', metrics.accuracy_score(y_train, predicted_y))
    print ('ROC-AUC of the RF model with selected features: ',  metrics.roc_auc_score(y_train, probs[:,1]))   
    
    cm = metrics.confusion_matrix(y_train, predicted_y)
    show_confusion_matrix(cm, ['No AKI', 'AKI'])
    
    # pick important features to other models
    #selector = SelectKBest(f_classif, k= CV_rfc.best_params_['kbest__k']).fit(X_train, y_train)
    #X_important_train = selector.transform(X_train)
    #X_important_test = selector.transform(X_test)
    
    finalFeatureIndices = CV_rfc.best_estimator_.named_steps['kbest'].get_support(indices=True)
    X_important_train = X_train.iloc[:, finalFeatureIndices]
    X_important_test = X_test.iloc[:, finalFeatureIndices]
    
    
    print ("Selected features for the model: ", list(X_important_train.columns))
    return probs[:,1],X_important_train,X_important_test, CV_rfc
    





def prelimAnalysis(X,y):

    #df.shape   (4305, 102)
    
    #print df.describe()
    
    
    # check for any NAN values in the dataset
    # print df.isnull().sum().sum()   --> There are no NAN values in the dataset
    
    # print df.dtypes
    
    # drop rows with outcome=5
    

    #print df.shape   # (4305, 102)
    
    # threshold the outcome column since outcomes are > 1 (e.g. 5)
    #df[df.outcome>=1] = 1
    
    ## Data exploration
    #print df.groupby(df.outcome).mean()
    #print df.groupby(df.outcome).median()
    ## There is no significant difference between mean values from 0 or 1 outcome 
    
    y.hist()
    
    # Test correlation
    fig = plt.figure()
    ax = fig.add_subplot(X.columns.size)
    cax = ax.matshow(X.corr(), vmin=-1, vmax=1)
    fig.colorbar(cax)
    plt.show()


def show_confusion_matrix(C,class_labels=['0','1']):
    """
    C: ndarray, shape (2,2) as given by scikit-learn confusion_matrix function
    class_labels: list of strings, default simply labels 0 and 1.

    Draws confusion matrix with associated metrics.
    """
    
    assert C.shape == (2,2), "Confusion matrix should be from binary classification only."
    
    # true negative, false positive, etc...
    tn = C[0,0]; fp = C[0,1]; fn = C[1,0]; tp = C[1,1];

    NP = fn+tp # Num positive examples
    NN = tn+fp # Num negative examples
    N  = NP+NN

    fig = plt.figure(figsize=(8,8))
    ax  = fig.add_subplot(111)
    ax.imshow(C, interpolation='nearest', cmap=plt.cm.gray)

    # Draw the grid boxes
    ax.set_xlim(-0.5,2.5)
    ax.set_ylim(2.5,-0.5)
    ax.plot([-0.5,2.5],[0.5,0.5], '-k', lw=2)
    ax.plot([-0.5,2.5],[1.5,1.5], '-k', lw=2)
    ax.plot([0.5,0.5],[-0.5,2.5], '-k', lw=2)
    ax.plot([1.5,1.5],[-0.5,2.5], '-k', lw=2)

    # Set xlabels
    ax.set_xlabel('Predicted Label', fontsize=16)
    ax.set_xticks([0,1,2])
    ax.set_xticklabels(class_labels + [''])
    ax.xaxis.set_label_position('top')
    ax.xaxis.tick_top()
    # These coordinate might require some tinkering. Ditto for y, below.
    ax.xaxis.set_label_coords(0.34,1.06)

    # Set ylabels
    ax.set_ylabel('True Label', fontsize=16, rotation=90)
    ax.set_yticklabels(class_labels + [''],rotation=90)
    ax.set_yticks([0,1,2])
    ax.yaxis.set_label_coords(-0.09,0.65)


    # Fill in initial metrics: tp, tn, etc...
    ax.text(0,0,
            'True Neg: %d\n(Num Neg: %d)'%(tn,NN),
            va='center',
            ha='center',
            bbox=dict(fc='w',boxstyle='round,pad=1'))

    ax.text(0,1,
            'False Neg: %d'%fn,
            va='center',
            ha='center',
            bbox=dict(fc='w',boxstyle='round,pad=1'))

    ax.text(1,0,
            'False Pos: %d'%fp,
            va='center',
            ha='center',
            bbox=dict(fc='w',boxstyle='round,pad=1'))


    ax.text(1,1,
            'True Pos: %d\n(Num Pos: %d)'%(tp,NP),
            va='center',
            ha='center',
            bbox=dict(fc='w',boxstyle='round,pad=1'))

    # Fill in secondary metrics: accuracy, true pos rate, etc...
    ax.text(2,0,
            'False Pos Rate: %.2f'%(fp / (fp+tn+0.)),
            va='center',
            ha='center',
            bbox=dict(fc='w',boxstyle='round,pad=1'))

    ax.text(2,1,
            'True Pos Rate: %.2f'%(tp / (tp+fn+0.)),
            va='center',
            ha='center',
            bbox=dict(fc='w',boxstyle='round,pad=1'))

    ax.text(2,2,
            'Accuracy: %.2f'%((tp+tn+0.)/N),
            va='center',
            ha='center',
            bbox=dict(fc='w',boxstyle='round,pad=1'))

    ax.text(0,2,
            'Neg Pre Val: %.2f'%(1-fn/(fn+tn+0.)),
            va='center',
            ha='center',
            bbox=dict(fc='w',boxstyle='round,pad=1'))

    ax.text(1,2,
            'Pos Pred Val: %.2f'%(tp/(tp+fp+0.)),
            va='center',
            ha='center',
            bbox=dict(fc='w',boxstyle='round,pad=1'))


    plt.tight_layout()
    plt.show()


# Univariate feature selections


if __name__ == '__main__':
    
    # start time:
    t0 = time.time()
    
    #os.chdir('/run/user/2209058/gvfs/smb-share:server=ahcdfs.ahc.ufl.edu,share=files/dom/SHARE/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith')
    os.chdir('S:/2016_223 IDEALIST/ANALYTIC CORE/MySurgeryRisk PostOP V1.0/3 Users/Lasith')
    
#    FILENAME_all_features = "Model/Data/features_to_model_withPF_moreClean.csv"
#    FILENAME_preds_from_preOp = "PreOp_model/Results/AKI_3/esrd_dropped_final_features_model/aki3Day_PreOp_trainBy2038_predScore_for_wholeCohort_esrd_Dropped.csv"
#
#
#    # read the feature file:
#    df = pd.read_csv(FILENAME_all_features, index_col=0)
#    df = df[df.index!=596949594]  # drop bec esrd_corr is 1
#    df = df[df.index!=602470999]
#    
#    
#    df_preOp_preds = pd.read_csv(FILENAME_preds_from_preOp, usecols=['acc','aki_predScore'], index_col=1)
#    df_preOp_preds.columns = ['preOp_aki3_preds']
#    
#    #merge the preOp prediction score to df
#    df1 = df.join(df_preOp_preds, how='left')
#    
#    #change columns
#    df = df1[df1.columns.tolist()[0:230]+['preOp_aki3_preds'] + df1.columns.tolist()[230:233]]
#    
#    # exclude outcome == 5 from the dataset
#    #df = df[df.outcome!=5]   # reduce 80 rows
#    
#    # convert outcome to 1,0
#    df.aki3day = df.aki3day.astype('category', categories=[0,1]).cat.codes
#    df.aki7day = df.aki7day.astype('category', categories=[0,1]).cat.codes
#    df.aki_overall = df.aki_overall.astype('category', categories=[0,1]).cat.codes
#    df.diuretic = df.diuretic.astype('category', categories=[0,1]).cat.codes
#    df['pressors'] = df['pressors'].astype('category', categories=[0,1]).cat.codes
#    df['anes_type'] = df['anes_type'].astype('category', categories=['GEN','BL/RM']).cat.codes
#    df['isNight_surg'] = df['isNight_surg'].astype('category', categories=[0,1]).cat.codes
#    #df.iloc[:,192:214] = df.iloc[:,192:214].astype(np.int8)
#    
#    # drop anes_tryp col. If it is a constant
#    #df = df.drop('anes_type', axis=1)
#    
#    
#    
#    # split the dataset to train and test
#    X = df.iloc[:,0:len(df.columns)-3]  #assume last 3 cols are outcomes
#    
#    
##    ###################### Normalize different magnitude data #########################
##    # Create a minimum and maximum processor object
##    min_max_scaler = preprocessing.MinMaxScaler()
##
##    # Create an object to transform the data to fit minmax processor
##    X_scaled = min_max_scaler.fit_transform(X)
##
##    # Run the normalizer on the dataframe
##    df_normalized = pd.DataFrame(X_scaled)
##    df_normalized.columns = X.columns
##    df_normalized.index = X.index
##    X = df_normalized
##    
##    # re-convert dtypes
##    X.diuretic = X.diuretic.astype('category', categories=[0,1]).cat.codes
##    X['pressors'] = X['pressors'].astype('category', categories=[0,1]).cat.codes
##    X['anes_type'] = X['anes_type'].astype('category', categories=[0,1]).cat.codes
##    X['isNight_surg'] = X['isNight_surg'].astype('category', categories=[0,1]).cat.codes
##    
##    ############################### End of normalize ########################################
#    
#    
#    # set AKI-nday outcome as y
#    y = df.aki3day
#    
#    # preform preliminary data exploaration 
#    #prelimAnalysis(X,y)
#    
#    X_train, X_test, y_train, y_test = train_test_split(X,y, test_size=0.3, random_state=0)
    
#    ###################### Solution for imbalance dataset: Upsample minority class ###########
#    # seperate majority and minority classes from the joined X_train and y_train
#    X_y_train = X_train.join(y_train)
#    X_y_tr_majo = X_y_train[X_y_train.aki3day == 0]
#    X_y_tr_mino = X_y_train[X_y_train.aki3day == 1]
#    
#    # upsample minority class
#    X_y_mino_upsampled = resample(X_y_tr_mino, replace=True, n_samples=X_y_tr_majo.shape[0], random_state=0) 
#    X_y_upsampled = pd.concat([X_y_tr_majo, X_y_mino_upsampled]) # now we have 50% data from each class
#    y_train = X_y_upsampled.aki3day
#    X_train = X_y_upsampled.drop('aki3day', axis=1)
#    
#    ########################## End of upsampling ##########################################
    
    # write train and test acccount_outcome to disk
    #aki3Day_y_test = y_test.reset_index(level=0)
    #aki3Day_y_test.to_csv("aki3Day_y_test.csv",index=False)
    
    
    # load already splitted data with aki outcome
    FILENAME_X_train = "Model/Data/aki7_drop_esrd1_patients/aki7Day_X_train.csv"
    FILENAME_X_test = "Model/Data/aki7_drop_esrd1_patients/aki7Day_X_test.csv"
    FILENAME_y_train = "Model/Data/aki7_drop_esrd1_patients/aki7Day_y_train.csv"
    FILENAME_y_test = "Model/Data/aki7_drop_esrd1_patients/aki7Day_y_test.csv"
    X_train = pd.read_csv(FILENAME_X_train, index_col=0)
    X_test = pd.read_csv(FILENAME_X_test, index_col=0)
    y_train = pd.read_csv(FILENAME_y_train, index_col=0).iloc[:,0]
    y_test = pd.read_csv(FILENAME_y_test, index_col=0).iloc[:,0]
    
    
    # read preop prediction scores and merge 
    FILENAME_preds_from_preOp = "PreOp_model/Results/AKI_7/esrd_dropped_final_features_model/aki7Day_PreOp_trainBy2038_predScore_for_wholeCohort_esrd_Dropped.csv"
    df_preOp_preds = pd.read_csv(FILENAME_preds_from_preOp, usecols=['acc','aki_predScore'], index_col=1)
    df_preOp_preds.columns = ['preOp_aki7_preds']
    
    X_train = X_train.join(df_preOp_preds, how='left')  #merge the preOp prediction score to X_train
    X_test = X_test.join(df_preOp_preds, how='left')  #merge the preOp prediction score to y_train
    
    # check dump model accuracy (if we predict every test case as 1)
    dump_accuracy = (y_train.sum()/float(y_train.size))
    print ('Dump model accuracy (if we predict all test cases as 1): ', dump_accuracy)
    
    
    ## Random Forest Classifier model with CV
    pred_probs_RF, iXtrain, iXtest, CV_rfc = fitRandomForestClassifier(X_train, X_test, y_train, y_test)
    #pred_probs_RF, iXtrain, iXtest, CV_rfc = fitRandomForestClassifier_Boruta()
    
    #To read F-test scores- all_f = CV_rfc.best_estimator_.named_steps['kbest'].scores_
    # run two lines in the RF for X_important_train and finalFeatureIndices
    # dff = pd.DataFrame({'features':np.array(X_important_train.columns), 'f-value': all_f[finalFeatureIndices]})
    #dff.to_csv('important_features_f_value.csv',index=False)

    ## GAM calssifier with important features from RF
#    pred_probs_GAM = fitGAMClassifier(iXtrain, iXtest)
    

    
    # RF - draw ROC and compute the AUC 
    fpr, tpr, thresholds = roc_curve(y_train, pred_probs_RF)
    roc_auc = auc(fpr, tpr)
    plt.plot(fpr, tpr, lw=1, color='red', label='RandomForest (%0.2f)'% roc_auc)
    
#    # GAM - draw ROC and compute the AUC 
#    fpr, tpr, thresholds = roc_curve(y_test, pred_probs_GAM)
#    roc_auc = auc(fpr, tpr)
#    plt.plot(fpr, tpr, lw=1, color='green', label='Logistic GAM (%0.2f)'% roc_auc)

    

    # plotting ROCs
    plt.plot([0,1], [0,1], color='gray', lw=1, linestyle='--')
    plt.ylabel('True positive rate')
    plt.xlabel('False positive rate')
    plt.title('ROC Curves with AUCs')
    plt.legend(loc='center left', bbox_to_anchor=(1,0.5))
    plt.show()
    
    # end timing:
    t1 = time.time()
    total_time = t1 - t0
    print('Total computational time for training and testing %2.2f' %total_time)
    
    ################ Write results to the disk ##################
    df_pred_scores = pd.DataFrame(pred_probs_RF)
    df_pred_scores.columns = ['pred_from_intraPreOp']
    df_y_train = pd.DataFrame(y_train)
    df_y_train.columns = ['true_AKI_outcome']
    df_y_train = df_y_train.reset_index(level=0)
    df_results = pd.concat([df_y_train, df_pred_scores],axis=1)
    df_results.to_csv("aki7Day_prediction_from_intraOP+preOP_probs_from_train2038.csv",index=False)
