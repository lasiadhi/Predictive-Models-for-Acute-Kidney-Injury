# Improved Predictive Models for Acute Kidney Injury with IDEAs: Intraoperative Data Embedded Analytics

Acute kidney injury (AKI) − an abrupt loss of kidney function − is a common and serious complication after a surgery which is associated with a high incidence of morbidity and mortality. The majority of existing perioperative AKI risk score prediction models are limited in their generalizability and do not fully utilize the physiological intraoperative time-series data. Thus, there is a need for intelligent, accurate, and robust systems, able to leverage information from large-scale data to predict patient’s risk of developing postoperative AKI. Here, we proposed an intelligent machine learning model that is able to improve patients’ postoperative AKI risk score by taking the intraoperative features into account. 


![Alt text](https://github.com/prisma-p/IDEAs-Algorithm/blob/master/Images/image1.png?raw=true "The conceptual diagram of the intraoperative data integrated AKI prediction model. This diagram shows the aggregation of data transformer, data engineering, and data analytics modules in preoperative and intraoperative layers. In particular, the two layers can be integrated in two different ways: (1) Stacked preoperative prediction scores with the cleaned and feature engineered intraoperative data, (2) obtain full perioperative dataset by merging all clean features from both layers.")

Figure 1. High level view of the MySurgeryRisk IDEA algorithm. All the available electronic health care data including vital signs from preoperative and intraoperative layers flow into the IDEA machine learning algorithm to preform acute kidney injury (AKI) predictions. 


Figure 3. Receiver-operating characteristic curves and performance metrics for (1) preoperative model, (2) postoperative stacked model, and (3) postoperative full model in predicting AKI-7day outcome from the validation cohort. 

