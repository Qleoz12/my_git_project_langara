Objective: Outline the purpose and the source of the data 

the objective of BrisT1D Blood Glucose Prediction Competition is to predict the blood glucose levels of individuals with type 1 diabetes using continuous glucose monitoring (CGM) data. The competition aims to develop models that can accurately forecast future glucose levels based on historical data, which can help in managing diabetes more effectively.

the data is from 12 people

check records of each people    



WH Questions: Identify the who, where and when
• How: Brief description of how the data set was drawn


Who
The dataset was collected from young adults in the UK with Type 1 Diabetes. Each participant used:
A Continuous Glucose Monitor (CGM)
An insulin pump
A smartwatch
A total of 16 participants were involved:
9 in the training set
15 in the testing set (no overlap with training participants)

Where
 United Kingdom, real-world monitoring of patients with Type 1 diabetes in natural, everyday settings.

When
The dataset includes:
Training data from the first three months of the study for 9 participants.
Test data from a later period for the remaining 15 participants.

How
Data from three devices (CGM, insulin pump, and smartwatch) were aggregated at 5-minute intervals.
Each row (sample) includes:
Historical data covering the previous six hours
The target value is blood glucose one hour in the future (bg+1:00)
Samples in the training set are chronological and overlapping, while test samples are non-overlapping and randomly ordered to avoid data leakage.

What
Each sample row includes:
Glucose readings (bg-*)
Insulin dosages (insulin-*)
Carbohydrate intake (carbs-*)
Activity data (hr-*, steps-*, cals-*, activity-*)
A target value (bg+1:00) to be predicted