# Predicting bacterial sepsis using novel clinical and laboratory parameters in a prospective cohort of South Asian Indians

## Background 
Bacterial sepsis is one of the leading causes of morbidity and mortality. However there are no diagnostic tools that predict sepsis with reproducible results. We propose that a combination of clinical and laboratory parameters along with a novel biomarker (plasma NGAL) may be useful.      

## Method: 
A prospective cohort study was done at tertiary care centre from south India (June 2017 - April 2018, n = 100). Standard clinical and laboratory parameters were collected at the time of admission, and patients were followed up for the duration of their hospital stay. Patients were identified as bacterial sepsis based on blood culture drawn at admission or diagnosed by independent reviewers. Clinically relevant cut-offs for plasma NGAL were identified using the Received Operator Characteristic curve. Two machine learning models (GLMNET and Random forests) were used with significant predictors from the bivariate analysis (Poisson regression with robust standard errors) to predict sepsis.     

## Results
Of the analytic sample, 37 had bacterial sepsis. The optimal cut-off for plasma NGAL to predict sepsis was 570 ng/ml (sensitivity = 0.87, specificity = 0.46, AUC = 0.69). At that cut-off, dichotomizing plasma NGAL resulted in a crude risk ratio (RR) of 2.91 (95% CI: 1.34, 6.30). Thirteen predictors were identified from the bivariate analysis (RR; 95% CI) including diabetes mellitus [2.03; 1.2-3.42], a clear focus of infections [3.5; 2.13-5.75], SIRS >3 [1.94; 1.19-3.18], qSOFA > 2 [2.3; 1.37-3.87] and creatinine >2 mg/dl [1.84; 1.13-3.00]. The AUC for GLMNET and Random Forests (RF) were similar in training (GLMNET: 0.81, RF: 0.83) and test (GLMNET: 0.77, RF: 0.76) data.     

## Conclusion
The plasma NGAL cut off value of 570 ng/ml used on our study had a high sensitivity of 87%. The machine learning models had modest AUC but a combination of predictors identified may be useful to predict bacterial sepsis.     


