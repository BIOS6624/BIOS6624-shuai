library(caret)
## read cleaned data

df_cleaned <- readRDS('DataProcessed/cleaned data.RDS')

## 


mims_variable <- c('TMIMS_mean', 'L5_MIMS_mean', 'RA_MIMS_mean')
risk_factors <- c("age_years_interview",  "diabetes", "CHF", "CHD", "BMI_cat" )
outcome <- c("mortstat ", "permth_exm")

## 

model_fomula <- c(paste0('permth_exm~',mims_variable),
 paste0('permth_exm~', paste0(mims_variable,'+', paste0(risk_factors, collapse = '+'))),
 paste0('permth_exm~s(',mims_variable,')'),
 paste0('permth_exm~',paste0('s(',mims_variable,')','+', paste0(risk_factors, collapse = '+')))
 )

train_control <- trainControl(method = "cv", number = 10)
train(model_fomula[1], 
               data = df_cleaned, 
               method = "gam", 
               trControl = train_control,
               weights = df_cleaned$mortstat, 
               family = cox.ph(), 
               method = "REML")

