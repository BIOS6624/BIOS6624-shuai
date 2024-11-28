library(caret)
library(mgcv)
library(survival)
## read cleaned data
set.seed(123)
df_cleaned <- readRDS('DataProcessed/cleaned data.RDS')

## outcome and predictors
mims_variable <- c('TMIMS_mean', 'L5_MIMS_mean', 'RA_MIMS_mean')
risk_factors <- c("age_years_interview",  "diabetes", "CHF", "CHD", "BMI_cat" )
outcome <- c("mortstat ", "permth_exm")

## define models 

model_fomula <- c(paste0('permth_exm~',mims_variable),
 paste0('permth_exm~', paste0(mims_variable,'+', paste0(risk_factors, collapse = '+'))),
 paste0('permth_exm~s(',mims_variable,')'),
 paste0('permth_exm~',paste0('s(',mims_variable,')','+', paste0(risk_factors, collapse = '+')))
 )
## divide data to male and female 
df_male <- df_cleaned%>%filter(gender =='Male')
df_female <- df_cleaned%>%filter(gender =='Female')
folds.male <- createFolds(df_male$mortstat, k = 10, list = TRUE, returnTrain = TRUE)
folds.female <- createFolds(df_female$mortstat, k = 10, list = TRUE, returnTrain = TRUE)

cross_validation_gam <- function(folds, df_cleaned, fomula){
  concordance <- c()
  for(i in 1:10){
    train <- df_cleaned[folds[[i]],]
    test <- df_cleaned[-folds[[i]],]

    model <- gam(as.formula(fomula), data = train, family = cox.ph(), weights = as.integer(mortstat))
    pred <- predict(model, newdata = test)
    concordance <- c(concordance, concordancefit(Surv(test$permth_exm, as.integer(test$mortstat)), x=pred, reverse=TRUE)$concordance )

  }
  return(mean(concordance))
}


model_concordance_male <-  sapply(model_fomula, function(x) cross_validation_gam(folds.male, df_male, x))
model_concordance_female <-  sapply(model_fomula, function(x) cross_validation_gam(folds.female, df_female, x))
model_concordance <- c('Uni TMIMS', 'Uni L5 MIMS', 'Uni RA TMIMS', 
                       'Multi TMIMS', 'Multi L5 MIMS', 'Multi RA TMIMS',
                       'Uni Non linear TMIMS', 'Uni Non linear L5 MIMS', 'Uni Non linear RA TMIMS',
                       'Multi Non linear TMIMS', 'Multi Non linear L5 MIMS', 'Multi Non linear RA TMIMS')
names(model_concordance_male) <- model_concordance
names(model_concordance_female) <- model_concordance

model_concordance_male_sort <- sort(model_concordance_male, decreasing  = T)
model_concordance_female_sort <- sort(model_concordance_female, decreasing  = T)
round_3 <- function(x) round(x, digits = 3)
tibble(Rank = 1:12,
       'Male Model' = names(model_concordance_male_sort),
       'Male Concordance' = model_concordance_male_sort,
       'Female Model' = names(model_concordance_female_sort),
       'Female concordance' = model_concordance_female_sort)%>%
  mutate_if(is.numeric, round_3)%>%
  saveRDS('Figure/concordance table.RDS')


