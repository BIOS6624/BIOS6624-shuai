
library(tidyverse)
library(gtsummary)
library(table1) 
library(nlme)

df_hiv <- read.csv('Project1/DataRaw/hiv_6624_final.csv')%>%as_tibble()

df_hiv_reduced <- df_hiv%>%select(newid, years, hard_drugs ,VLOAD, LEU3N, AGG_PHYS, AGG_MENT)%>%drop_na(hard_drugs)
## spaghetti plot 
df_hiv_reduced%>%
  ggplot()+
  geom_line(aes(x = years,y= LEU3N, group =newid))+facet_wrap(~hard_drugs)
## tableone
table1(~ factor(hard_drugs)+VLOAD+LEU3N+AGG_PHYS+AGG_MENT|years, data = df_hiv_reduced)


## fit a model
df_hiv_reduced_model <- df_hiv_reduced
df_hiv_reduced_model$hard_drugs <- factor(df_hiv_reduced_model$hard_drugs)
model.fit <- gls(LEU3N~years+hard_drugs ,data = df_hiv_reduced_model%>%filter(years>=0),
                 correlation = corSymm(form = ~ 1 | newid),
                 na.action=na.exclude )
summary(model.fit)
