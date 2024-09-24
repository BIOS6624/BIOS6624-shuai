
library(tidyverse)
library(gtsummary)
library(table1) 

n = 430
df_hiv <- read.csv('BIOS6624-shuai/Project1/DataRaw/hiv_6624_final.csv')%>%as_tibble()


df_hiv_wider <- df_hiv%>%
  select(newid, age, years, hard_drugs ,VLOAD, LEU3N, AGG_PHYS, AGG_MENT, BMI, EDUCBAS, ADH)%>%
  filter(years == 0|years == 2)%>%
  pivot_wider(names_from = years, names_glue = "{.value}_year{years}",
              values_from = c("VLOAD", "LEU3N", "AGG_PHYS",
                              "AGG_MENT",'age', 'BMI','EDUCBAS',
                              'hard_drugs','ADH'))%>%
  mutate(adh = case_when(ADH_year2<=3~1,.default = F))%>%
  select(-c(ADH_year2,ADH_year0))
df_hiv_lm_model <- df_hiv_wider
## univarite model
df_hiv_lm_model$VLOAD_year0_log <- log(df_hiv_lm_model$VLOAD_year0)
df_hiv_lm_model$VLOAD_year2_log <- log(df_hiv_lm_model$VLOAD_year2)
df_hiv_lm_model%>%tbl_uvregression(
  method = lm,
  y = VLOAD_year2_log,
  include = c('hard_drugs_year2', 'BMI_year2', 'age_year2', 'EDUCBAS_year2', 'VLOAD_year0_log', 'adh')
)%>%modify_header(label = "**VLOAD Model**")

df_hiv_lm_model%>%tbl_uvregression(
  method = lm,
  y = LEU3N_year2,
  include = c('hard_drugs_year2', 'BMI_year2', 'age_year2', 'EDUCBAS_year2', 'LEU3N_year0', 'adh')
)%>%modify_header(label = "**LEU3N Model**")

df_hiv_lm_model%>%tbl_uvregression(
  method = lm,
  y = AGG_PHYS_year2,
  include = c('hard_drugs_year2', 'BMI_year2', 'age_year2', 'EDUCBAS_year2', 'AGG_PHYS_year0', 'adh')
)%>%modify_header(label = "**AGG_PHYS Model**")

df_hiv_lm_model%>%tbl_uvregression(
  method = lm,
  y = AGG_MENT_year2,
  include = c('hard_drugs_year2', 'BMI_year2', 'age_year2', 'EDUCBAS_year2', 'AGG_MENT_year0', 'adh')
)%>%modify_header(label = "**AGG_MENTModel**")




## fit a lm vload model

model.fit.vload <- lm(log(VLOAD_year2)~hard_drugs_year2+BMI_year2+age_year2+EDUCBAS_year2+log(VLOAD_year0),
                data = df_hiv_lm_model)
summary(model.fit.vload)

model.fit.LEU3N <- lm(LEU3N_year2~hard_drugs_year0+BMI_year2+age_year2+EDUCBAS_year2+LEU3N_year0,
                      data = df_hiv_lm_model)
summary(model.fit.LEU3N)
model.fit.AGG_PHYS <- lm(AGG_PHYS_year2~hard_drugs_year0+BMI_year2+age_year2+EDUCBAS_year2+AGG_PHYS_year0,
                      data = df_hiv_lm_model)
summary(model.fit.AGG_PHYS )
model.fit.AGG_MENT <- lm(AGG_MENT_year2~hard_drugs_year0+BMI_year2+age_year2+EDUCBAS_year2+AGG_MENT_year0,
                      data = df_hiv_lm_model)
summary(model.fit.AGG_MENT )





