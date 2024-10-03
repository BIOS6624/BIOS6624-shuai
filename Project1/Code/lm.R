library(openxlsx)



df_cleaned <- read.xlsx('BIOS6624-shuai/Project1/DataProcessed/cleaned data.xlsx')

df_hiv_lm_model <- df_cleaned

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

##
model.fit.vload <- lm(VLOAD_year2_log~hard_drugs_year0+EDUCBAS_year2+VLOAD_year0_log+adh,
                      data = df_hiv_lm_model)
summary(model.fit.vload)

model.fit.LEU3N <- lm(LEU3N_year2~hard_drugs_year0+LEU3N_year0,
                      data = df_hiv_lm_model)
summary(model.fit.LEU3N)

model.fit.AGG_PHYS <- lm(AGG_PHYS_year2~hard_drugs_year0+age_year2+AGG_PHYS_year0,
                         data = df_hiv_lm_model)
summary(model.fit.AGG_PHYS)

model.fit.AGG_MENT <- lm(AGG_MENT_year2~hard_drugs_year0+EDUCBAS_year2+AGG_MENT_year0,
                         data = df_hiv_lm_model)
summary(model.fit.AGG_MENT)




