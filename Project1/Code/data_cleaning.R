
library(tidyverse)
library(table1) 
library(openxlsx)
n = 430
working_directory <- 'C:/Users/zhu-s/OneDrive/Graduate File/Course/Bios6624/BIOS6624-shuai/Project1'
df_hiv <- read.csv(paste0(working_directory, '/DataRaw/hiv_6624_final.csv'))%>%as_tibble()

### convert long format to wide format
df_hiv_wider <- df_hiv%>%
  select(newid, age, years, hard_drugs ,VLOAD, LEU3N, AGG_PHYS, AGG_MENT, BMI, EDUCBAS, ADH)%>%
  filter(years == 0|years == 2)%>%
  pivot_wider(names_from = years, names_glue = "{.value}_year{years}",
              values_from = c("VLOAD", "LEU3N", "AGG_PHYS",
                              "AGG_MENT",'age', 'BMI','EDUCBAS',
                              'hard_drugs','ADH'))%>% 
  ### set up adherence 
  mutate(adh = case_when(ADH_year2<=2~1,.default = 0))%>%
  
  select(-c(ADH_year2,ADH_year0))


### transform vload and leu3n 
df_hiv_lm_model <- df_hiv_wider%>%
  mutate(VLOAD_year0_log =log(VLOAD_year0),
         VLOAD_year2_log =log(VLOAD_year2),
         LEU3N_year0_log =log(LEU3N_year0),
         LEU3N_year2_log =log(LEU3N_year2))%>%
  ### categorize education variable
  mutate(edu = factor(EDUCBAS_year2, levels = 1:7, 
                    labels = c(rep('High school', 3),'some college', 'some college', 'Graduate, Post Graduate','Graduate, Post Graduate')))%>%
  ### categroize bmi variable
  mutate(BMI = case_when(BMI_year0 < 18.5 ~ "Underweight",
                         BMI_year0 > 18.5 & BMI_year0 < 25 ~ "Healthy",
                         BMI_year0 > 25 & BMI_year0 < 30 ~ "Overweight",
                         BMI_year0 > 30 ~ "Obsese",
                         BMI_year0<0|BMI_year0>200 ~NA
                         ))%>%
  select(-c('BMI_year0', 'BMI_year2', 'EDUCBAS_year2', 'hard_drugs_year2', 'age_year2', 'EDUCBAS_year0'))%>%
  rename(Age = 'age_year0', Hard_drugs = 'hard_drugs_year0', Edu = edu, Adh = adh)

### label variable name readable
label(df_hiv_lm_model$newid) <- "Participant ID"
label(df_hiv_lm_model$VLOAD_year0) <- "Viral Load at Baseline"
label(df_hiv_lm_model$VLOAD_year2) <- "Viral Load at Year 2"
label(df_hiv_lm_model$LEU3N_year0) <- "CD4+ T Cell Count at Baseline"
label(df_hiv_lm_model$LEU3N_year2) <- "CD4+ T Cell Count at Year 2"
label(df_hiv_lm_model$AGG_PHYS_year0) <- "Physical Quality of Life at Baseline"
label(df_hiv_lm_model$AGG_PHYS_year2) <- "Physical Quality of Life at Year 2"
label(df_hiv_lm_model$AGG_MENT_year0) <- "Mental Quality of Life at Baseline"
label(df_hiv_lm_model$AGG_MENT_year2) <- "Mental Quality of Life at Year 2"
label(df_hiv_lm_model$Age) <- "Age (years)"
label(df_hiv_lm_model$Adh) <- "Adherence Level"
label(df_hiv_lm_model$VLOAD_year0_log) <- "Log-transformed Viral Load at Baseline"
label(df_hiv_lm_model$VLOAD_year2_log) <- "Log-transformed Viral Load at Year 2"
label(df_hiv_lm_model$LEU3N_year0_log) <- "Log-transformed CD4+ T Cell Count at Baseline"
label(df_hiv_lm_model$LEU3N_year2_log) <- "Log-transformed CD4+ T Cell Count at Year 2"
label(df_hiv_lm_model$Edu) <- "Education Level"
label(df_hiv_lm_model$BMI) <- "Body Mass Index (kg/m²)"


###


df_hiv_lm_model %>%
  select(c("VLOAD_year0", "VLOAD_year2", "LEU3N_year0", "LEU3N_year2", 
           "AGG_PHYS_year0", "AGG_PHYS_year2", "AGG_MENT_year0", "AGG_MENT_year2", "Age","Adh", "Edu", "BMI"))%>%
  rename(
    Viral_Load_Baseline = VLOAD_year0,
    Viral_Load_Year2 = VLOAD_year2,
    CD4_Count_Baseline = LEU3N_year0,
    CD4_Count_Year2 = LEU3N_year2,
    Physical_Health_Baseline = AGG_PHYS_year0,
    Physical_Health_Year2 = AGG_PHYS_year2,
    Mental_Health_Baseline = AGG_MENT_year0,
    Mental_Health_Year2 = AGG_MENT_year2,
    Age = Age,
    Adherence = Adh,
    Education_Level = Edu,
    Body_Mass_Index = BMI
  )%>%
  summarise(across(everything(), ~mean(is.na(.)) * 100)) %>%
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "missing_percentage") %>%
  ggplot(aes(x = missing_percentage, y = reorder(variable, missing_percentage))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Percentage of Missing Values", y = "Variables") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +
  geom_text(aes(label = sprintf("%.1f%%", missing_percentage)), hjust = -0.1)
ggsave(paste0(working_directory, '/DataProcessed/missing visulization.png'))

### drop missing value
df_hiv_lm_model <- df_hiv_lm_model%>%drop_na()

### save cleaned data to excel file
write.xlsx(df_hiv_lm_model, paste0(working_directory, '/DataProcessed/cleaned data.xlsx'))




df_hiv_lm_model%>%mutate(Hard_drugs = case_when(Hard_drugs==1~'Hard drugs use',
                                                Hard_drugs==0~'No hard drugs use'))%>%
  table1(~VLOAD_year0 + VLOAD_year2 + LEU3N_year0 + LEU3N_year2 + AGG_PHYS_year0 + AGG_PHYS_year2 + AGG_MENT_year0 + AGG_MENT_year2+Age + BMI + Adh + Edu | Hard_drugs, 
       data = .,caption="Summary of outcomes and predictors stratified by hard drugs")
#%>%saveRDS(paste0(working_directory, '/Figure/Tableone.RDS'))


