

## set the working directory
working_directory <- 'C:/Users/zhushu/OneDrive/Graduate File/Course/Bios6624/BIOS6624-shuai/Project2'
setwd(working_directory)
## read cleaned data

df_filtered <- readRDS(paste0(working_directory, '/DataProcessed/cleaned data.RDS'))
glm( mortstat~gender, data = df_filtered)%>%summary()
