


df_filtered <- readRDS(paste0(working_directory, '/DataProcessed/cleaned data.RDS'))
glm( mortstat~gender, data = df_filtered)%>%summary()
