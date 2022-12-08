n_countries <- length(countries)

#full_data_v1$'Medium_High_Tech_Subscriptions'[is.nan(full_data_v1$'Medium_High_Tech_Subscriptions')]<-NA

data_1960_total <- full_data_v1 %>% filter(Year == 1960)
data_2000_total <- full_data_v1 %>% filter(Year == 2000)
data_1980_total <- full_data_v1 %>% filter(Year == 1980)
data_1990_total <- full_data_v1 %>% filter(Year == 1990)
data_2000_total <- full_data_v1 %>% filter(Year == 2000)

######################### 1960

life_expectancy_matrix_1960 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mortality_rate_female_matrix_1960 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mortality_rate_male_matrix_1960 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
survival_rate_female_matrix_1960 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
survival_rate_male_matrix_1960 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
telephone_subscriptions_matrix_1960 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
electrical_power_matrix_1960 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mobile_subscriptions_matrix_1960 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
internet_subscriptions_matrix_1960 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mediumhightech_exports_matrix_1960 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
technicians_rd_matrix_1960 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))


for (r in 1:length(countries)) {
  
  for (c in 1:length(countries)) {
  
    life_expectancy_matrix_1960[r,c] <-  as.numeric(data_1960_total$'Life_Expectancy'[c]) -as.numeric(data_1960_total$'Life_Expectancy'[r])
    mortality_rate_female_matrix_1960[r,c] <-  as.numeric(data_1960_total$'Mortality_Rate_Female'[c]) -as.numeric(data_1960_total$'Mortality_Rate_Female'[r])
    mortality_rate_male_matrix_1960[r,c] <-  as.numeric(data_1960_total$'Mortality_Rate_Male'[c]) -as.numeric(data_1960_total$'Mortality_Rate_Male'[r])
    survival_rate_female_matrix_1960[r,c] <-  as.numeric(data_1960_total$'Survival_Rate_Female'[c]) -as.numeric(data_1960_total$'Survival_Rate_Female'[r])
    survival_rate_male_matrix_1960[r,c] <-  as.numeric(data_1960_total$'Survival_Rate_Male'[c]) -as.numeric(data_1960_total$'Survival_Rate_Male'[r])
    telephone_subscriptions_matrix_1960[r,c] <-  as.numeric(data_1960_total$'Telephone_Subscriptions'[c]) -as.numeric(data_1960_total$'Telephone_Subscriptions'[r])
    electrical_power_matrix_1960[r,c] <-  as.numeric(data_1960_total$'Electrical_Power'[c]) -as.numeric(data_1960_total$'Electrical_Power'[r])
    mobile_subscriptions_matrix_1960[r,c] <-  as.numeric(data_1960_total$'Mobile_Subscriptions'[c]) -as.numeric(data_1960_total$'Mobile_Subscriptions'[r])
    internet_subscriptions_matrix_1960[r,c] <-  as.numeric(data_1960_total$'Internet_Subscriptions'[c]) -as.numeric(data_1960_total$'Internet_Subscriptions'[r])
    mediumhightech_exports_matrix_1960[r,c] <-  as.numeric(data_1960_total$'Medium_High_Tech_Exports'[c]) -as.numeric(data_1960_total$'Medium_High_Tech_Exports'[r])
    technicians_rd_matrix_1960[r,c] <-  as.numeric(data_1960_total$'Technicians_RD'[c]) -as.numeric(data_1960_total$'Technicians_RD'[r])
  
  }
}

life_expectancy__df_1960 <- as.data.frame(life_expectancy_matrix_1960)
mortality_rate_female_df_1960 <- as.data.frame(mortality_rate_female_matrix_1960)
mortality_rate_male_df_1960 <- as.data.frame(mortality_rate_male_matrix_1960)
survival_rate_female_df_1960 <- as.data.frame(survival_rate_female_matrix_1960)
survival_rate_male_df_1960 <- as.data.frame(survival_rate_male_matrix_1960)
telephone_subscriptions_df_1960 <- as.data.frame(telephone_subscriptions_matrix_1960)
electrical_power_df_1960 <- as.data.frame(electrical_power_matrix_1960)
mobile_subscriptions_df_1960 <- as.data.frame(mobile_subscriptions_matrix_1960)
internet_subscriptions_df_1960 <- as.data.frame(internet_subscriptions_matrix_1960)
mediumhightech_exports_df_1960 <- as.data.frame(mediumhightech_exports_matrix_1960)
technicians_rd_df_1960 <- as.data.frame(technicians_rd_matrix_1960)

######################### 1970

life_expectancy_matrix_1970 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mortality_rate_female_matrix_1970 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mortality_rate_male_matrix_1970 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
survival_rate_female_matrix_1970 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
survival_rate_male_matrix_1970 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
telephone_subscriptions_matrix_1970 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
electrical_power_matrix_1970 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mobile_subscriptions_matrix_1970 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
internet_subscriptions_matrix_1970 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mediumhightech_exports_matrix_1970 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
technicians_rd_matrix_1970 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))


for (r in 1:length(countries)) {
  
  for (c in 1:length(countries)) {
    
    life_expectancy_matrix_1970[r,c] <-  as.numeric(data_1970_total$'Life_Expectancy'[c]) -as.numeric(data_1970_total$'Life_Expectancy'[r])
    mortality_rate_female_matrix_1970[r,c] <-  as.numeric(data_1970_total$'Mortality_Rate_Female'[c]) -as.numeric(data_1970_total$'Mortality_Rate_Female'[r])
    mortality_rate_male_matrix_1970[r,c] <-  as.numeric(data_1970_total$'Mortality_Rate_Male'[c]) -as.numeric(data_1970_total$'Mortality_Rate_Male'[r])
    survival_rate_female_matrix_1970[r,c] <-  as.numeric(data_1970_total$'Survival_Rate_Female'[c]) -as.numeric(data_1970_total$'Survival_Rate_Female'[r])
    survival_rate_male_matrix_1970[r,c] <-  as.numeric(data_1970_total$'Survival_Rate_Male'[c]) -as.numeric(data_1970_total$'Survival_Rate_Male'[r])
    telephone_subscriptions_matrix_1970[r,c] <-  as.numeric(data_1970_total$'Telephone_Subscriptions'[c]) -as.numeric(data_1970_total$'Telephone_Subscriptions'[r])
    electrical_power_matrix_1970[r,c] <-  as.numeric(data_1970_total$'Electrical_Power'[c]) -as.numeric(data_1970_total$'Electrical_Power'[r])
    mobile_subscriptions_matrix_1970[r,c] <-  as.numeric(data_1970_total$'Mobile_Subscriptions'[c]) -as.numeric(data_1970_total$'Mobile_Subscriptions'[r])
    internet_subscriptions_matrix_1970[r,c] <-  as.numeric(data_1970_total$'Internet_Subscriptions'[c]) -as.numeric(data_1970_total$'Internet_Subscriptions'[r])
    mediumhightech_exports_matrix_1970[r,c] <-  as.numeric(data_1970_total$'Medium_High_Tech_Exports'[c]) -as.numeric(data_1970_total$'Medium_High_Tech_Exports'[r])
    technicians_rd_matrix_1970[r,c] <-  as.numeric(data_1970_total$'Technicians_RD'[c]) -as.numeric(data_1970_total$'Technicians_RD'[r])
    
  }
}

life_expectancy__df_1970 <- as.data.frame(life_expectancy_matrix_1970)
mortality_rate_female_df_1970 <- as.data.frame(mortality_rate_female_matrix_1970)
mortality_rate_male_df_1970 <- as.data.frame(mortality_rate_male_matrix_1970)
survival_rate_female_df_1970 <- as.data.frame(survival_rate_female_matrix_1970)
survival_rate_male_df_1970 <- as.data.frame(survival_rate_male_matrix_1970)
telephone_subscriptions_df_1970 <- as.data.frame(telephone_subscriptions_matrix_1970)
electrical_power_df_1970 <- as.data.frame(electrical_power_matrix_1970)
mobile_subscriptions_df_1970 <- as.data.frame(mobile_subscriptions_matrix_1970)
internet_subscriptions_df_1970 <- as.data.frame(internet_subscriptions_matrix_1970)
mediumhightech_exports_df_1970 <- as.data.frame(mediumhightech_exports_matrix_1970)
technicians_rd_df_1970 <- as.data.frame(technicians_rd_matrix_1970)

######################### 1980

life_expectancy_matrix_1980 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mortality_rate_female_matrix_1980 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mortality_rate_male_matrix_1980 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
survival_rate_female_matrix_1980 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
survival_rate_male_matrix_1980 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
telephone_subscriptions_matrix_1980 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
electrical_power_matrix_1980 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mobile_subscriptions_matrix_1980 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
internet_subscriptions_matrix_1980 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mediumhightech_exports_matrix_1980 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
technicians_rd_matrix_1980 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))


for (r in 1:length(countries)) {
  
  for (c in 1:length(countries)) {
    
    life_expectancy_matrix_1980[r,c] <-  as.numeric(data_1980_total$'Life_Expectancy'[c]) -as.numeric(data_1980_total$'Life_Expectancy'[r])
    mortality_rate_female_matrix_1980[r,c] <-  as.numeric(data_1980_total$'Mortality_Rate_Female'[c]) -as.numeric(data_1980_total$'Mortality_Rate_Female'[r])
    mortality_rate_male_matrix_1980[r,c] <-  as.numeric(data_1980_total$'Mortality_Rate_Male'[c]) -as.numeric(data_1980_total$'Mortality_Rate_Male'[r])
    survival_rate_female_matrix_1980[r,c] <-  as.numeric(data_1980_total$'Survival_Rate_Female'[c]) -as.numeric(data_1980_total$'Survival_Rate_Female'[r])
    survival_rate_male_matrix_1980[r,c] <-  as.numeric(data_1980_total$'Survival_Rate_Male'[c]) -as.numeric(data_1980_total$'Survival_Rate_Male'[r])
    telephone_subscriptions_matrix_1980[r,c] <-  as.numeric(data_1980_total$'Telephone_Subscriptions'[c]) -as.numeric(data_1980_total$'Telephone_Subscriptions'[r])
    electrical_power_matrix_1980[r,c] <-  as.numeric(data_1980_total$'Electrical_Power'[c]) -as.numeric(data_1980_total$'Electrical_Power'[r])
    mobile_subscriptions_matrix_1980[r,c] <-  as.numeric(data_1980_total$'Mobile_Subscriptions'[c]) -as.numeric(data_1980_total$'Mobile_Subscriptions'[r])
    internet_subscriptions_matrix_1980[r,c] <-  as.numeric(data_1980_total$'Internet_Subscriptions'[c]) -as.numeric(data_1980_total$'Internet_Subscriptions'[r])
    mediumhightech_exports_matrix_1980[r,c] <-  as.numeric(data_1980_total$'Medium_High_Tech_Exports'[c]) -as.numeric(data_1980_total$'Medium_High_Tech_Exports'[r])
    technicians_rd_matrix_1980[r,c] <-  as.numeric(data_1980_total$'Technicians_RD'[c]) -as.numeric(data_1980_total$'Technicians_RD'[r])
    
  }
}

life_expectancy__df_1980 <- as.data.frame(life_expectancy_matrix_1980)
mortality_rate_female_df_1980 <- as.data.frame(mortality_rate_female_matrix_1980)
mortality_rate_male_df_1980 <- as.data.frame(mortality_rate_male_matrix_1980)
survival_rate_female_df_1980 <- as.data.frame(survival_rate_female_matrix_1980)
survival_rate_male_df_1980 <- as.data.frame(survival_rate_male_matrix_1980)
telephone_subscriptions_df_1980 <- as.data.frame(telephone_subscriptions_matrix_1980)
electrical_power_df_1980 <- as.data.frame(electrical_power_matrix_1980)
mobile_subscriptions_df_1980 <- as.data.frame(mobile_subscriptions_matrix_1980)
internet_subscriptions_df_1980 <- as.data.frame(internet_subscriptions_matrix_1980)
mediumhightech_exports_df_1980 <- as.data.frame(mediumhightech_exports_matrix_1980)
technicians_rd_df_1980 <- as.data.frame(technicians_rd_matrix_1980)

######################### 1990

life_expectancy_matrix_1990 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mortality_rate_female_matrix_1990 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mortality_rate_male_matrix_1990 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
survival_rate_female_matrix_1990 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
survival_rate_male_matrix_1990 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
telephone_subscriptions_matrix_1990 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
electrical_power_matrix_1990 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mobile_subscriptions_matrix_1990 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
internet_subscriptions_matrix_1990 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mediumhightech_exports_matrix_1990 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
technicians_rd_matrix_1990 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))


for (r in 1:length(countries)) {
  
  for (c in 1:length(countries)) {
    
    life_expectancy_matrix_1990[r,c] <-  as.numeric(data_1990_total$'Life_Expectancy'[c]) -as.numeric(data_1990_total$'Life_Expectancy'[r])
    mortality_rate_female_matrix_1990[r,c] <-  as.numeric(data_1990_total$'Mortality_Rate_Female'[c]) -as.numeric(data_1990_total$'Mortality_Rate_Female'[r])
    mortality_rate_male_matrix_1990[r,c] <-  as.numeric(data_1990_total$'Mortality_Rate_Male'[c]) -as.numeric(data_1990_total$'Mortality_Rate_Male'[r])
    survival_rate_female_matrix_1990[r,c] <-  as.numeric(data_1990_total$'Survival_Rate_Female'[c]) -as.numeric(data_1990_total$'Survival_Rate_Female'[r])
    survival_rate_male_matrix_1990[r,c] <-  as.numeric(data_1990_total$'Survival_Rate_Male'[c]) -as.numeric(data_1990_total$'Survival_Rate_Male'[r])
    telephone_subscriptions_matrix_1990[r,c] <-  as.numeric(data_1990_total$'Telephone_Subscriptions'[c]) -as.numeric(data_1990_total$'Telephone_Subscriptions'[r])
    electrical_power_matrix_1990[r,c] <-  as.numeric(data_1990_total$'Electrical_Power'[c]) -as.numeric(data_1990_total$'Electrical_Power'[r])
    mobile_subscriptions_matrix_1990[r,c] <-  as.numeric(data_1990_total$'Mobile_Subscriptions'[c]) -as.numeric(data_1990_total$'Mobile_Subscriptions'[r])
    internet_subscriptions_matrix_1990[r,c] <-  as.numeric(data_1990_total$'Internet_Subscriptions'[c]) -as.numeric(data_1990_total$'Internet_Subscriptions'[r])
    mediumhightech_exports_matrix_1990[r,c] <-  as.numeric(data_1990_total$'Medium_High_Tech_Exports'[c]) -as.numeric(data_1990_total$'Medium_High_Tech_Exports'[r])
    technicians_rd_matrix_1990[r,c] <-  as.numeric(data_1990_total$'Technicians_RD'[c]) -as.numeric(data_1990_total$'Technicians_RD'[r])
    
  }
}

life_expectancy__df_1990 <- as.data.frame(life_expectancy_matrix_1990)
mortality_rate_female_df_1990 <- as.data.frame(mortality_rate_female_matrix_1990)
mortality_rate_male_df_1990 <- as.data.frame(mortality_rate_male_matrix_1990)
survival_rate_female_df_1990 <- as.data.frame(survival_rate_female_matrix_1990)
survival_rate_male_df_1990 <- as.data.frame(survival_rate_male_matrix_1990)
telephone_subscriptions_df_1990 <- as.data.frame(telephone_subscriptions_matrix_1990)
electrical_power_df_1990 <- as.data.frame(electrical_power_matrix_1990)
mobile_subscriptions_df_1990 <- as.data.frame(mobile_subscriptions_matrix_1990)
internet_subscriptions_df_1990 <- as.data.frame(internet_subscriptions_matrix_1990)
mediumhightech_exports_df_1990 <- as.data.frame(mediumhightech_exports_matrix_1990)
technicians_rd_df_1990 <- as.data.frame(technicians_rd_matrix_1990)

######################### 2000

life_expectancy_matrix_2000 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mortality_rate_female_matrix_2000 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mortality_rate_male_matrix_2000 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
survival_rate_female_matrix_2000 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
survival_rate_male_matrix_2000 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
telephone_subscriptions_matrix_2000 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
electrical_power_matrix_2000 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mobile_subscriptions_matrix_2000 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
internet_subscriptions_matrix_2000 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
mediumhightech_exports_matrix_2000 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))
technicians_rd_matrix_2000 <- matrix(nrow = n_countries, ncol = n_countries, dimnames = list(countries, countries))


for (r in 1:length(countries)) {
  
  for (c in 1:length(countries)) {
    
    life_expectancy_matrix_2000[r,c] <-  as.numeric(data_2000_total$'Life_Expectancy'[c]) -as.numeric(data_2000_total$'Life_Expectancy'[r])
    mortality_rate_female_matrix_2000[r,c] <-  as.numeric(data_2000_total$'Mortality_Rate_Female'[c]) -as.numeric(data_2000_total$'Mortality_Rate_Female'[r])
    mortality_rate_male_matrix_2000[r,c] <-  as.numeric(data_2000_total$'Mortality_Rate_Male'[c]) -as.numeric(data_2000_total$'Mortality_Rate_Male'[r])
    survival_rate_female_matrix_2000[r,c] <-  as.numeric(data_2000_total$'Survival_Rate_Female'[c]) -as.numeric(data_2000_total$'Survival_Rate_Female'[r])
    survival_rate_male_matrix_2000[r,c] <-  as.numeric(data_2000_total$'Survival_Rate_Male'[c]) -as.numeric(data_2000_total$'Survival_Rate_Male'[r])
    telephone_subscriptions_matrix_2000[r,c] <-  as.numeric(data_2000_total$'Telephone_Subscriptions'[c]) -as.numeric(data_2000_total$'Telephone_Subscriptions'[r])
    electrical_power_matrix_2000[r,c] <-  as.numeric(data_2000_total$'Electrical_Power'[c]) -as.numeric(data_2000_total$'Electrical_Power'[r])
    mobile_subscriptions_matrix_2000[r,c] <-  as.numeric(data_2000_total$'Mobile_Subscriptions'[c]) -as.numeric(data_2000_total$'Mobile_Subscriptions'[r])
    internet_subscriptions_matrix_2000[r,c] <-  as.numeric(data_2000_total$'Internet_Subscriptions'[c]) -as.numeric(data_2000_total$'Internet_Subscriptions'[r])
    mediumhightech_exports_matrix_2000[r,c] <-  as.numeric(data_2000_total$'Medium_High_Tech_Exports'[c]) -as.numeric(data_2000_total$'Medium_High_Tech_Exports'[r])
    technicians_rd_matrix_2000[r,c] <-  as.numeric(data_2000_total$'Technicians_RD'[c]) -as.numeric(data_2000_total$'Technicians_RD'[r])
    
  }
}

life_expectancy__df_2000 <- as.data.frame(life_expectancy_matrix_2000)
mortality_rate_female_df_2000 <- as.data.frame(mortality_rate_female_matrix_2000)
mortality_rate_male_df_2000 <- as.data.frame(mortality_rate_male_matrix_2000)
survival_rate_female_df_2000 <- as.data.frame(survival_rate_female_matrix_2000)
survival_rate_male_df_2000 <- as.data.frame(survival_rate_male_matrix_2000)
telephone_subscriptions_df_2000 <- as.data.frame(telephone_subscriptions_matrix_2000)
electrical_power_df_2000 <- as.data.frame(electrical_power_matrix_2000)
mobile_subscriptions_df_2000 <- as.data.frame(mobile_subscriptions_matrix_2000)
internet_subscriptions_df_2000 <- as.data.frame(internet_subscriptions_matrix_2000)
mediumhightech_exports_df_2000 <- as.data.frame(mediumhightech_exports_matrix_2000)
technicians_rd_df_2000 <- as.data.frame(technicians_rd_matrix_2000)
