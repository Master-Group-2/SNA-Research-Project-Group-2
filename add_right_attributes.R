library(dplyr)
library(tidyr)

full_data <- read_excel("full_data.xlsx")
attributes_data <- read_excel('attributes_data.xlsx')
attributes_data <-subset(attributes_data, attributes_data$'Country Name' !="Czechia")


data_1960 <- attributes_data %>% filter(Time >= 1960 & Time < 1970)
data_1970 <- attributes_data %>% filter(Time >= 1970 & Time < 1980)
data_1980 <- attributes_data %>% filter(Time >= 1980 & Time < 1990)
data_1990 <- attributes_data %>% filter(Time >= 1990 & Time < 2000)
data_2000 <- attributes_data %>% filter(Time >= 2000 & Time < 2010)

countries <- unique(attributes_data$`Country Name`)
countries <- countries[1:34]

lst_life_expectancy_birth_1960 <- list()
lst_life_expectancy_birth_1970 <- list()
lst_life_expectancy_birth_1980 <- list()
lst_life_expectancy_birth_1990 <- list()
lst_life_expectancy_birth_2000 <- list()

lst_mortality_rate_female_1960 <- list()
lst_mortality_rate_female_1970 <- list()
lst_mortality_rate_female_1980 <- list()
lst_mortality_rate_female_1990 <- list()
lst_mortality_rate_female_2000 <- list()

lst_mortality_rate_male_1960 <- list()
lst_mortality_rate_male_1970 <- list()
lst_mortality_rate_male_1980 <- list()
lst_mortality_rate_male_1990 <- list()
lst_mortality_rate_male_2000 <- list()

lst_survival_rate_female_1960 <- list()
lst_survival_rate_female_1970 <- list()
lst_survival_rate_female_1980 <- list()
lst_survival_rate_female_1990 <- list()
lst_survival_rate_female_2000 <- list()

lst_survival_rate_male_1960 <- list()
lst_survival_rate_male_1970 <- list()
lst_survival_rate_male_1980 <- list()
lst_survival_rate_male_1990 <- list()
lst_survival_rate_male_2000 <- list()

lst_telephone_subscriptions_1960 <- list()
lst_telephone_subscriptions_1970 <- list()
lst_telephone_subscriptions_1980 <- list()
lst_telephone_subscriptions_1990 <- list()
lst_telephone_subscriptions_2000 <- list()

lst_electrical_power_1960 <- list()
lst_electrical_power_1970 <- list()
lst_electrical_power_1980 <- list()
lst_electrical_power_1990 <- list()
lst_electrical_power_2000 <- list()

lst_mobile_subscriptions_1960 <- list()
lst_mobile_subscriptions_1970 <- list()
lst_mobile_subscriptions_1980 <- list()
lst_mobile_subscriptions_1990 <- list()
lst_mobile_subscriptions_2000 <- list()

lst_internet_subscriptions_1960 <- list()
lst_internet_subscriptions_1970 <- list()
lst_internet_subscriptions_1980 <- list()
lst_internet_subscriptions_1990 <- list()
lst_internet_subscriptions_2000 <- list()

lst_mediumhightech_exports_1960 <- list()
lst_mediumhightech_exports_1970 <- list()
lst_mediumhightech_exports_1980 <- list()
lst_mediumhightech_exports_1990 <- list()
lst_mediumhightech_exports_2000 <- list()

lst_technicians_rd_1960 <- list()
lst_technicians_rd_1970 <- list()
lst_technicians_rd_1980 <- list()
lst_technicians_rd_1990 <- list()
lst_technicians_rd_2000 <- list()

for (i in 1:length(countries)) {
  
  country <- countries[i]
  data_1960_country <- data_1960 %>% filter(data_1960$'Country Name' == country)
  
  avg_life_expectancy_birth_country <- mean(as.numeric(data_1960_country$'Life expectancy at birth, total (years) [SP.DYN.LE00.IN]'), na.rm = TRUE)
  lst_life_expectancy_birth_1960 <- append(lst_life_expectancy_birth_1960, avg_life_expectancy_birth_country)
  
  avg_mortality_rate_female_country <- mean(as.numeric(data_1960_country$"Mortality rate, adult, female (per 1,000 female adults) [SP.DYN.AMRT.FE]"), na.rm = TRUE)
  lst_mortality_rate_female_1960 <- append(lst_mortality_rate_female_1960, avg_mortality_rate_female_country)
  
  avg_mortality_rate_male_country <- mean(as.numeric(data_1960_country$"Mortality rate, adult, male (per 1,000 male adults) [SP.DYN.AMRT.MA]"), na.rm = TRUE)
  lst_mortality_rate_male_1960 <- append(lst_mortality_rate_male_1960, avg_mortality_rate_male_country)
  
  avg_survival_rate_female_country <- mean(as.numeric(data_1960_country$"Survival to age 65, female (% of cohort) [SP.DYN.TO65.FE.ZS]"), na.rm = TRUE)
  lst_survival_rate_female_1960 <- append(lst_survival_rate_female_1960, avg_survival_rate_female_country)
  
  avg_survival_rate_male_country <- mean(as.numeric(data_1960_country$"Survival to age 65, male (% of cohort) [SP.DYN.TO65.MA.ZS]"), na.rm = TRUE)
  lst_survival_rate_male_1960 <- append(lst_survival_rate_male_1960, avg_survival_rate_male_country)
  
  med_telephone_subscriptions_country <- median(as.numeric(data_1960_country$"Fixed telephone subscriptions [IT.MLT.MAIN]"), na.rm = TRUE)
  lst_telephone_subscriptions_1960 <- append(lst_telephone_subscriptions_1960, med_telephone_subscriptions_country)
  
  med_electrical_power_country <- median(as.numeric(data_1960_country$"Electric power consumption (kWh per capita) [EG.USE.ELEC.KH.PC]"), na.rm = TRUE)
  lst_electrical_power_1960 <- append(lst_electrical_power_1960, med_electrical_power_country)
  
  med_mobile_subscriptions_country <- median(as.numeric(data_1960_country$"Mobile cellular subscriptions [IT.CEL.SETS]"), na.rm = TRUE)
  lst_mobile_subscriptions_1960 <- append(lst_mobile_subscriptions_1960, med_mobile_subscriptions_country)
  
  med_internet_subscriptions_country <- median(as.numeric(data_1960_country$"Individuals using the Internet (% of population) [IT.NET.USER.ZS]"), na.rm = TRUE)
  lst_internet_subscriptions_1960 <- append(lst_internet_subscriptions_1960, med_internet_subscriptions_country)
  
  avg_mediumhightech_exports_country <- mean(as.numeric(data_1960_country$"Medium and high-tech exports (% manufactured exports) [TX.MNF.TECH.ZS.UN]"), na.rm = TRUE)
  lst_mediumhightech_exports_1960 <- append(lst_mediumhightech_exports_1960, avg_mediumhightech_exports_country)
  
  med_technicians_rd_country <- median(as.numeric(data_1960_country$"Technicians in R&D (per million people) [SP.POP.TECH.RD.P6]"), na.rm = TRUE)
  lst_technicians_rd_1960 <- append(lst_technicians_rd_1960, med_technicians_rd_country)
  
}

for (i in 1:length(countries)) {
  
  country <- countries[i]
  data_1970_country <- data_1970 %>% filter(data_1970$'Country Name' == country)
  
  avg_life_expectancy_birth_country <- mean(as.numeric(data_1970_country$'Life expectancy at birth, total (years) [SP.DYN.LE00.IN]'), na.rm = TRUE)
  lst_life_expectancy_birth_1970 <- append(lst_life_expectancy_birth_1970, avg_life_expectancy_birth_country)
  
  avg_mortality_rate_female_country <- mean(as.numeric(data_1970_country$"Mortality rate, adult, female (per 1,000 female adults) [SP.DYN.AMRT.FE]"), na.rm = TRUE)
  lst_mortality_rate_female_1970 <- append(lst_mortality_rate_female_1970, avg_mortality_rate_female_country)
  
  avg_mortality_rate_male_country <- mean(as.numeric(data_1970_country$"Mortality rate, adult, male (per 1,000 male adults) [SP.DYN.AMRT.MA]"), na.rm = TRUE)
  lst_mortality_rate_male_1970 <- append(lst_mortality_rate_male_1970, avg_mortality_rate_male_country)
  
  avg_survival_rate_female_country <- mean(as.numeric(data_1970_country$"Survival to age 65, female (% of cohort) [SP.DYN.TO65.FE.ZS]"), na.rm = TRUE)
  lst_survival_rate_female_1970 <- append(lst_survival_rate_female_1970, avg_survival_rate_female_country)
  
  avg_survival_rate_male_country <- mean(as.numeric(data_1960_country$"Survival to age 65, male (% of cohort) [SP.DYN.TO65.MA.ZS]"), na.rm = TRUE)
  lst_survival_rate_male_1970 <- append(lst_survival_rate_male_1970, avg_survival_rate_male_country)
  
  med_telephone_subscriptions_country <- median(as.numeric(data_1970_country$"Fixed telephone subscriptions [IT.MLT.MAIN]"), na.rm = TRUE)
  lst_telephone_subscriptions_1970 <- append(lst_telephone_subscriptions_1970, med_telephone_subscriptions_country)
  
  med_electrical_power_country <- median(as.numeric(data_1970_country$"Electric power consumption (kWh per capita) [EG.USE.ELEC.KH.PC]"), na.rm = TRUE)
  lst_electrical_power_1970 <- append(lst_electrical_power_1970, med_electrical_power_country)
  
  med_mobile_subscriptions_country <- median(as.numeric(data_1970_country$"Mobile cellular subscriptions [IT.CEL.SETS]"), na.rm = TRUE)
  lst_mobile_subscriptions_1970 <- append(lst_mobile_subscriptions_1970, med_mobile_subscriptions_country)
  
  med_internet_subscriptions_country <- median(as.numeric(data_1970_country$"Individuals using the Internet (% of population) [IT.NET.USER.ZS]"), na.rm = TRUE)
  lst_internet_subscriptions_1970 <- append(lst_internet_subscriptions_1970, med_internet_subscriptions_country)
  
  avg_mediumhightech_exports_country <- mean(as.numeric(data_1970_country$"Medium and high-tech exports (% manufactured exports) [TX.MNF.TECH.ZS.UN]"), na.rm = TRUE)
  lst_mediumhightech_exports_1970 <- append(lst_mediumhightech_exports_1970, avg_mediumhightech_exports_country)
  
  med_technicians_rd_country <- median(as.numeric(data_1970_country$"Technicians in R&D (per million people) [SP.POP.TECH.RD.P6]"), na.rm = TRUE)
  lst_technicians_rd_1970 <- append(lst_technicians_rd_1970, med_technicians_rd_country)
  
}

for (i in 1:length(countries)) {
  
  country <- countries[i]
  data_1980_country <- data_1980 %>% filter(data_1980$'Country Name' == country)
  
  avg_life_expectancy_birth_country <- mean(as.numeric(data_1980_country$'Life expectancy at birth, total (years) [SP.DYN.LE00.IN]'), na.rm = TRUE)
  lst_life_expectancy_birth_1980 <- append(lst_life_expectancy_birth_1980, avg_life_expectancy_birth_country)
  
  avg_mortality_rate_female_country <- mean(as.numeric(data_1980_country$"Mortality rate, adult, female (per 1,000 female adults) [SP.DYN.AMRT.FE]"), na.rm = TRUE)
  lst_mortality_rate_female_1980 <- append(lst_mortality_rate_female_1980, avg_mortality_rate_female_country)
  
  avg_mortality_rate_male_country <- mean(as.numeric(data_1980_country$"Mortality rate, adult, male (per 1,000 male adults) [SP.DYN.AMRT.MA]"), na.rm = TRUE)
  lst_mortality_rate_male_1980 <- append(lst_mortality_rate_male_1980, avg_mortality_rate_male_country)
  
  avg_survival_rate_female_country <- mean(as.numeric(data_1980_country$"Survival to age 65, female (% of cohort) [SP.DYN.TO65.FE.ZS]"), na.rm = TRUE)
  lst_survival_rate_female_1980 <- append(lst_survival_rate_female_1980, avg_survival_rate_female_country)
  
  avg_survival_rate_male_country <- mean(as.numeric(data_1980_country$"Survival to age 65, male (% of cohort) [SP.DYN.TO65.MA.ZS]"), na.rm = TRUE)
  lst_survival_rate_male_1980 <- append(lst_survival_rate_male_1980, avg_survival_rate_male_country)
  
  med_telephone_subscriptions_country <- median(as.numeric(data_1980_country$"Fixed telephone subscriptions [IT.MLT.MAIN]"), na.rm = TRUE)
  lst_telephone_subscriptions_1980 <- append(lst_telephone_subscriptions_1980, med_telephone_subscriptions_country)
  
  med_electrical_power_country <- median(as.numeric(data_1980_country$"Electric power consumption (kWh per capita) [EG.USE.ELEC.KH.PC]"), na.rm = TRUE)
  lst_electrical_power_1980 <- append(lst_electrical_power_1980, med_electrical_power_country)
  
  med_mobile_subscriptions_country <- median(as.numeric(data_1980_country$"Mobile cellular subscriptions [IT.CEL.SETS]"), na.rm = TRUE)
  lst_mobile_subscriptions_1980 <- append(lst_mobile_subscriptions_1980, med_mobile_subscriptions_country)
  
  med_internet_subscriptions_country <- median(as.numeric(data_1980_country$"Individuals using the Internet (% of population) [IT.NET.USER.ZS]"), na.rm = TRUE)
  lst_internet_subscriptions_1980 <- append(lst_internet_subscriptions_1980, med_internet_subscriptions_country)
  
  avg_mediumhightech_exports_country <- mean(as.numeric(data_1980_country$"Medium and high-tech exports (% manufactured exports) [TX.MNF.TECH.ZS.UN]"), na.rm = TRUE)
  lst_mediumhightech_exports_1980 <- append(lst_mediumhightech_exports_1980, avg_mediumhightech_exports_country)
  
  med_technicians_rd_country <- median(as.numeric(data_1980_country$"Technicians in R&D (per million people) [SP.POP.TECH.RD.P6]"), na.rm = TRUE)
  lst_technicians_rd_1980 <- append(lst_technicians_rd_1980, med_technicians_rd_country)
  
}

for (i in 1:length(countries)) {
  
  country <- countries[i]
  data_1990_country <- data_1990 %>% filter(data_1970$'Country Name' == country)
  
  avg_life_expectancy_birth_country <- mean(as.numeric(data_1990_country$'Life expectancy at birth, total (years) [SP.DYN.LE00.IN]'), na.rm = TRUE)
  lst_life_expectancy_birth_1990 <- append(lst_life_expectancy_birth_1990, avg_life_expectancy_birth_country)
  
  avg_mortality_rate_female_country <- mean(as.numeric(data_1990_country$"Mortality rate, adult, female (per 1,000 female adults) [SP.DYN.AMRT.FE]"), na.rm = TRUE)
  lst_mortality_rate_female_1990 <- append(lst_mortality_rate_female_1990, avg_mortality_rate_female_country)
  
  avg_mortality_rate_male_country <- mean(as.numeric(data_1990_country$"Mortality rate, adult, male (per 1,000 male adults) [SP.DYN.AMRT.MA]"), na.rm = TRUE)
  lst_mortality_rate_male_1990 <- append(lst_mortality_rate_male_1990, avg_mortality_rate_male_country)
  
  avg_survival_rate_female_country <- mean(as.numeric(data_1990_country$"Survival to age 65, female (% of cohort) [SP.DYN.TO65.FE.ZS]"), na.rm = TRUE)
  lst_survival_rate_female_1990 <- append(lst_survival_rate_female_1990, avg_survival_rate_female_country)
  
  avg_survival_rate_male_country <- mean(as.numeric(data_1990_country$"Survival to age 65, male (% of cohort) [SP.DYN.TO65.MA.ZS]"), na.rm = TRUE)
  lst_survival_rate_male_1990 <- append(lst_survival_rate_male_1990, avg_survival_rate_male_country)
  
  med_telephone_subscriptions_country <- median(as.numeric(data_1990_country$"Fixed telephone subscriptions [IT.MLT.MAIN]"), na.rm = TRUE)
  lst_telephone_subscriptions_1990 <- append(lst_telephone_subscriptions_1990, med_telephone_subscriptions_country)
  
  med_electrical_power_country <- median(as.numeric(data_1990_country$"Electric power consumption (kWh per capita) [EG.USE.ELEC.KH.PC]"), na.rm = TRUE)
  lst_electrical_power_1990 <- append(lst_electrical_power_1990, med_electrical_power_country)
  
  med_mobile_subscriptions_country <- median(as.numeric(data_1990_country$"Mobile cellular subscriptions [IT.CEL.SETS]"), na.rm = TRUE)
  lst_mobile_subscriptions_1990 <- append(lst_mobile_subscriptions_1990, med_mobile_subscriptions_country)
  
  med_internet_subscriptions_country <- median(as.numeric(data_1990_country$"Individuals using the Internet (% of population) [IT.NET.USER.ZS]"), na.rm = TRUE)
  lst_internet_subscriptions_1990 <- append(lst_internet_subscriptions_1990, med_internet_subscriptions_country)
  
  avg_mediumhightech_exports_country <- mean(as.numeric(data_1990_country$"Medium and high-tech exports (% manufactured exports) [TX.MNF.TECH.ZS.UN]"), na.rm = TRUE)
  lst_mediumhightech_exports_1990 <- append(lst_mediumhightech_exports_1990, avg_mediumhightech_exports_country)
  
  med_technicians_rd_country <- median(as.numeric(data_1990_country$"Technicians in R&D (per million people) [SP.POP.TECH.RD.P6]"), na.rm = TRUE)
  lst_technicians_rd_1990 <- append(lst_technicians_rd_1990, med_technicians_rd_country)
}

for (i in 1:length(countries)) {
  
  country <- countries[i]
  data_2000_country <- data_2000 %>% filter(data_2000$'Country Name' == country)
  
  avg_life_expectancy_birth_country <- mean(as.numeric(data_2000_country$'Life expectancy at birth, total (years) [SP.DYN.LE00.IN]'), na.rm = TRUE)
  lst_life_expectancy_birth_2000 <- append(lst_life_expectancy_birth_2000, avg_life_expectancy_birth_country)
  
  avg_mortality_rate_female_country <- mean(as.numeric(data_2000_country$"Mortality rate, adult, female (per 1,000 female adults) [SP.DYN.AMRT.FE]"), na.rm = TRUE)
  lst_mortality_rate_female_2000 <- append(lst_mortality_rate_female_2000, avg_mortality_rate_female_country)
  
  avg_mortality_rate_male_country <- mean(as.numeric(data_2000_country$"Mortality rate, adult, male (per 1,000 male adults) [SP.DYN.AMRT.MA]"), na.rm = TRUE)
  lst_mortality_rate_male_2000 <- append(lst_mortality_rate_male_2000, avg_mortality_rate_male_country)
  
  avg_survival_rate_female_country <- mean(as.numeric(data_2000_country$"Survival to age 65, female (% of cohort) [SP.DYN.TO65.FE.ZS]"), na.rm = TRUE)
  lst_survival_rate_female_2000 <- append(lst_survival_rate_female_2000, avg_survival_rate_female_country)
  
  avg_survival_rate_male_country <- mean(as.numeric(data_2000_country$"Survival to age 65, male (% of cohort) [SP.DYN.TO65.MA.ZS]"), na.rm = TRUE)
  lst_survival_rate_male_2000 <- append(lst_survival_rate_male_2000, avg_survival_rate_male_country)
  
  med_telephone_subscriptions_country <- median(as.numeric(data_2000_country$"Fixed telephone subscriptions [IT.MLT.MAIN]"), na.rm = TRUE)
  lst_telephone_subscriptions_2000 <- append(lst_telephone_subscriptions_2000, med_telephone_subscriptions_country)
  
  med_electrical_power_country <- median(as.numeric(data_2000_country$"Electric power consumption (kWh per capita) [EG.USE.ELEC.KH.PC]"), na.rm = TRUE)
  lst_electrical_power_2000 <- append(lst_electrical_power_2000, med_electrical_power_country)
  
  med_mobile_subscriptions_country <- median(as.numeric(data_2000_country$"Mobile cellular subscriptions [IT.CEL.SETS]"), na.rm = TRUE)
  lst_mobile_subscriptions_2000 <- append(lst_mobile_subscriptions_2000, med_mobile_subscriptions_country)
  
  med_internet_subscriptions_country <- median(as.numeric(data_2000_country$"Individuals using the Internet (% of population) [IT.NET.USER.ZS]"), na.rm = TRUE)
  lst_internet_subscriptions_2000 <- append(lst_internet_subscriptions_2000, med_internet_subscriptions_country)
  
  avg_mediumhightech_exports_country <- mean(as.numeric(data_2000_country$"Medium and high-tech exports (% manufactured exports) [TX.MNF.TECH.ZS.UN]"), na.rm = TRUE)
  lst_mediumhightech_exports_2000 <- append(lst_mediumhightech_exports_2000, avg_mediumhightech_exports_country)
  
  med_technicians_rd_country <- median(as.numeric(data_2000_country$"Technicians in R&D (per million people) [SP.POP.TECH.RD.P6]"), na.rm = TRUE)
  lst_technicians_rd_2000 <- append(lst_technicians_rd_2000, med_technicians_rd_country)
  
}

full_data_v1 <- full_data

col_life_expectancy_birth <- c(lst_life_expectancy_birth_1960, lst_life_expectancy_birth_1970, lst_life_expectancy_birth_1980, lst_life_expectancy_birth_1990, lst_life_expectancy_birth_2000)
full_data_v1$`Life_Expectancy` <- col_life_expectancy_birth

col_mortality_rate_female <- c(lst_mortality_rate_female_1960, lst_mortality_rate_female_1970, lst_mortality_rate_female_1980, lst_mortality_rate_female_1990, lst_mortality_rate_female_2000)  
full_data_v1$`Mortality_Rate_Female` <- col_mortality_rate_female

col_mortality_rate_male <- c(lst_mortality_rate_male_1960, lst_mortality_rate_male_1970, lst_mortality_rate_male_1980, lst_mortality_rate_male_1990, lst_mortality_rate_male_2000)
full_data_v1$`Mortality_Rate_Male` <- col_mortality_rate_male

col_survival_rate_female <- c(lst_survival_rate_female_1960, lst_survival_rate_female_1970, lst_survival_rate_female_1980, lst_survival_rate_female_1990, lst_survival_rate_female_2000) 
full_data_v1$`Survival_Rate_Female` <- col_survival_rate_female
  
col_survival_rate_male <- c(lst_survival_rate_male_1960, lst_survival_rate_male_1970, lst_survival_rate_male_1980, lst_survival_rate_male_1990, lst_survival_rate_male_2000) 
full_data_v1$`Survival_Rate_Male` <- col_survival_rate_male

col_telephone_subscriptions <- c(lst_telephone_subscriptions_1960, lst_telephone_subscriptions_1970, lst_telephone_subscriptions_1980, lst_telephone_subscriptions_1990, lst_telephone_subscriptions_2000)
full_data_v1$`Telephone_Subscriptions` <- col_telephone_subscriptions

col_electrical_power <- c(lst_electrical_power_1960, lst_electrical_power_1970, lst_electrical_power_1980, lst_electrical_power_1990, lst_electrical_power_2000)
full_data_v1$`Electrical_Power` <- col_electrical_power

col_mobile_subscriptions <- c(lst_mobile_subscriptions_1960, lst_mobile_subscriptions_1970, lst_mobile_subscriptions_1980, lst_mobile_subscriptions_1990, lst_mobile_subscriptions_2000)
full_data_v1$`Mobile_Subscriptions` <- col_mobile_subscriptions

col_internet_subscriptions <- c(lst_internet_subscriptions_1960, lst_internet_subscriptions_1970, lst_electrical_power_1980, lst_internet_subscriptions_1990, lst_internet_subscriptions_2000)
full_data_v1$`Internet_Subscriptions` <- col_internet_subscriptions

col_mediumhightech_exports <- c(lst_mediumhightech_exports_1960, lst_mediumhightech_exports_1970, lst_mediumhightech_exports_1980, lst_mediumhightech_exports_1990, lst_mediumhightech_exports_2000)
full_data_v1$`Medium_High_Tech_Exports` <- col_mediumhightech_exports

col_technicians_rd <- c(lst_technicians_rd_1960, lst_technicians_rd_1970, lst_technicians_rd_1980, lst_technicians_rd_1990, lst_technicians_rd_2000)
full_data_v1$`Technicians_RD` <- col_technicians_rd

full_data_v2 <- full_data_v1

library("writexl")
write_xlsx(full_data_v2, "full_data_v2.xlsx")

