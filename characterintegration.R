library("readxl")
library("writexl")
library("dplyr")


my_data <- read_excel("attributes_data.xlsx")

drop <- c("Time Code", "Country Code")
attribute_data = my_data[,!(names(my_data) %in% drop)]
colnames(attribute_data)
attribute_data <- setNames(attribute_data, c("Year", "Country", "Air transport departures worldwide", "Alternative and nuclear energy (% of total energy use)", "Cereal production (metric tons)", "Electric power consumption (kWh per capita)", "Employers, total (% of total employment)", "Fixed telephone subscriptions", "GDP growth (annual %)", "Individuals using the Internet (% of population)", "Life expectancy at birth, total (years)", "Medium and high-tech exports (% manufactured exports)", "Medium and high-tech manufacturing value added (% manufacturing value added)", "Mobile cellular subscriptions", "Mortality rate, adult, female (per 1,000 female adults)", "Mortality rate, adult, male (per 1,000 male adults)", "People using safely managed drinking water services (% of population)", "Scientific and technical journal articles", "Secure Internet servers", "Survival to age 65, female (% of cohort)", "Survival to age 65, male (% of cohort)", "Technicians in R&D (per million people)"))


write_xlsx(attribute_data, "attribute_data.xlsx")


migration_data <- read_excel("migration_data.xlsx")

merge(migration_data, attribute_data, by=c("k1","k2"))
