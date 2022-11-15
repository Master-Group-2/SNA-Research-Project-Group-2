library("readxl")
library("writexl")
library("dplyr")


my_data <- read_excel("5.1/Social Network Analysis for Data Scientists/Groupproject/SNA-Research-Project-Group-2/attributes_data.xlsx")

drop <- c("Time Code", "Country Code")
info_data = my_data[,!(names(my_data) %in% drop)]
colnames(info_data)
info_data_2 <- setNames(info_data, c("Year", "Country", "Airtransport", "energy", "Cereal", "Electricpower", "Employers", "telephonesubscriptions", "GDP", "Internet", "Lifeexpectancy", "techexports", "techmanufacturing", "Mobilesubscriptions", "Mortalityfemale", "Mortalitymale", "drinkingwater", "Journalarticles", "Internetservers", "Survival65female", "Survival65male", "Technicians"))


write_xlsx(info_data_2, "info_data.xlsx")


migration_data <- read_excel("migration_data.xlsx")

full_data <- merge(migration_data, info_data_2, by=c("Year","Country"))

write_xlsx(full_data, "full_data.xlsx")
