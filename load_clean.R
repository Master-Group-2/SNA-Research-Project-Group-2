library("readxl")
library("writexl")
library("dplyr")

my_data <- read_excel("Data_Extract_From_Global_Bilateral_Migration.xlsx")

drop <- c("Year Code", "Country Origin Code", "Migration by Gender Name", "Migration by Gender Code")
migration_data = my_data[,!(names(my_data) %in% drop)]
#df <- setNames(migration_data, c("Year", "Country","Belarus", "Czech Republic", "Germany", "Hungary", "Macedonia, FYR", "Russian Federation", "Ukraine", "Austria", "Belgium", "Bulgaria", "Croatia", "Denmark","Estonia", "France", "Finland", "Georgia", "Greece", "Iceland", "Ireland", "Italy", "Liechtenstein", "Lithuani", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "Albania", "Bosnia and Herzegovina", "Cyprus", "Latvia", "Monaco", "Slovak Republic"))

write_xlsx(migration_data, "migration_data.xlsx")

migration_1960 <- migration_data %>% filter(Year == 1960)
migration_1970 <- migration_data %>% filter(Year == 1970)
migration_1980 <- migration_data %>% filter(Year == 1980)
migration_2000 <- migration_data %>% filter(Year == 2000)

