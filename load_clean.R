library("readxl")
library("writexl")


my_data <- read_excel("Data_Extract_From_Global_Bilateral_Migration.xlsx")

drop <- c("Year Code", "Country Origin Code", "Migration by Gender Name", "Migration by Gender Code")
migration_data = my_data[,!(names(my_data) %in% drop)]
df <- setNames(migration_data, c("Year", "Country","Belarus", "Czech Republic", "Germany", "Hungary", "Macedonia, FYR", "Russian Federation", "Ukraine", "Austria", "Belgium", "Bulgaria", "Croatia", "Denmark","Estonia", "France", "Finland", "Georgia", "Greece", "Iceland", "Ireland", "Italy", "Liechtenstein", "Lithuani", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "Albania", "Bosnia and Herzegovina", "Cyprus", "Latvia", "Monaco", "Slovak Republic"))

df

write_xlsx(df, "migration_data.xlsx")
 
