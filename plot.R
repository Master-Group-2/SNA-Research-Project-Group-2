library("readxl")
library("writexl")
library("dplyr")
library("ggplot2")

my_data <- read_excel("C:/Users/Jeroen/Downloads/P_Data_Extract_From_World_Development_Indicators (1).xlsx")
migration_data <- read_excel("C:/Users/Jeroen/Documents/School/Master_Year1/Social Network Analysis/Project/SNA-Research-Project-Group-2/Data_Extract_From_Global_Bilateral_Migration.xlsx")
drop <- c("Year Code", "Country Origin Code", "Migration by Gender Name", "Migration by Gender Code")
migration_data = migration_data[,!(names(migration_data) %in% drop)]
migration_data <- setNames(migration_data, c("Year", "Country", "Belarus", "Czech Republic", "Germany", "Hungary", "Macedonia, FYR", "Russian Federation", "Ukraine", "Austria", "Belgium", "Bulgaria", "Croatia", "Denmark", "Estonia", "France", "Finland", "Georgia", "Greece", "Iceland", "Ireland", "Italy", "Liechtenstein", "Lithuania", "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "Albania", "Bosnia and Herzegovina", "Cyprus", "Latvia", "Monaco", "Slovak Republic"))
migration_data
#migration_data[migration_data == 0] <- NA

#write_xlsx(migration_data, "migration_data.xlsx")

my_data
netherlands <- my_data %>% filter(Country == 'Netherlands')
poland <- my_data %>% filter(Country == 'Poland')
ireland <- my_data %>% filter(Country == 'Ireland')

poland$Time
poland_migration <- migration_data %>% filter(Country == 'Poland')

netherlands
colnames(migration_1960)

plot(netherlands$Time, netherlands$GDP, type='l', main='GDP Growth for the Netherlands', xlab='Year', ylab='Growth in percentage')
plot(poland$Time, poland$telephone, type='l', main='Fixed telephone subscriptions in Poland', xlab='Year', ylab='Amount of fixed telephone subscriptions')
plot(ireland$Time, ireland$mortality_rate, type='l', main='Male mortality rate in Ireland', xlab='Year', ylab='Mortality rate, adult, male (per 1,000 male adults)')

