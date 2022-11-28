library("readxl")
library("writexl")
library("dplyr")
library("ggplot2")

# Migration data
migration_data <- read_excel('migration_data.xlsx')

# Additional attributes
my_data <- read_excel("attributes_data.xlsx")

# Rename additional attributes
drop <- c("Time Code", "Country Code")
info_data = my_data[,!(names(my_data) %in% drop)]
colnames(info_data)
info_data <- setNames(info_data, c("Year", "Country", "Airtransport", "Energy", "Cereal", "Electricpower", "Employers", "Telephonesubscriptions", "GDP", "Internet", "Lifeexpectancy", "Techexports", "Techmanufacturing", "Mobilesubscriptions", "Mortalityfemale", "Mortalitymale", "Drinkingwater", "Journalarticles", "Internetservers", "Survival65female", "Survival65male", "Technicians"))

# Filters for plots
netherlands <- info_data %>% filter(Country == 'Netherlands')
poland <- info_data %>% filter(Country == 'Poland')
ireland <- info_data %>% filter(Country == 'Ireland')

# Plots for report
plot(netherlands$Year, netherlands$GDP, type='l', main='GDP Growth for the Netherlands', xlab='Year', ylab='Growth in percentage')
plot(poland$Year, poland$Telephonesubscriptions, type='l', main='Fixed telephone subscriptions in Poland', xlab='Year', ylab='Amount of fixed telephone subscriptions')
plot(ireland$Year, ireland$Mortalitymale, type='l', main='Male mortality rate in Ireland', xlab='Year', ylab='Mortality rate, adult, male (per 1,000 male adults)')
