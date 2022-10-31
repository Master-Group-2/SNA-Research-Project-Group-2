library("readxl")


my_data <- read_excel("C:/Users/Jeroen/Documents/School/Master_Year1/Social Network Analysis/Project/Data_Extract_From_Global_Bilateral_Migration.xlsx")

drop <- c("Year Code", "Country Origin Code", "Migration by Gender Name", "Migration by Gender Code")
migration_data = my_data[,!(names(my_data) %in% drop)]

migration_data
