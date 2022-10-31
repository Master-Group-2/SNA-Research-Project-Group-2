library("readxl")

my_data <- read_excel("migration_data.xlsx")

test <- graph_from_data_frame(my_data, directed = TRUE, vertices = NULL)

