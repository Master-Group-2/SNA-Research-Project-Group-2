library("readxl")
library("writexl")
library("dplyr")

my_data <- read_excel("Data_Extract_From_Global_Bilateral_Migration.xlsx")

drop <- c("Year Code", "Country Origin Code", "Migration by Gender Name", "Migration by Gender Code")
migration_data = my_data[,!(names(my_data) %in% drop)]
migration_data <- setNames(migration_data, c("Year", "Country", "Belarus", "Czech Republic", "Germany", "Hungary", "Macedonia, FYR", "Russian Federation", "Ukraine", "Austria", "Belgium", "Bulgaria", "Croatia", "Denmark", "Estonia", "France", "Finland", "Georgia", "Greece", "Iceland", "Ireland", "Italy", "Liechtenstein", "Lithuania", "Luxembourg", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "Albania", "Bosnia and Herzegovina", "Cyprus", "Latvia", "Monaco", "Slovak Republic"))
#migration_data[migration_data == 0] <- NA

write_xlsx(migration_data, "migration_data.xlsx")

migration_1960 <- migration_data %>% filter(Year == 1960)
migration_1970 <- migration_data %>% filter(Year == 1970)
migration_1980 <- migration_data %>% filter(Year == 1980)
migration_1990 <- migration_data %>% filter(Year == 1990)
migration_2000 <- migration_data %>% filter(Year == 2000)

countries <- migration_1960$Country

migration_1960_test <- migration_1960
migration_1960_test$Year <- NULL
migration_1960_test$Country <- NULL

migration_1960_test <- migration_1960_test[ ,sort(names(migration_1960_test))]
rownames(migration_1960_test) <- countries

migration_1960_test2 <- migration_1960_test[sort(row.names(migration_1960_test)), ]

migration_1960_matrix <- data.matrix(migration_1960_test2)
test <- igraph::graph_from_adjacency_matrix(migration_1960_matrix,
                                            mode = c('directed'),
                                            weighted = TRUE)
summary(test)
igraph::edge.attributes(test)
test <- igraph::delete.edges(test, which(igraph::E(test)$weight<100))
test <- igraph::simplify(test)
test
igraph::tkplot(test)

# Adding an extra edge attribute with scaled weights
# First creating the function to sclae
fun_range <- function(x) {                              # Create user-defined function
  (x - min(x)) / (max(x)-min(x))
}

# Then running the function and adding it to our graph 
weightscaled <- fun_range(x = igraph::edge.attributes(test)$weight)
test <- igraph::set.edge.attribute(test, "weight.scaled", index = igraph::E(test), weightscaled)

# Plotting the network 
plot(test,
     main= "Migrations between EU countries 1960",
     edge.width = igraph::E(test)$weight.scaled*20,
     edge.arrow.size = 0.1,
     curved = TRUE,
     vertex.size = 20,
     layout = igraph::layout_on_sphere
)


plot(test,
     vertex.shape="circle",
     layout = igraph::layout_on_grid,
     main= "Migrations between EU countries 1960",
     edge.width = igraph::E(test)$weight.scaled*15,
     edge.arrow.size = 0.1,
     vertex.size = 10,
)

plot(test,
     vertex.shape="circle",
     layout = igraph::layout_randomly,
     main= "Migrations between EU countries 1960",
     edge.width = igraph::E(test)$weight.scaled*15,
     edge.arrow.size = 0.1,
     vertex.size = 10,
)
