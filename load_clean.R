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
migration_data_list <- list()
migration_data_list[[1]] <- migration_1960
migration_data_list[[2]] <- migration_1970
migration_data_list[[3]] <- migration_1980
migration_data_list[[4]] <- migration_1990
migration_data_list[[5]] <- migration_2000

length(migration_data_list[5])
class(migration_data_list[[5]])
class(migration_2000)
migration_list <- list()
for (migration in migration_data_list) {
  migration <- migration$Year <- NULL
  migration <- migration$Country <- NULL
  migration_list <- append(migration_list, migration)  
}
migration_list
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
test <- igraph::delete.edges(test, which(igraph::E(test)$weight<10000))
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

test <- igraph::set.vertex.attribute(test, 'region', index = 1, 'South')
test <- igraph::set.vertex.attribute(test, 'region', index = 2, 'West')
test <- igraph::set.vertex.attribute(test, 'region', index = 3, 'East')
test <- igraph::set.vertex.attribute(test, 'region', index = 4, 'West')
test <- igraph::set.vertex.attribute(test, 'region', index = 5, 'South')
test <- igraph::set.vertex.attribute(test, 'region', index = 6, 'East')
test <- igraph::set.vertex.attribute(test, 'region', index = 7, 'South')
test <- igraph::set.vertex.attribute(test, 'region', index = 8, 'South')
test <- igraph::set.vertex.attribute(test, 'region', index = 9, 'East')
test <- igraph::set.vertex.attribute(test, 'region', index = 10, 'North')
test <- igraph::set.vertex.attribute(test, 'region', index = 11, 'North')
test <- igraph::set.vertex.attribute(test, 'region', index = 12, 'North')
test <- igraph::set.vertex.attribute(test, 'region', index = 13, 'West')
test <- igraph::set.vertex.attribute(test, 'region', index = 14, 'East')
test <- igraph::set.vertex.attribute(test, 'region', index = 15, 'West')
test <- igraph::set.vertex.attribute(test, 'region', index = 16, 'South')
test <- igraph::set.vertex.attribute(test, 'region', index = 17, 'East')
test <- igraph::set.vertex.attribute(test, 'region', index = 18, 'North')
test <- igraph::set.vertex.attribute(test, 'region', index = 19, 'North')
test <- igraph::set.vertex.attribute(test, 'region', index = 20, 'South')
test <- igraph::set.vertex.attribute(test, 'region', index = 21, 'North')
test <- igraph::set.vertex.attribute(test, 'region', index = 22, 'West')
test <- igraph::set.vertex.attribute(test, 'region', index = 23, 'North')
test <- igraph::set.vertex.attribute(test, 'region', index = 24, 'West')
test <- igraph::set.vertex.attribute(test, 'region', index = 25, 'South')
test <- igraph::set.vertex.attribute(test, 'region', index = 26, 'South')
test <- igraph::set.vertex.attribute(test, 'region', index = 27, 'West')
test <- igraph::set.vertex.attribute(test, 'region', index = 28, 'North')
test <- igraph::set.vertex.attribute(test, 'region', index = 29, 'East')
test <- igraph::set.vertex.attribute(test, 'region', index = 30, 'South')
test <- igraph::set.vertex.attribute(test, 'region', index = 31, 'East')
test <- igraph::set.vertex.attribute(test, 'region', index = 32, 'East')
test <- igraph::set.vertex.attribute(test, 'region', index = 33, 'East')
test <- igraph::set.vertex.attribute(test, 'region', index = 34, 'South')
test <- igraph::set.vertex.attribute(test, 'region', index = 35, 'South')
test <- igraph::set.vertex.attribute(test, 'region', index = 36, 'North')
test <- igraph::set.vertex.attribute(test, 'region', index = 37, 'West')
test <- igraph::set.vertex.attribute(test, 'region', index = 38, 'East')
test <- igraph::set.vertex.attribute(test, 'region', index = 39, 'North')

colrs <- c("South"="yellow", "West"="green", "East"="red", "North"="blue")
igraph::V(test)$color <- colrs[igraph::V(test)$region]

igraph::get.vertex.attribute(test, 'color')

eastWest <- igraph::delete.vertices(test, which(igraph::V(test)$region=='North'))
eastWest2 <- igraph::delete.vertices(eastWest, which(igraph::V(eastWest)$region=='South'))

eastWest
eastWest2

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
     vertex.color = igraph::V(test)$color,
     vertex.size = 10,
     vertex.label.color = 'black',
)

## Centrality
snafun::v_stress(test)
snafun::g_centralize(test, measure = 'betweenness')$centralization

betweenness_central <- function(x, directed = FALSE){
  netw <- snafun::fix_cug_input(x, directed = directed)
  snafun::g_centralize(netw, 'betweenness')$centralization
}

betw_igraph <- function(x, directed = FALSE){
  netw <- snafun::fix_cug_input(x, directed = directed)
  igraph::centr_betw(netw, directed = TRUE)$centralization
}

test_network <- snafun::to_network(test)
summary(test_network)

sna::cug.test(test_network,
              FUN = betweenness_central,
              mode = 'digraph',
              cmode = 'edges',
              ignore.eval = FALSE,
              reps = 1000)

centrality_analysis <- function(igraph_graphs, years){
  countries <- igraph::get.vertex.attribute(igraph_graphs[1], 'name')
  central_data <- data.frame(countries)
  colnames(central_data) <- 'Country'
  
  for (index in 1:length(years)){
    central_data[sprintf('Centrality_%s', years[index])] <- snafun::v_stress(igraph_graphs[index])
  }
}

centrality <- snafun::v_stress(test_network)
countries <- igraph::get.vertex.attribute(test, 'name')

central_data <- data.frame(countries, centrality)
colnames(central_data) <- c('Country', 'Centrality_1960')

central_data['test'] <- countries
