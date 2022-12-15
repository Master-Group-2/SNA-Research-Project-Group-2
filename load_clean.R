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

migration_networks <- list()

add_region_attribute <- function(graph){
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 1, 'South')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 2, 'West')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 3, 'East')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 4, 'West')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 5, 'South')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 6, 'East')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 7, 'South')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 8, 'South')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 9, 'East')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 10, 'North')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 11, 'North')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 12, 'North')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 13, 'West')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 14, 'East')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 15, 'West')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 16, 'South')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 17, 'East')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 18, 'North')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 19, 'North')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 20, 'South')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 21, 'North')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 22, 'West')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 23, 'North')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 24, 'West')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 25, 'South')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 26, 'South')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 27, 'West')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 28, 'North')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 29, 'East')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 30, 'South')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 31, 'East')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 32, 'East')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 33, 'East')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 34, 'South')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 35, 'South')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 36, 'North')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 37, 'West')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 38, 'East')
  graph <- igraph::set.vertex.attribute(graph, 'region', index = 39, 'North')
  
  colrs <- c("South"="yellow", "West"="green", "East"="red", "North"="blue")
  igraph::V(graph)$color <- colrs[igraph::V(graph)$region]
  return(graph)
}

# First creating the function to sclae
fun_range <- function(x) {                              # Create user-defined function
  (x - min(x)) / (max(x)-min(x))
}


for(i in 1:length(migration_data_list))
{
  migration_data_list[[i]]$Year <- NULL
  countries <- migration_data_list[[i]]$Country
  migration_data_list[[i]]$Country <- NULL
  
  placeholder <- migration_data_list[[i]]
  placeholder <- placeholder[ ,sort(names(placeholder))]
  rownames(placeholder) <- countries
  
  migration_test2 <- placeholder[sort(row.names(placeholder)), ]
  
  migration_matrix <- data.matrix(migration_test2)
  migration_networks[[i]] <- igraph::graph_from_adjacency_matrix(migration_matrix,
                                              mode = c('directed'),
                                              weighted = TRUE)
  migration_networks[[i]] <- add_region_attribute(migration_networks[[i]])
  
  weightscaled <- fun_range(x = igraph::edge.attributes(migration_networks[[i]])$weight)
  migration_networks[[i]] <- igraph::set.edge.attribute(migration_networks[[i]], "weightscaled", index = igraph::E(migration_networks[[i]]), weightscaled)
  
}

migration_data_list
migration_networks

#test <- igraph::delete.edges(test, which(igraph::E(test)$weight<10000))
#test <- igraph::simplify(test)


#eastWest <- igraph::delete.vertices(test, which(igraph::V(test)$region=='North'))
#eastWest2 <- igraph::delete.vertices(eastWest, which(igraph::V(eastWest)$region=='South'))

# Plotting the network 
#plot(migration_networks[[1]],
 #    vertex.shape="circle",
  #   layout = igraph::layout_randomly,
   #  main= "Migrations between EU countries 1960",
    # edge.width = igraph::E(migration_networks[[1]])$weightscaled*15,
     #edge.arrow.size = 0.1,
     #vertex.color = igraph::V(migration_networks[[1]])$color,
     #vertex.size = 10,
     #vertex.label.color = 'black',
#)

## Centrality Analysis
centrality_analysis <- function(igraph_graphs, years){
  countries <- igraph::get.vertex.attribute(igraph_graphs[[1]], 'name')
  central_data <- data.frame(countries)
  colnames(central_data) <- 'Country'
  
  for (index in 1:length(years)){
    central_data[sprintf('Centrality_%s', years[index])] <- snafun::v_stress(igraph_graphs[[index]])
  }
  central_data
}

years <- c(1960, 1970, 1980, 1990, 2000)
central_data <- centrality_analysis(migration_networks, years)

## Igraph to network
migration_networks_network <- list()
for (network_no in 1:length(migration_networks)){
  migration_networks_network[[network_no]] <- snafun::to_network(migration_networks[[network_no]])
}

### QAP test example
# mod <- sna::netlm(y = migration_networks_network[[1]], 
#                   x = list(migration_networks_network[c(2:5)]), 
#                   nullhyp = 'qapspp', reps = 1001)
# mod$names <- c("Intcpt", "1970", "1980", "1990", "2000")
# summary(mod)

