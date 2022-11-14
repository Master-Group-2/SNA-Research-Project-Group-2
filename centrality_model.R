## Load cleaned data
load('migration_networks.RData')

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

color <- rep('red', snafun::count_vertices(migration_networks[[1]]))
central_countries_1960 <- which(central_data['Centrality_1960'] > 50)
color[central_countries_1960] <- 'green'

plot(migration_networks[[1]],
     vertex.shape = 'circle',
     vertex.color = color, 
     vertex.size = sqrt(central_data[['Centrality_1960']]),
#     edge.width = igraph::E(migration_networks[[1]])$weight.scaled*15,
     edge.arrow.size = 0.2,
     vertex.label.color = 'black')
#     layout = igraph::layout.reingold.tilford(migration_networks[[1]]))


