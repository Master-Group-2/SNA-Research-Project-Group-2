library(gridExtra)

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
     edge.width = igraph::E(migration_networks[[1]])$weight.scaled*15,
     edge.arrow.size = 0.2,
     vertex.label.color = 'black',
     layout = igraph::layout_in_circle)


plot_centralities <- function(networks, years){
  central_data = centrality_analysis(networks, years)
  plots = list()
  
  for (year_idx in 1:length(years)){
    color <- rep('red', snafun::count_vertices(networks[[year_idx]]))
    central_countries <- which(central_data[[year_idx + 1]] > 50)
    color[central_countries] <- 'green'
    
    plots[[year_idx]] <- recordPlot(plot(networks[[year_idx]],
                                         vertex.shape = 'circle',
                                         vertex.color = color,
                                         vertex.size = sqrt(central_data[[year_idx + 1]]),
                                         edge.width = igraph::E(networks[[year_idx]])$weight.scaled*15,
                                         edge.arrow.size = 0.2,
                                         vertex.label.color = 'black',
                                         main = sprintf('Centrality per country in %s', years[year_idx]),
                                         layout = igraph::layout_in_circle))
  }
  #grid.arrange(grobs = plots, nrow = length(years)%/%2 + 1)
  plots
}

test <- plot_centralities(migration_networks, years)

grid.arrange(test)
class(test[[1]])

## Plot layouts
# layout_on_sphere (2)
# layout_nicely
# layout_as_tree
# layout.fruchterman.reingold
# layout_in_circle (1)
# layout_as_star
# layout_on_grid (2)
# layout_randomly
# layout_with_dh 
# layout_with_fr
# layout_with_gem
# layout_with_graphopt
# layout_with_kk (3)
# layout_with_lgl (3)
# layout_with_mds
# layout_with_sugiyama

