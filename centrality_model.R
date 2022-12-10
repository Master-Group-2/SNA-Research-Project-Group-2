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

#### CUG Test
year = c()
country = c()
centrality = c()

# Find maximum centrality per year
for (i in 1:length(years)){
  index = match(max(central_data[i + 1]), central_data[[i + 1]])
  year = c(year, years[i])
  country = c(country, central_data$Country[index])
  centrality = c(centrality, central_data[[i + 1]][index])
}

# Create dataframe of maximum centralities
max_centrality_year <- data.frame(country, year, centrality)

# Function for CUG test --> Finds maximum centrality
max_centrality <- function(graph, directed = TRUE){
  x <- snafun::fix_cug_input(graph, directed = directed)
  stress_centrality <- snafun::v_stress(x)
  max_stress_cen <- max(stress_centrality)
  max_stress_cen
}

aus_centrality <- function(graph, directed = TRUE){
  x <- snafun::fix_cug_input(graph, directed = directed)
  stress_centrality <- snafun::v_stress(x, vids = 2)
  stress_centrality
}

# CUG Tests
P_X_greater_than_Obs <- c()
P_X_smaller_than_Obs <- c()

for (i in 1:length(years)){
  cug_test <- sna::cug.test(migration_networks_network[[i]],
                            FUN = aus_centrality,
                            mode = 'digraph',
                            cmode = 'size',
                            reps = 1500)
  P_X_smaller_than_Obs <- c(P_X_smaller_than_Obs, cug_test[["plteobs"]])
  P_X_greater_than_Obs <- c(P_X_greater_than_Obs, cug_test[["pgteobs"]])
}
max_centrality_year['P_X_greater_than_Obs'] <- P_X_greater_than_Obs
max_centrality_year['P_X_smaller_than_Obs'] <- P_X_smaller_than_Obs

cug_test <- sna::cug.test(migration_networks_network[[5]],
                          FUN = max_centrality,
                          mode = 'digraph',
                          cmode = 'edges',
                          reps = 1500)
plot(cug_test)
# Is v_stress the right method? The Shapley method can consider weights
# However, this method uses Dijkstra to find shortest paths. Dijkstra
# will find the lowest weights, which is not working in our case.

stress_test <- function(graph, directed = TRUE){
  x <- snafun::fix_cug_input(graph, directed = directed)
  snafun::v_stress(x)
}

shapley_test <- function(graph, directed = TRUE){
  x <- snafun::fix_cug_input(graph, directed = directed)
  snafun::v_shapley(x)
}

stress_cug <- sna::cug.test(migration_networks_network[[5]],
                            FUN = stress_test,
                            mode = 'digraph',
                            cmode = 'edges',
                            reps = 1500)

shapley_cug <- sna::cug.test(migration_networks_network[[5]],
                             FUN = shapley_test,
                             mode = 'digraph',
                             cmode = 'edges',
                             reps = 1500)
stress_cug
shapley_cug
igraph::E(migration_networks[[5]])$weight
weight_invert <- migration_networks[[5]]
class(weight_invert)

igraph::E(weight_invert)$weigth <- 1 / igraph::E(migration_networks[[5]])$weight 
igraph::E(weight_invert)$weigth[1]
igraph::E(migration_networks[[5]])$weight[1]
weight_invert <- snafun::to_network(weight_invert)
class(weight_invert)
sna::cug.test(weight_invert,
              FUN = shapley_test,
              mode = 'digraph',
              cmode = 'edges',
              reps = 1500)
shapley_cug

## CUG Test Manually
# Find centrality scores of Austria
aus_stress <- central_data[central_data$Country == 'Austria',]

# Count edges and vertices per year
edges <- c()
vertices <- c()

for (i in 1:length(years)){
  edges <- c(edges, snafun::count_edges(migration_networks[[i]]))
  vertices <- c(vertices, snafun::count_vertices(migration_networks[[i]]))
}

# Create manual simulation
sim_manual <- function(simulations, edges, vertices){
  sim_results <- c()
  for (i in 1:simulations){
    sim <- snafun::create_random_graph(n_vertices = vertices,
                                       strategy = "gnm",
                                       m = edges,
                                       directed = TRUE,
                                       graph = "network")
    max_stress <- mean(snafun::v_stress(sim))
    sim_results <- c(sim_results, max_stress)
  }
  sim_results
}

cug_tests <- list()
for (i in 1:length(years)){
  cug_tests[[i]] <- sim_manual(2000, edges[i], vertices[i])
}
# xlim = c(aus_stress[[i + 1]] - 10, max(density(cug_tests[[i]])[['x']] + 10)
# test <- sim_manual(2000, edges[1], vertices[1])
for (i in 1:length(cug_tests)){
  plot(density(cug_tests[[i]]), xlim = c(aus_stress[[i + 1]] - 10, max(density(cug_tests[[i]])[['x']] + 10)),
       main = 'Centrality of Austria compared to average centrality of simulations')
  abline(v = aus_stress[[i + 1]], lty = 'dashed')
  mean(cug_tests[[i]] > aus_stress[[i + 1]])
}


# plot(density(test), xlim = c(aus_stress[[2]] - 10, max(density(test)[['x']] + 10)))
# abline(v = aus_stress[[2]], lty = 'dashed')
# 
# mean(test > aus_stress[[2]])
# max(density(test)[['x']])
# density(cug_tests[[1]])
