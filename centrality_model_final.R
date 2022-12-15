library(gridExtra)

## Load cleaned data
load('migration_networks.RData')

## Centrality Analysis Function
centrality_analysis <- function(igraph_graphs, years){
  countries <- igraph::get.vertex.attribute(igraph_graphs[[1]], 'name')
  central_data <- data.frame(countries)
  colnames(central_data) <- 'Country'
  
  for (index in 1:length(years)){
    central_data[sprintf('Centrality_%s', years[index])] <- snafun::v_stress(igraph_graphs[[index]])
  }
  central_data
}

## Centrality Analysis
years <- c(1960, 1970, 1980, 1990, 2000)
central_data <- centrality_analysis(migration_networks, years)

## Centrality Plots

### 1960
color <- rep('red', snafun::count_vertices(migration_networks[[1]]))
austria_centrality <- central_data[central_data['Country'] == 'Austria', 'Centrality_1960']
central_countries_1960 <- which(central_data['Centrality_1960'] >= austria_centrality)
color[central_countries_1960] <- 'green'

snafun::plot(migration_networks[[1]],
             vertex.shape = 'circle',
             vertex.color = color,
             vertex.size = sqrt(central_data[['Centrality_1960']]),
             edge.width = igraph::E(migration_networks[[1]])$weight.scaled*15,
             edge.arrow.size = 0.2,
             vertex.label.color = 'black',
             main = 'Stress centrality per country \n in 1960',
             layout = igraph::layout_in_circle,
             label.cex = 0.6)

graphics::legend(x = -1.5, y = 0, c('>= Austria', '< Austria'), pch = 21, col = "#777777", pt.bg = c('green', 'red'), pt.cex = 2, cex = .8, bty = "n", ncol = 1)

### 1970
color <- rep('red', snafun::count_vertices(migration_networks[[2]]))
austria_centrality <- central_data[central_data['Country'] == 'Austria', 'Centrality_1970']
central_countries_1970 <- which(central_data['Centrality_1970'] >= austria_centrality)
color[central_countries_1970] <- 'green'

snafun::plot(migration_networks[[2]],
             vertex.shape = 'circle',
             vertex.color = color,
             vertex.size = sqrt(central_data[['Centrality_1970']]),
             edge.width = igraph::E(migration_networks[[2]])$weight.scaled*15,
             edge.arrow.size = 0.2,
             vertex.label.color = 'black',
             main = 'Stress centrality per country \n in 1970',
             layout = igraph::layout_in_circle)

graphics::legend(x = -1.5, y = 0, c('>= Austria', '< Austria'), pch = 21, col = "#777777", pt.bg = c('green', 'red'), pt.cex = 2, cex = .8, bty = "n", ncol = 1)

### 1980
color <- rep('red', snafun::count_vertices(migration_networks[[3]]))
austria_centrality <- central_data[central_data['Country'] == 'Austria', 'Centrality_1980']
central_countries_1970 <- which(central_data['Centrality_1980'] >= austria_centrality)
color[central_countries_1970] <- 'green'

snafun::plot(migration_networks[[3]],
             vertex.shape = 'circle',
             vertex.color = color,
             vertex.size = sqrt(central_data[['Centrality_1980']]),
             edge.width = igraph::E(migration_networks[[3]])$weight.scaled*15,
             edge.arrow.size = 0.2,
             vertex.label.color = 'black',
             main = 'Stress centrality per country \n in 1980',
             layout = igraph::layout_in_circle)

graphics::legend(x = -1.5, y = 0, c('>= Austria', '< Austria'), pch = 21, col = "#777777", pt.bg = c('green', 'red'), pt.cex = 2, cex = .8, bty = "n", ncol = 1)

### 1990

color <- rep('red', snafun::count_vertices(migration_networks[[4]]))
austria_centrality <- central_data[central_data['Country'] == 'Austria', 'Centrality_1990']
central_countries_1970 <- which(central_data['Centrality_1990'] >= austria_centrality)
color[central_countries_1970] <- 'green'

snafun::plot(migration_networks[[4]],
             vertex.shape = 'circle',
             vertex.color = color,
             vertex.size = sqrt(central_data[['Centrality_1990']]),
             edge.width = igraph::E(migration_networks[[4]])$weight.scaled*15,
             edge.arrow.size = 0.2,
             vertex.label.color = 'black',
             main = 'Stress centrality per country \n in 1990',
             layout = igraph::layout_in_circle)

graphics::legend(x = -1.5, y = 0, c('>= Austria', '< Austria'), pch = 21, col = "#777777", pt.bg = c('green', 'red'), pt.cex = 2, cex = .8, bty = "n", ncol = 1)

### 2000

color <- rep('red', snafun::count_vertices(migration_networks[[5]]))
austria_centrality <- central_data[central_data['Country'] == 'Austria', 'Centrality_2000']
central_countries_1970 <- which(central_data['Centrality_2000'] >= austria_centrality)
color[central_countries_1970] <- 'green'

snafun::plot(migration_networks[[5]],
             vertex.shape = 'circle',
             vertex.color = color,
             vertex.size = sqrt(central_data[['Centrality_2000']]),
             edge.width = igraph::E(migration_networks[[5]])$weight.scaled*15,
             edge.arrow.size = 0.2,
             vertex.label.color = 'black',
             main = 'Stress centrality per country \n in 2000',
             layout = igraph::layout_in_circle)

graphics::legend(x = -1.5, y = 0, c('>= Austria', '< Austria'), pch = 21, col = "#777777", pt.bg = c('green', 'red'), pt.cex = 2, cex = .8, bty = "n", ncol = 1)

## CUG Test Manually

### Find centrality scores of Austria
aus_stress <- central_data[central_data$Country == 'Austria',]

### Count edges and vertices per year
edges <- c()
vertices <- c()

for (i in 1:length(years)){
  edges <- c(edges, snafun::count_edges(migration_networks[[i]]))
  vertices <- c(vertices, snafun::count_vertices(migration_networks[[i]]))
}

### Create manual simulation
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

### Plot Results
for (i in 1:length(cug_tests)){
  plot(density(cug_tests[[i]]), xlim = c(aus_stress[[i + 1]] - 10, max(density(cug_tests[[i]])[['x']] + 10)),
       main = 'Centrality of Austria compared to average centrality of simulations')
  abline(v = aus_stress[[i + 1]], lty = 'dashed')
  mean(cug_tests[[i]] > aus_stress[[i + 1]])
}

### Store Results
save(cug_tests, file = 'cug_tests.RData')

