
# Loading libraries -------------------------------------------------------

library(tidyverse)
library(googlesheets4)
library(anomalize)
library(bcp)
library(tidygraph)
library(ggraph)
library(visNetwork)
library(networkD3)

source("r-functions/plot_anomaly.R")
source("r-functions/eliminate_anomalies.R")
source("r-functions/price_trends.R")



# Data upload and wrangling -------------------------------------------------------------


df1 <- vroom::vroom("data/prices_origin_destiny_webscraped_sniim.csv") %>% 
          mutate(Year = lubridate::year(fecha))

# Data for inflation correction

inflcor <- readxl::read_xlsx("data/inflation_correction.xlsx") %>% 
          rename(Year = Period)

# Price wrangling and correction for inflation

prices <- merge(df1, inflcor, by = "Year", all.x = T) %>% 
          mutate(pmin = pmin * Value, 
                 pmax = pmax * Value, 
                 pfrec = pfrec * Value) 

## Plot trends with the price_trends function, you can change the name of the state or choose "All" to have a trend of all the states together
price_trends(prices, "Baja California Sur")


## Summary records for each destiny state
prices %>% 
          filter(type == "destiny") %>% 
          #filter(!origen %in% c("No especificado", "Nacional", "Global", "Local", "Origen")) %>% 
          select(fecha, origen) %>% 
          unique() %>% 
          group_by(origen) %>% 
          count() %>% 
          arrange(-n) %>% 
          View()


## Summary records for each destiny state

prices %>% 
          filter(type == "origin") %>% 
          select(fecha, origen) %>% 
          unique() %>% 
          group_by(origen) %>% 
          count() %>% 
          arrange(-n) %>% 
          View()

# General plot

prices %>% 
          filter(type == "origin") %>%
          filter(category == "pescado") %>% 
          group_by(fecha) %>% 
          summarise(price = mean(pfrec, na.rm = T),
                    price.sd = sd(pfrec, na.rm = T)/sqrt(n())) %>% 
          ggplot(aes(x = fecha, y = price)) +
          geom_point() +
          hrbrthemes::theme_ipsum() +
          labs(x = "Year", 
               y = "Mexican Pesos", 
               caption = "*Fuente: SNIIM", 
               title = "Average prices (*corrected* for inflation)", 
               color = "")  +
          theme(axis.text.x = element_text(angle = 90))

# Plotting price anomalies per category in origin markets

origin_markets <- prices %>% 
          filter(type == "origin") %>%
          group_by(fecha, category) %>% 
          summarise(pfrec = mean(pfrec, na.rm = T)) %>% 
          filter(!is.na(pfrec)) %>% 
          split(.$category)

myplots <- lapply(origin_markets, plot_anomaly)
gridExtra::grid.arrange( grobs = myplots, nrow = 2 )


# Eliminating price anomalies

origin_markets_cleaned <- lapply(origin_markets, eliminate_anomalies)

myplots <- lapply(origin_markets_cleaned, plot_anomaly)
gridExtra::grid.arrange( grobs = myplots, nrow = 2 )



# Plotting trends of cleaned data -----------------------------------------


myplots <- lapply(origin_markets_cleaned, function(x) {ggplot(x, aes(x=fecha, y=pfrec)) +
          geom_line(col = "gray40", alpha = .3)+
          geom_point(alpha =.1, col = "#2c3e50") + 
          labs(title = x$category) +
          geom_smooth(method = "gam") +
          hrbrthemes::scale_color_ipsum() +
          hrbrthemes::theme_ipsum()})
gridExtra::grid.arrange( grobs = myplots, nrow = 2 )



# A bit of Bayesian trends ------------------------------------------------

myplots <- lapply(origin_markets_cleaned, function(x) {plot(bcp(x$pfrec), main = x$category)})

gridExtra::grid.arrange( grobs = myplots, nrow = 2 )





# A bit of network --------------------------------------------------------

destiny <- prices %>% 
          filter(type == "destiny") %>% 
          #filter(Year == 2010) %>% 
          filter(!is.na(pfrec))

sources <- destiny %>% 
          select(origen) %>% 
          unique() %>% 
          rename(label = origen)
destinations <- destiny %>% 
          select(fuente) %>% 
          unique() %>% 
          rename(label = fuente)


nodes <- rbind(sources, destinations) %>% arrange(label) %>% rowid_to_column("id")

per_route <- destiny %>% 
          rename(source = origen, destination = fuente) %>% 
          group_by(source, destination) %>% 
          summarise(weight = n()) %>% 
          ungroup() %>% 
          mutate(weight = (weight/sum(weight))*100)


edges <- per_route %>% 
          left_join(nodes, by = c("source" = "label")) %>% 
          rename(from = id)

edges <- edges %>% 
          left_join(nodes, by = c("destination" = "label")) %>% 
          rename(to = id) %>% 
          mutate(weight = weight)
nodes




routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

routes_tidy %>% 
          activate(edges) %>% 
          arrange(desc(weight))

ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(routes_tidy, layout = "graphopt") + 
          geom_node_point() +
          geom_edge_link(aes(width = weight), alpha = 0.8) + 
          scale_edge_width(range = c(0.2, 2)) +
          geom_node_text(aes(label = label), repel = TRUE) +
          labs(edge_width = "Markets") +
          theme_graph()

routes_tidy %>% 
          activate(edges) %>% 
          arrange(desc(weight))

ggraph(routes_tidy, layout = "graphopt") + 
          geom_node_point() +
          geom_edge_link(aes(width = weight), alpha = 0.8) + 
          scale_edge_width(range = c(0.2, 2)) +
          geom_node_text(aes(label = label), repel = TRUE) +
          labs(edge_width = "Markets") +
          theme_graph()




visNetwork(nodes, edges)

visNetwork(nodes, edges) %>% 
          visIgraphLayout(layout = "layout_with_fr") %>% 
          visEdges(arrows = "middle")


nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)

forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Group = "id", Value = "weight", 
             opacity = 1, fontSize = 16, zoom = TRUE)

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")
