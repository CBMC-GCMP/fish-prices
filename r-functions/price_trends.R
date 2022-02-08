price_trends <- function(x, origin = "All") {
          if (origin == "All") {
                    prices <- x %>%
                              #filter(origen == origin) %>%
                              group_by(fecha, type, category) %>%
                              summarise(pfrec = mean(pfrec, na.rm = T)) %>%
                              filter(!is.na(pfrec)) %>%
                              group_by(type, category) %>%
                              group_split()
                    
                    prices_cleaned <-
                              lapply(prices, eliminate_anomalies)
                    
                    prices_cleaned <- do.call(rbind, prices_cleaned)
                    
                    p1 <- ggplot(prices_cleaned, aes(x = fecha, y = trend)) +
                              geom_point(aes(
                                        x = fecha,
                                        y = trend,
                                        col = type
                              )) +
                              facet_wrap( ~ category, scales = "free_y") +
                              hrbrthemes::scale_color_ipsum() +
                              hrbrthemes::theme_ipsum() +
                              labs(
                                        x = "Date",
                                        y = "MXN pesos**",
                                        caption = "**MXN pesos are corrected for inflation \n *Fuente: Sistema Nacional de Información e Integración de Mercados",
                                        title = paste0("Average prices in Mexican markets"),
                                        subtitle = "Data are sourced from the SNIIM*",
                                        color = ""
                              ) +
                              theme(
                                        legend.position = "bottom",
                                        panel.background = element_rect(fill = "white"),
                                        plot.background = element_rect()
                              )
                    print(p1)
                    
                    cat("you chose All!\n")
          
                    
                    
          } else {
                    prices <- x %>%
                              filter(origen == origin) %>%
                              group_by(fecha, type, category) %>%
                              summarise(pfrec = mean(pfrec, na.rm = T)) %>%
                              filter(!is.na(pfrec)) %>%
                              group_by(type, category) %>%
                              group_split()
                    
                    prices_cleaned <-
                              lapply(prices, eliminate_anomalies)
                    
                    prices_cleaned <-
                              do.call(rbind, prices_cleaned)
                    
                    p2 <- ggplot(prices_cleaned, aes(x = fecha, y = trend)) +
                              geom_point(aes(
                                        x = fecha,
                                        y = trend,
                                        col = type
                              )) +
                              facet_wrap( ~ category, scales = "free_y") +
                              hrbrthemes::scale_color_ipsum() +
                              hrbrthemes::theme_ipsum() +
                              labs(
                                        x = "Date",
                                        y = "MXN pesos**",
                                        caption = "**MXN pesos are corrected for inflation \n *Fuente: Sistema Nacional de Información e Integración de Mercados",
                                        title = paste0("Average prices in ", origin, " markets"),
                                        subtitle = "Data are sourced from the SNIIM*",
                                        color = ""
                              ) +
                              theme(
                                        legend.position = "bottom",
                                        panel.background = element_rect(fill =
                                                                                  "white"),
                                        plot.background = element_rect()
                              )
                    
                    print(p2)
                    cat(paste0("you chose!",origin,"\n"))
                    
          }
          
}

