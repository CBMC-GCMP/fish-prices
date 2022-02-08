eliminate_anomalies <- function(x) {
          as_tibble(x)  %>%
                    time_decompose(pfrec, merge = T) %>%
                    anomalize(remainder) %>%
                    # Function to clean & repair anomalous data
                    clean_anomalies() %>% 
                    filter(anomaly == "No") 
}
