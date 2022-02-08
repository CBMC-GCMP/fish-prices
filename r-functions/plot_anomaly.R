plot_anomaly <- function(x) {
          require(anomalize)
          as_tibble(x) %>%
                    # Data Manipulation / Anomaly Detection
                    time_decompose(target = pfrec, method = "stl") %>%
                    anomalize(remainder, method = "iqr") %>%
                    time_recompose() %>%
                    # Anomaly Visualization
                    plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.25) +
                    labs(title = paste0(x$category, "Tidyverse Anomalies", unique(x$type)), 
                         subtitle = "STL + IQR Methods", 
                         x = "Time", y = "MXN pesos") 
}
