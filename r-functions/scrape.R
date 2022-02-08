scrape <- function (x) {
          # Origin prices -----------------------------------------------------------
          
          pescado_origin <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_origen-res1.asp?T=m&T1=mr&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year[t],"&RegPag=100000&x=32&y=14")
          crustaceos_origin <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_origen-res1.asp?T=C&T1=C&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year[t],"&RegPag=100000&x=37&y=17")
          moluscos_origin <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_origen-res1.asp?T=o&T1=of&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year[t],"&RegPag=100000&x=42&y=15")
          filete_otros_origin <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_origen-res1.asp?T=f&T1=pf&T2=of&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year[t],"&RegPag=100000&x=41&y=20")
          
          #create list of websites
          weblist <- list(pescado_origin, crustaceos_origin, moluscos_origin, filete_otros_origin)
          names(weblist) <- c("pescado", "crustaceos", "moluscos", "filetes")
          
          
          # Scraping the website as a single query category
          
          scrap <- list()
          
          for (i in 1:length(weblist)) {
                    
                    scrap[[i]] <-  read_html(
                              curl::curl(weblist[[i]], 
                                         handle = curl::new_handle("useragent" = "Mozilla/5.0"))) %>% 
                              html_nodes(xpath = '/html/body/div[1]/center/table[2]') %>%
                              html_table() %>% 
                              .[[1]] %>% 
                              as.data.frame() %>% 
                              mutate(category = c("category", rep(names(weblist)[[i]], length(.$X1)-1)))
                    nytnyt()
                    
          }
          
          results_origin <- do.call(rbind, scrap)
          
          results_origin <- clean_names(row_to_names(results_origin, row_number = 1))
          results_origin <- results_origin %>% filter(
                    !str_detect(.$fecha, c("Fuente")), 
                    !str_detect(.$fecha, "Fecha")) %>% 
                    mutate_at(c("pmin", "pmax", "pfrec"), as.numeric) %>% 
                    mutate(id = 1:length(.$fecha)) %>% 
                    mutate(fecha = as.Date(fecha, "%d/%m/%Y")) %>% 
                    mutate(type = "origin")
          
          # Destiny prices ----------------------------------------------------------
          
          
          pescado_destiny <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_destino-res1.asp?T=m&T1=mr&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year[t],"&RegPag=10000&x=36&y=24")
          crustaceos_destiny <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_destino-res1.asp?T=C&T1=C&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year[t],"&RegPag=10000&x=25&y=17")
          moluscos_destiny <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_destino-res1.asp?T=o&T1=of&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year[t],"&RegPag=10000&x=50&y=9")
          filete_otros_destiny <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_destino-res1.asp?T=f&T1=pf&T2=of&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year[t],"&RegPag=10000&x=28&y=17")
          
          
          weblist <- list(pescado_destiny, crustaceos_destiny, moluscos_destiny, filete_otros_destiny)
          names(weblist) <- c("pescado", "crustaceos", "moluscos", "filetes")
          
          # Scraping the website as a single query category -------------------------------------------------------
          
          scrap <- list()
          
          for (i in 1:length(weblist)) {
                    
                    scrap[[i]] <-  read_html(curl::curl(weblist[[i]], handle = curl::new_handle("useragent" = "Mozilla/5.0"))) %>%
                              html_nodes(xpath = '/html/body/div[1]/center/table[2]') %>%
                              html_table() %>% 
                              .[[1]] %>% 
                              as.data.frame() %>% 
                              mutate(category = c("category", rep(names(weblist)[[i]], length(.$X1)-1)))
                    nytnyt()
                    
          }
          
          results_destiny <- do.call(rbind, scrap)
          
          results_destiny <- clean_names(row_to_names(results_destiny, row_number = 1))
          results_destiny <- results_destiny %>% filter(
                    !str_detect(.$fecha, c("Fuente")), 
                    !str_detect(.$fecha, "Fecha")) %>% 
                    mutate_at(c("pmin", "pmax", "pfrec"), as.numeric) %>% 
                    mutate(id = 1:length(.$fecha)) %>% 
                    mutate(fecha = as.Date(fecha, "%d/%m/%Y")) %>% 
                    mutate(type = "destiny")
          
          results <- rbind(results_origin, results_destiny)
          
          # Uploading to drive
          
          sheet_append(ss = "https://docs.google.com/spreadsheets/d/1KIjRvcUI5ujUheSj3_bbaIldbhb9Fx2aiO7KKn-FT3I/edit?usp=sharing", results)
          
          
          cat("You succesfully webscraped the shit out of SNIIM! \n Dates scraped: ", paste0(day1, "-", month,"-", year[t]), "to", paste0(day2, "-", month, "-", year[t]), "\n")
          
}
