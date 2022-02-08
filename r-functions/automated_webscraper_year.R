options(
          # whenever there is one account token found, use the cached token
          gargle_oauth_email = TRUE,
          # specify auth tokens should be stored in a hidden directory ".secrets"
          gargle_oauth_cache = ".secrets",
          warn=-1
)

source("nytnyt.R")

# Loading libraries -------------------------------------------------------

library(rvest)
library(janitor)
library(stringr)
library(tidyverse)
library(googlesheets4)

source("r-functions/scrape_local.R")

# Input days --------------------------------------------------------------

day1 <- 01
day2 <- 30
month <- 06
year <- c(2013:2021)




for (t in 1:length(year)) {
       scrape_local(x)
          
}







