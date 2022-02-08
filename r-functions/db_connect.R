
# Loading libraries -------------------------------------------------------

## Install necessary packages, then execute:

library(dplyr)
library(RMySQL)
library(odbc)
library(tidyverse)


# Establish connection to server db ---------------------------------------

## Connection details

# Important: Do not modify
# Requires user and password manual input
host= "conapesca-catch-database.chax18bbz6pb.us-east-2.rds.amazonaws.com"
port = 3306
dbname = "conapesca-catch-database"
.user = rstudioapi::askForPassword("User")
.password= rstudioapi::askForPassword("Database password")

