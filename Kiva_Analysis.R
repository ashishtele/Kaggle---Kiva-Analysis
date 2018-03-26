
rm(list=ls())

##Data Retrieval

#1. Loading Libraries

load_lb <- function()
{
  library(readxl)
  library(tidyr)
  library(dplyr)
  library(caret)
  library(rpart)
  library(tree)
  library(MASS)
  require(xgboost)
  require(data.table)
  require(Matrix)
}

load_lb()

#2. Reading the data files 

Kiva_loan <- read.csv("E:\\Study\\R Projects\\Kiva\\kiva_loans.csv",header = T,sep = ",")
tbl_df(Kiva_loan)

kiva_mpi_region <- read.csv("E:\\Study\\R Projects\\Kiva\\kiva_mpi_region_locations.csv",header = T,sep = ",")
tbl_df(kiva_mpi_region)

loan_theme_ids <- read.csv("E:\\Study\\R Projects\\Kiva\\loan_theme_ids.csv",header = T,sep = ",")
tbl_df(loan_theme_ids)  

loan_theme_region <- read.csv("E:\\Study\\R Projects\\Kiva\\loan_themes_by_region.csv",header = T,sep = ",")
tbl_df(loan_theme_region)

#Additional data files

lender_data <- read.csv("E:\\Study\\R Projects\\Kiva\\lenders.csv",header = T,sep = ",")
tbl_df(lender_data)

country_stats <- read.csv("E:\\Study\\R Projects\\Kiva\\country_stats.csv",header = T,sep = ",")
tbl_df(country_stats)

MPI_national <- read.csv("E:\\Study\\R Projects\\Kiva\\MPI_national.csv",header = T,sep = ",")
tbl_df(MPI_national)

MPI_sub_national <- read.csv("E:\\Study\\R Projects\\Kiva\\MPI_subnational.csv",header = T,sep = ",")
tbl_df(MPI_sub_national)


## Data preparation



countMissing <- function(x,y) {
  ## calculate counts
  if (mode(x) == "character") emptyStrings = sum(x=="", na.rm=TRUE) else emptyStrings = 0
  if (mode(x) == "numeric") missing = sum(x=="", na.rm=TRUE) else missing = 0
  totalRows = NROW(x)
  nonMissing = totalRows - missing - emptyStrings
  
  ## present results
  cat("#          Column name: ", y, "\n")
  cat("#           TOTAL ROWS: ", totalRows, " (", percent(totalRows/NROW(x)), ")\n", sep="")
  cat("# Missing Values (NAs): ", missing, " (", percent(missing/NROW(x)), ")\n", sep="")
  cat("  # Empty Strings (\"\"): ", emptyStrings, " (", percent(emptyStrings/NROW(x)), ")\n", sep="")
  cat("   # Non-missing Value: ", nonMissing, " (", percent(nonMissing/NROW(x)), ")\n", sep="")
  cat("    Mode & Class: ", mode(x), ", ", class(x), "\n", sep="")
}

percent <- function(x, digits = 1, format = "f", ...) 
{
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}


table_info <- function(x)
{
for(i in 1:length(colnames(x)))
{
  countMissing(x[[i]],colnames(x)[i])
}}

##OR

sapply(Kiva_loan, function(x) sum(is.na(x))) 

library(mice)

init <- mice(Kiva_loan, maxit = 0)

#Checking missing data





