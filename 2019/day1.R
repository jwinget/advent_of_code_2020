Day <- 1

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- readLines(here("2019", "data", glue("day_{Day}_input.txt")))

# Functions ---------------------------------------------------------------
# to find the fuel required for a module,
# take its mass, divide by three, round down, and subtract 2

calculate_fuel <- function(d) {
  res <- d %>%
    as.numeric() %>%
    as_tibble() %>%
    mutate(fuel = floor((value / 3)) - 2)
  
  return(sum(res$fuel))
}

fuel_fuel <- function(mass) {
  recursive_fuel <- floor(mass / 3) - 2
  fuel <- recursive_fuel
  fuel_vec <- c(fuel)
  while(recursive_fuel > 0){
    recursive_fuel <- floor(recursive_fuel/3) -2
    fuel_vec <- c(fuel_vec, recursive_fuel)
  }
  
  fuel <- sum(fuel_vec[which(fuel_vec > 0)])
  return(fuel)
}
calculate_fuel_recursive <- function(d) {
  res <- d %>%
    as.numeric() %>%
    as_tibble() %>%
    rowwise() %>%
    mutate(fuel = fuel_fuel(value))
  
  return(sum(res$fuel))
}

# Question 1 --------------------------------------------------------------
answer1 <- calculate_fuel(d)
answer1

# Question 2 --------------------------------------------------------------
answer2 <- calculate_fuel_recursive(d)
answer2