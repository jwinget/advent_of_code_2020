Day <- 4

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)
library(stringr)

# Functions ---------------------------------------------------------------

#' Parse data
#' 
#' Given passport data,
#' Split the entries and parse key:value pairs
#' 
#' @input d Advent of Code 2020 day 4 input
#' @output A list of passports, each as a data frame with "key" and "value" columns
#' 
#' @example passports <- parse_data(input_file)
parse_data <- function(input_file) {
  # Read in the input
  d <- readLines(here("data", glue("day_{Day}_input.txt")))
  
  # keys are 3 characters followed by a colon,
  # values immediately follow
  # Records are separated by blank line
  # Sometimes have multiple k:v pairs on same line
  
  passports <- list()
  i = 1
  df <- tibble(k = character(), v = character())
  
  for(line in d) {
    if(nchar(line) == 0) {
      # New entry. Assign current item to output list and reset
      passports[[i]] <- df
      i <- i + 1
      df <- tibble(k = character(), v = character())
    } else {
    # Split a vector into k:v
    ldf <- str_split(line, ":|\\s") %>%
      unlist() %>%
      as_tibble() %>%
      mutate(variable = rep(c("k", "v"), nrow(.)/2),
                            key = rep(1:(nrow(.)/2),
                            each = 2)) %>%
      pivot_wider(id_cols = key, names_from = variable, values_from = value) %>%
      select(-key)
    df <- bind_rows(df, ldf)
    }
  }
# Write the last passport
passports[[i]] <- df 

return(passports)
}

#' Check passports to see that they have required fields
#' 
#' Given a passport in a dataframe with columns "k" and "v"
#' Check that the passport is has all required fields
#' 
#' @input passports A list of passport data frames
#' @input required_fields A vector of required fields
#' 
#' @export
check_passports <- function(passports, required_fields) {
  check_passport <- function(passport, required_fields) {
    return(length(setdiff(required_fields, passport$k)))
  }
  
  missing_fields <- map(passports, ~check_passport(., required_fields)) %>%
    unlist()
  
  return(length(missing_fields[missing_fields == 0]))
}

#' Validate passport data
#' 
#' Given a passport in a dataframe with columns "k" and "v"
#' Check that the passport is has all required fields
#' AND that the data is valid
#' 
#' @input passports A list of passport data frames
#' @input required_fields A vector of required fields
#' 
#' @export
validate_passports <- function(passports, required_fields) {
  check_passport <- function(passport, required_fields) {
    missing_fields <- length(setdiff(required_fields, passport$k))
    
    # Now check for valid data
    inv <- passport %>%
      rowwise() %>%
      mutate(invalid = case_when(
        k == "byr" & str_detect(v, "[0-9]{4}") & between(as.numeric(v), 1920, 2002) ~ FALSE,
        k == "iyr" & str_detect(v, "[0-9]{4}") & between(as.numeric(v), 2010, 2020) ~ FALSE,
        k == "eyr" & str_detect(v, "[0-9]{4}") & between(as.numeric(v), 2020, 2030) ~ FALSE,
        # This might be the ugliest piece of code I've ever written
        k == "hgt" & between(as.numeric(str_split(v, pattern = "cm")[[1]][1]), 150, 193) | between(as.numeric(str_split(v, pattern = "in")[[1]][1]), 59, 76) ~ FALSE, 
        k == "hcl" & str_detect(v, "^#[0-9|a-z]{6}$") ~ FALSE,
        k == "ecl" & v %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth") ~ FALSE,
        k == "pid" & str_detect(v, "^[0-9]{9}$") ~ FALSE,
        k == "cid" ~ FALSE,
        TRUE ~ TRUE
      ))
    
    if(sum(inv$invalid) > 0 | missing_fields > 0) {
      return(1)
    } else {
      return(0)
    }
  }
  
  invalid_passport <- map(passports, ~check_passport(., required_fields)) %>%
    unlist()
  
  return(length(invalid_passport[invalid_passport == 0]))
}

# Read data ---------------------------------------------------------------

d <- parse_data(here("data", glue("day_{Day}_input.txt")))

# Question 1 --------------------------------------------------------------

required_fields <- c("byr", "iyr", "eyr", "hcl",
                     "hgt", "ecl", "pid")

check_passports(d, required_fields)

# Question 2 --------------------------------------------------------------
validate_passports(d, required_fields)