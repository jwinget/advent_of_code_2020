Day <- 10

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- readLines(here("data", glue("day_{Day}_input.txt"))) %>%
  as.numeric()

test <- c(16,10,15,5,1,11,7,19,6,12,4)
test2 <- c(28,33,18,42,31,14,46,20,48,
           47,24,23,49,45,19,38,39,11,
           1,32,25,35,8,17,7,9,4,2,34,
           10,3)

# Functions ---------------------------------------------------------------

chain_adapters <- function(d) {
  difftab <- table(diff(sort(d)))
  ones <- as.numeric(difftab[names(difftab) == 1]) + 1
  threes <- as.numeric(difftab[names(difftab) == 3]) + 1
  
  return(ones * threes)
}

# Question 1 --------------------------------------------------------------
answer1 <- chain_adapters(d)
answer1

# Question 2 --------------------------------------------------------------

answer2