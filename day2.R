Day <- 2

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- read_delim(here("data", "day_2_input.txt"),
                delim = " ",
                col_names = FALSE)

# Data cleaning and calculations ------------------------------------------
d <- d %>%
  # Set the column names
  rename("letter" = X2,
         "password" = X3) %>%
  # Data cleanup
  separate(X1, into = c('min', 'max')) %>%
  mutate(min = as.numeric(min),
         max = as.numeric(max)) %>%
  mutate(letter = str_remove(letter, ":"),
         # Add the count of the key letter
         count = str_count(password, letter),
         # Test for validity
         valid_1 = count >= min & count <= max) %>%
  rowwise() %>%
  mutate(valid_2 = length(intersect(c(min, max), 
                                    gregexpr(letter, password)[[1]])) == 1)

# Question 1 --------------------------------------------------------------
answer1 <- sum(d$valid_1)
answer1

# Question 2 --------------------------------------------------------------
answer2 <- sum(d$valid_2)
answer2
