# Day 2

library(here)
library(tidyverse)

# Read in the file with spaces as the delimiter
d <- read_delim(here("data", "day_2_input.txt"),
                delim = " ",
                col_names = FALSE) %>%
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
         valid = count >= min & count <= max)

answer1 <- sum(d$valid)
answer1

## For part 2 we need to check that exactly 1 position contains the letter
d <- d %>%
  rowwise() %>%
  mutate(valid_2 = length(intersect(c(min, max), 
                                    gregexpr(letter, password)[[1]])) == 1)

answer2 <- sum(d$valid_2)
answer2
