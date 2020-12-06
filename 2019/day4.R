Day <- 4

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

#d <- readLines(here("data", glue("day_{Day}_input.txt")))
candidates <- seq(from = 236491, to = 713787)

# Functions ---------------------------------------------------------------

check_passwords <- function(candidates) {
  candidates %>%
    as_tibble() %>%
    mutate(rule_1 = str_detect(value, "(\\d)\\1"),
           rule_2 = str_detect(value, "^(?=\\d{6}$)0*1*2*3*4*5*6*7*8*9*$"),
           rule_3 = str_detect(value, "(?:^|(.)(?!\\1))(\\d)\\2(?!\\2)"),
           valid_1 = rule_1 + rule_2 == 2,
           valid_2 = rule_2 + rule_3 == 2)
}

# Question 1 --------------------------------------------------------------

pw_check <- check_passwords(candidates)
answer1 <- nrow(filter(pw_check, valid_1 == TRUE))
answer1

# Question 2 --------------------------------------------------------------
answer2 <- nrow(filter(pw_check, valid_2 == TRUE))
answer2
