Day <- 13

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)
library(numbers)
library(Rmpfr)

# Read data ---------------------------------------------------------------

d <- readLines(here("data", glue("day_{Day}_input.txt")))

# Functions ---------------------------------------------------------------
generate_departures <- function(d) {
  leave_time <- as.numeric(d[1])
  buses <- as.numeric(strsplit(d[2],",")[[1]][which(strsplit(d[2],",")[[1]] != "x")])
  
  max_leave <- leave_time + max(buses*2)
  
  # Find the closest leave time
  res <- buses %>%
    as_tibble() %>%
    rowwise() %>%
    mutate("leave_times" = list(seq(from = 0, to = max_leave, by = value))) %>%
    unnest(cols = c(leave_times)) %>%
    mutate(leave_diff = leave_times - leave_time) %>%
    filter(leave_diff > 0) %>%
    top_n(-1, wt = leave_diff)
  
  return(res$value * res$leave_diff)
}

win_contest <- function(sched) {
  df <- strsplit(sched, ",")[[1]] %>%
    as_tibble() %>%
    rownames_to_column(var = "idx") %>%
    mutate(idx = as.numeric(idx) - 1) %>%
    filter(value != "x") %>%
    mutate(value = as.numeric(value)) %>%
    mutate(cum_start = cumsum(idx))
  
  # The solution will start at some multiple of 29
  # Next bit: (start + 23) %% 37 = 0
  # Then: (start + 23 + 29) %% 631 = 0
  # etc.
  # (Start time + the cumulative sum of idx) modulo value
  # must equal zero for all buses
  time <- 100000000000001
  solution <- FALSE
  step <- 29
  solved <- c(29)
  d <- filter(df, value > 29)
  
  while(solution == FALSE) {
    d <- d %>%
      rowwise() %>%
      mutate(test = as.numeric((mpfr(as.character(time), 128) + idx) %% value))
    
    if (0 %in% d$test) {
      # Found a solution for two buses
      # Switch to increment by the LCM
      bus <- d %>% filter(test == 0) %>% pull(value)
      solved <- c(solved, bus)
      step <- mLCM(solved)
      print(glue("Solved bus {bus}. Step is now {step}"))
    }
    if (sum(d$test) == 0) {
      solution <- TRUE
      print(d)
      return(time)
    } else {
      #print(glue("{time} result: {sum(d$test)}"))
      time <- time + step
    }
  }
}

# Question 1 --------------------------------------------------------------
answer1 <- generate_departures(d)
answer1

# Question 2 --------------------------------------------------------------
answer2 <- win_contest(d[2])
answer2
# Should be higher than 100000000000000
# 100000898645998 too low
# 100005680372855 too low