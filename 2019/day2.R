Day <- 2

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- readLines(here("2019", "data", glue("day_{Day}_input.txt"))) %>%
  str_split(",") %>%
  unlist() %>%
  as.numeric()

# Functions ---------------------------------------------------------------
run_opcode <- function(d) {
  mod <- d
  i <- 1
  stop <- FALSE
  
  while(!stop){
  if (mod[i] == 99) {
    stop <- TRUE
    break
  } else if (mod[i] == 1) {
    res <- mod[eval(mod[i+1] + 1)] + mod[eval(mod[i+2] + 1)]
    mod[eval(mod[i+3] + 1)] <- res
    i <- i + 4
  } else if (mod[i] == 2) {
    res <- mod[eval(mod[i+1] + 1)] * mod[eval(mod[i+2] + 1)]
    mod[eval(mod[i+3] + 1)] <- res
    i <- i + 4
  } else {
    print(glue("Stuck at position {i} with value {mod[i]}"))
  }
  }
  
  return(mod[1])
}

optimize_opcode <- function(d) {
  mod <- d
  solution <- 19690720
  nouns <- seq(from = 0, to = 99)
  verbs <- seq(from = 0, to = 99)
  
  for(noun in nouns) {
    for(verb in verbs){
      mod[2] <- noun
      mod[3] <- verb
      res <- run_opcode(mod)
      
      if(res == solution) {
        print(glue("Found solution with {noun}, {verb}"))
        return((100 * noun) + verb)
        break
      } else(
        # Reset memory
        mod <- d
      )
    }
  }
  return(FALSE)
}

# Question 1 --------------------------------------------------------------
# before running the program, 
# replace position 1 with the value 12 and replace position 2 with the value 2
# Note: we offset because R uses 1-indexed vectors
d[2] <- 12 ; d[3] <- 2

answer1 <- run_opcode(d)
answer1
# 4462686

# Question 2 --------------------------------------------------------------
answer2 <- optimize_opcode(d)
answer2
