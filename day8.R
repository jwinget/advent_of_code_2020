Day <- 8

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- readLines(here("data", glue("day_{Day}_input.txt")))
test <- c("nop +0",
          "acc +1",
          "jmp +4",
          "acc +3",
          "jmp -3",
          "acc -99",
          "acc +1",
          "jmp -4",
          "acc +6")
# Functions ---------------------------------------------------------------

echo_inst <- function(d, idx, accumulator) {
  print(glue("Index: {idx}"))
  print(glue("Accumulator: {accumulator}"))
  print(glue("Instruction: {d[idx]}"))
  readline(prompt="Press [enter] to continue")
}

boot_gameboy <- function(d) {
  accumulator <- 0
  idxs <- seq(length(d))
  idx <- 1
  
  visited_indices <- c()
  
  while(!idx %in% visited_indices) {
    visited_indices <- c(visited_indices, idx)
    inst <- str_split(d[idx], "\\s")[[1]]
    if (inst[1] == "acc") {
      accumulator <- eval(parse(text = glue("{accumulator}{inst[2]}")))
      idx <- idx + 1
    } else if (inst[1] == "jmp") {
      idx <- eval(parse(text = glue("{idx}{inst[2]}")))
    } else {
      idx <- idx + 1
    }
  }
  return(accumulator)
}

# Question 1 --------------------------------------------------------------

answer1 <- boot_gameboy(d)
answer1

# Question 2 --------------------------------------------------------------

answer2
