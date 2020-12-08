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
  successful_termination <- FALSE
  
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
    if(idx > length(d)) {
      successful_termination <- TRUE
      break
    }
  }
  return(list(acc = accumulator,
              term = successful_termination))
}

brute_force <- function(d) {
  # Replace a single instruction and test
  drows <- seq(length(d))
  found_fix <- FALSE
  
  orig_d <- d
  
  while(found_fix == FALSE) {
    for(drow in drows) {
    df <- orig_d
    #readline(prompt = "Press [return] to continue")
    line <- df[drow]
    if (substr(line, 1, 3) == "jmp") {
      print(glue("Replacing {line} with nop at address {drow}"))
      df[drow] <- str_replace(line, "jmp", "nop")
    } else if (substr(line, 1, 3) == "nop") {
      print(glue("Replacing {line} with jmp at address {drow}"))
      df[drow] <- str_replace(line, "nop", "jmp")
    }
    res <- boot_gameboy(df)
    found_fix <- res$term

    if (found_fix == TRUE) { break }
    print(glue("Accumulator: {res$acc}"))
    print(glue("Fixed: {found_fix}"))
    }
  }
  return(res$acc)
}

# Question 1 --------------------------------------------------------------

answer1 <- boot_gameboy(d)$acc
answer1

# Question 2 --------------------------------------------------------------
answer2 <- brute_force(d)
answer2
