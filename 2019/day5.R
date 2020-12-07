Day <- 5

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- readLines(here("2019", "data", glue("day_{Day}_input.txt")))
d <- strsplit(d, ",")[[1]] %>% 
  as.numeric() %>% as_tibble() %>%
  rownames_to_column(var = "idx") %>%
  mutate(idx = as.numeric(idx) - 1)

# Functions ---------------------------------------------------------------
# This is the hardest program I've written

parse_instruction <- function(code) {
  code <- sprintf("%05d", code)
  opcode <- as.numeric(stringi::stri_sub(code, 
                              from = nchar(code) - 1, 
                              to = nchar(code)))
  
  modes <- rev(as.numeric(strsplit(stringi::stri_sub(code, 
                             from = 1, 
                             to = 3), "")[[1]]))
  
  return(list(oc = opcode,
       m = modes))
}

modal_read <- function(d, mode, addr) {
  v <- pull(filter(d, idx == addr))
  
  if (mode == 1) {
    return(v)
  } else {
    return(pull(filter(d, 
                       idx == v),
                value))
  }
}

execute_instruction <- function(d, inst, idx) {
  
  param_1_v <- modal_read(d, inst$m[1], idx + 1)
  param_2_v <- modal_read(d, inst$m[2], idx + 2)
  
  num_params <- case_when(
    inst$oc %in% c(1, 2) ~ 3,
    inst$oc %in% c(3, 4) ~ 1,
    TRUE ~ 5000
  )
  
  write_addr <- pull(d[d$idx == idx + num_params,], value)
  
  if (inst$oc == 99) {
    print("Exiting...")
    return("stop")
    break
  } else if (inst$oc == 1) {
    # Add
    d[d$idx == write_addr,] <- param_1_v + param_2_v
  } else if (inst$oc == 2) {
    # Multiply
    d[d$idx == write_addr,] <- param_1_v * param_2_v
  } else if (inst$oc == 3) {
    # Prompt for input
    in_val <- as.numeric(readline(prompt="Enter input: "))
    d[d$idx == write_addr,] <- in_val
  } else if (inst$oc == 4) {
    # Print result
    readline(prompt="Press [enter] to continue")
  }
  
  # Return the next opcode
  next_code <- pull(d[d$idx == idx + num_params + 1,], value)
  print(next_code)
  readline(prompt="Press [enter] to continue")
  return(next_code)
}

ic <- function(d) {
  idx <- pull(d[1,], idx)
  code <- pull(d[1,], value)
  
  while(code != "stop") {
    inst <- parse_instruction(code)
    code <- execute_instruction(d, inst, idx)
  }
}

# Question 1 --------------------------------------------------------------

answer1

# Question 2 --------------------------------------------------------------

answer2