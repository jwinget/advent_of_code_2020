Day <- 9

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- readLines(here("data", glue("day_{Day}_input.txt"))) %>%
  as.numeric()

test <- c(35,20,15,25,47,40,62,55,65,95,102,117,
          150,182,127,219,299,277,309,576)
# Functions ---------------------------------------------------------------

comb_subset_sum = function(x, vec){
  return(sum(vec[x]))
}

find_invalid <- function(d, pre_length = 25) {
  for (i in seq(d)) {
    if (i > pre_length){
      start <- i - pre_length
      stop <- i - 1
      
      vec <- d[start:stop]
      sums = combn(pre_length, 
                   m = 2, 
                   FUN = comb_subset_sum, 
                   vec = vec)
      if (!d[i] %in% sums) {
        return(d[i])
        break
      }
    }
  }
}

find_contiguous <- function(d, target) {
  res <- FALSE
  n <- 2
  while(res == FALSE) {
    test <- expand.grid(rep(list(d), n))
    sums <- rowSums(test)
    print(glue("N: {n}"))
  if(target %in% sums) {
    idx <- match(target, sums)
    vals <- test[idx,]
    print(glue(paste(vals, collapse = " ")))
    return(min(vals) + max(vals))
    break
  } else {
    n <- n + 1
  }
  }
}

# Question 1 --------------------------------------------------------------
answer1 <- find_invalid(d)
answer1

# Question 2 --------------------------------------------------------------
answer2 <- find_contiguous(d, target = answer1)
answer2