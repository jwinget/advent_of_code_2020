Day <- 11

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)

# Read data ---------------------------------------------------------------

d <- readLines(here("data", glue("day_{Day}_input.txt")))
dt <- c("L.LL.LL.LL", "LLLLLLL.LL", "L.L.L..L..", "LLLL.LL.LL",
        "L.LL.LL.LL", "L.LLLLL.LL", "..L.L.....", "LLLLLLLLLL",
        "L.LLLLLL.L", "L.LLLLL.LL")

# Functions ---------------------------------------------------------------

preprocess_data <- function(d) {
  # Convert the input data to a matrix
  # FALSE indicates occupancy
  dm <- unlist(map(d, ~strsplit(., "")[[1]]))
  dm[dm == "L"] <- FALSE
  dm[dm == "."] <- NA
  
  return(matrix(as.logical(dm), ncol = nchar(d[1]), byrow = TRUE))
}

fill_seats <- function(m) {
  # Rules:
  # If a seat is empty 
  # and there are no occupied seats adjacent to it, 
  # the seat becomes occupied.
  # If a seat is occupied
  # and four or more seats adjacent to it are also occupied, 
  # the seat becomes empty.
  # Otherwise, the seat's state does not change.
  # Adjacency: one of the eight positions immediately 
  # up, down, left, right, or diagonal from the seat
  
  # After preprocessing:
  # FALSE = empty seat
  # TRUE = filled seat
  # NA = floor
  
  empty_seats <- which(m == FALSE)
  occupied_seats <- which(m == TRUE)
  
  n <- nrow(m)
  mat.pad <- rbind(NA, cbind(NA, m, NA), NA)
  ind <- 2:(n + 1) # row/column indices of the "middle"
  neigh <- rbind(N  = as.vector(mat.pad[ind - 1, ind    ]),
                 NE = as.vector(mat.pad[ind - 1, ind + 1]),
                 E  = as.vector(mat.pad[ind    , ind + 1]),
                 SE = as.vector(mat.pad[ind + 1, ind + 1]),
                 S  = as.vector(mat.pad[ind + 1, ind    ]),
                 SW = as.vector(mat.pad[ind + 1, ind - 1]),
                 W  = as.vector(mat.pad[ind    , ind - 1]),
                 NW = as.vector(mat.pad[ind - 1, ind - 1]))
  
  # Update empty seats
  for (es in empty_seats) {
    adj_occ <- sum(neigh[,es], na.rm = T)
    if (adj_occ < 1) {
      m[es] <- TRUE
    }
  }
  
  # Update occupied seats
  for (os in occupied_seats) {
    adj_occ <- sum(neigh[,os], na.rm = T)
    if (adj_occ >= 4) {
      m[os] <- FALSE
    }
  }
  return(m)
}

stabilize_seating <- function(m) {
  stable <- FALSE
  state <- m
  while (stable == FALSE) {
    new_state <- fill_seats(state)
    if (all.equal(state, new_state) == TRUE) {
      stable <- TRUE
      occupied_seats <- sum(as.vector(new_state), na.rm = T)
      return(occupied_seats)
    } else {
      state <- new_state
      #print(new_state)
      #readline("Not yet stable. Press [Enter] to continue...")
    }
  }
}


# Question 1 --------------------------------------------------------------
dm <- preprocess_data(d)
answer1 <- stabilize_seating(cbind(dm, NA)) # Pad input to make it square
answer1

# Question 2 --------------------------------------------------------------

answer2