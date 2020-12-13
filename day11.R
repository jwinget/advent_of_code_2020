Day <- 11

# This runs obscenely slowly but it's *my* code, at least

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

find_neighbors <- function(d, mode) {
  width <- nchar(d[1])
  height <- length(d)
  
  # Set up the data structure
  df <- tibble(pos = unlist(strsplit(d, "")),
               neighbors = rep(NA, sum(nchar(d)))) %>%
    mutate(neighbors = as.list(neighbors),
           pos = case_when(
             pos == "L" ~ FALSE,
             pos == "." ~ NA
           ))
  
  # Find the neighbors of each seat
  # Store their row indices as a vector in "neighbors"
  for(i in seq(1:nrow(df))){
      possible_indices <- list(N = i - width,
                            NE = i - width + 1,
                            E = i + 1,
                            SE = i + width + 1,
                            S = i + width,
                            SW = i + width - 1,
                            W = i - 1,
                            NW = i - width - 1
                            )
      
      # Deal with left and right edges
      if (i %% width == 0) {
        # End of a row
        possible_indices$E <- NULL
        possible_indices$NE <- NULL
        possible_indices$SE <- NULL
      } else if (i %% width == 1) {
        # Start of a row
        possible_indices$W <- NULL
        possible_indices$NW <- NULL
        possible_indices$SW <- NULL
      }
      
      # Deal with top and bottom edges
      if (i <= width) {
        # Top row
        possible_indices$N <- NULL
        possible_indices$NE <- NULL
        possible_indices$NW <- NULL
      } else if (i >= width * height) {
        # Bottom row
        possible_indices$S <- NULL
        possible_indices$SE <- NULL
        possible_indices$SW <- NULL
      }
      
      if (mode == "visible") {
        for (pi in names(possible_indices)) {
        j <- 2
        while (is.na(df[possible_indices[[pi]],]$pos)) {
          if (pi == "N") {
            ti <- i - width*j
            if (ti > 0 & !is.na(df[ti,]$pos)) {
              possible_indices[[pi]] <- ti
              break
            } else if (is.na(df[ti,]$pos)) {
              j <- j + 1
            } else if (ti <= 0) {
              break
            }
          } else if (pi == "NE") {
            ti = i - width*j + 1*j
            if (ti > 0 & !is.na(df[ti,]$pos)) {
              possible_indices[[pi]] <- ti
              break
            } else if (is.na(df[ti,]$pos)) {
              j <- j + 1
            } else if (ti <= 0) {
              break
            }
          } else if (pi == "E") {
            while(j <= width - i) {
            ti = i + j
            if (!is.na(df[ti,]$pos)) {
              possible_indices[[pi]] <- ti
              break
            } else { j <- j + 1}
            }
          } else if (pi == "SE") {
            ti = i + width*j + 1*j
            if (ti < width * height & !is.na(df[ti,]$pos)) {
              possible_indices[[pi]] <- ti
              break
            } else if (is.na(df[ti,]$pos)) {
              j <- j + 1
            } else if (ti >= width * height) {
              break
            }
          } else if (pi == "S") {
            ti = i + width*j
            if (ti < width * height & !is.na(df[ti,]$pos)) {
              possible_indices[[pi]] <- ti
              break
            } else if (is.na(df[ti,]$pos)) {
              j <- j + 1
            } else if (ti >= width * height) {
              break
            }
            
          } else if (pi == "SW") {
            ti = i + width*j - 1*j
            if (ti < width * height & !is.na(df[ti,]$pos)) {
              possible_indices[[pi]] <- ti
              break
            } else if (is.na(df[ti,]$pos)) {
              j <- j + 1
            } else if (ti >= width * height) {
              break
            }
          } else if (pi == "W") {
            ti = i - j
            if (!is.na(df[ti,]$pos)) {
              possible_indices[[pi]] <- ti
              break
            } else { j <- j + 1}
          }
          else if (pi == "NW") {
            ti = i - width*j - 1*j
            if (ti > 0 & !is.na(df[ti,]$pos)) {
              possible_indices[[pi]] <- ti
              break
            } else if (is.na(df[ti,]$pos)) {
              j <- j + 1
            } else if (ti <= 0) {
              break
            }
          }
        }
        }
      }
      
      
  }
  
  
  return(df)
}

update_seats <- function(nb, max_occupied) {
  out <- nb
  stable <- FALSE
  
  cache <- out
  while(stable == FALSE) {
  for (i in seq(1:nrow(cache))) {
    if(!is.na(cache[i,]$pos)) {
      occ <- sum(cache[cache[i,]$neighbors[[1]],]$pos, na.rm = T)
      if(cache[i,]$pos == TRUE) {
        # This seat is taken. See if the person wants to leave
        if (occ >= max_occupied) {
          out[i,]$pos <- FALSE
        } else {out[i,]$pos <- TRUE}
      } else if (cache[i,]$pos == FALSE) {
        # Available seat. Fill if no adjacent seats
        if(occ == 0) {
          out[i,]$pos <- TRUE
        } else {out[i,]$pos <- FALSE}
      }
    }
  }
    if (all.equal(out$pos, cache$pos) == TRUE) {
      stable == TRUE
      return(sum(out$pos, na.rm = T))
    } else {
      print(sum(out$pos, na.rm = T))
      cache <- out
    }
  }
}

# Question 1 --------------------------------------------------------------
nb <- find_neighbors(d, mode = "adjacent")
answer1 <- update_seats(nb, 4)
answer1

# Question 2 --------------------------------------------------------------
nb <- find_neighbors(d, mode = "visible")
answer2 <- update_seats(nb, 5)
answer2