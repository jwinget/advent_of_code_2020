Day <- 10

# Load libraries ----------------------------------------------------------

library(here)
library(glue)
library(tidyverse)
library(igraph)

# Read data ---------------------------------------------------------------

d <- readLines(here("data", glue("day_{Day}_input.txt"))) %>%
  as.numeric()

test <- c(16,10,15,5,1,11,7,19,6,12,4)
test2 <- c(28,33,18,42,31,14,46,20,48,
           47,24,23,49,45,19,38,39,11,
           1,32,25,35,8,17,7,9,4,2,34,
           10,3)

# Functions ---------------------------------------------------------------

chain_adapters <- function(d) {
  difftab <- table(diff(sort(d)))
  ones <- as.numeric(difftab[names(difftab) == 1]) + 1
  threes <- as.numeric(difftab[names(difftab) == 3]) + 1
  
  return(ones * threes)
}

make_graph <- function(d) {
  # Create a graph with each adapter as a node
  # Edges are directed and only connect increasing jolt ratings
  in_jolts <- 0
  out_jolts <- max(d) + 3
  nodes <- sort(c(in_jolts, d, out_jolts))
  
  gdf <- expand.grid(nodes, nodes) %>%
    filter(Var2 > Var1) %>%
    mutate(diff = Var2 - Var1) %>%
    filter(diff <= 3) %>%
    unique()
  
  g <- graph_from_data_frame(gdf)
  
  # Set the edge weights as the differences
  E(g)$weight <- gdf$diff
  
  return(g)
}

arrange_adapters <- function(g) {
  
  tibble(node = names(V(g))) %>%
    rowwise() %>%
    mutate(out_paths = length(neighbors(g, node, mode = "out")),
           in_paths = length(neighbors(g, node, mode = "in"))) %>%
    filter(sum(out_paths, in_paths) > 2) # anything else just has 1 route
  
  #return(sum(multinodes$out_paths, 1))
}


test_arrangements <- function(d, target) {
  if (arrange_adapters(d) == target) {
    return(TRUE)
  }
  return(FALSE)
}


# I cheated and copied this to get part 2 :(
# just to get a correct answer to check my work against
adapter_combos = function(df) {
  dfx = df %>% 
    rbind(tibble(jolts = 0)) %>% 
    arrange(jolts) %>% 
    mutate(ttl = if_else(jolts == 0, 1, 0))
  
  for (i in seq(2, nrow(dfx))) {
    j = dfx[[i,'jolts']]
    
    ttl = dfx %>% 
      filter(jolts < j,
             jolts > j - 4) %>% 
      pull(ttl) %>% 
      sum()
    
    if (i == nrow(dfx)) {return(ttl)}
    
    dfx[[i,'ttl']] = ttl
    
  }
}

# Question 1 --------------------------------------------------------------
answer1 <- chain_adapters(d)
answer1

# Question 2 --------------------------------------------------------------
answer2 <- adapter_combos(tibble(jolts = d))
answer2
# 5289227976704