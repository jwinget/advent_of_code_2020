# Day 1

library(here)
library(readr)
library(tidyr)
library(dplyr)

d <- read_tsv(here("data", "day_1_input.txt"),
              col_names = FALSE) %>%
  rename("expenses" = X1)

# Find the ones that sum to 2020
# From https://stackoverflow.com/questions/46836918/sum-all-possible-combinations-of-2-or-more-elements-from-vector
res1 <- combn(d$expenses, 2)[, which(colSums(combn(d$expenses ,2)) == 2020)]
answer1 <- prod(res1)
answer1

# To do the same thing for 3 numbers, we just use that in `combn`
res2 <- combn(d$expenses, 3)[, which(colSums(combn(d$expenses ,3)) == 2020)]
answer2 <- prod(res2)
answer2
