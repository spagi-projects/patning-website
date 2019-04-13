library(tidyverse)
library(readxl)

rec_reference <- read_excel("data-raw/RKT_Reference.xlsx") %>%
  select(1, 2, 3) %>%
  gather(Cell, Expression, -1)

k_reference <- read_excel("data-raw/RKT_Reference.xlsx", sheet = 2) %>%
  select(1, 2, 3) %>%
  gather(Cell, Expression, -1)

tf_reference <- read_excel("data-raw/RKT_Reference.xlsx", sheet = 3) %>%
  select(1, 2, 3) %>%
  gather(Cell, Expression, -1)



reference <- map(1:3, ~{
  temp <- read_excel("data-raw/RKT_Reference.xlsx", sheet = .x)
  type <- names(temp)[[1]]
  # browser()
  temp %>%
    rename(Name = 1) %>%
    select(1, 2, 3) %>%
    mutate(type = type)
}) %>% reduce(bind_rows)

use_data(reference, overwrite = TRUE)
