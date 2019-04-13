
TFs <- pathways %>%
  filter(Type == "TF") %>%
  pull(Name) %>%
  unique() %>%
  str_to_upper()

TFs2 <- pathways %>%
  filter(Type == "TF") %>%
  pull(Name) %>%
  unique() %>%
  sort() %>%
  str_to_upper()


library(tidyverse)


Targets_LEC <- read_tsv("data-raw/lens_epithelial_cells.txt", col_names = c("TR", "Target", "Regulation_Score")) %>%
  filter(TR %in% TFs2) %>%
  pull(Target) %>%
  filter(TR %in% TFs2)

use_data(Targets_LEC, overwrite = TRUE)


Targets_LEC_Line <- read_tsv("data-raw/lens_epithelial_cell_line.txt", col_names = c("TR", "Target", "Regulation_Score")) %>%
  filter(TR %in% TFs)


crossing(Targets_LEC %>% select(1, 2))


Targets_LEC %>%
  count(TF)
