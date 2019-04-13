# See Vingette for comments
# TODO: Extract functions for this and add commentary
library(tidyverse)
library(readxl)
library(usethis)

pathways_messy <- readxl::read_excel("data-raw/RK_pathways.xlsx", sheet = 1, col_names = FALSE)

ref_receptors <- readxl::read_excel("data-raw/RKT_Reference.xlsx", sheet = 1)
ref_kinase <- readxl::read_excel("data-raw/RKT_Reference.xlsx", sheet = 2)
ref_TF <- readxl::read_excel("data-raw/RKT_Reference.xlsx", sheet = 3)

pathways_long <- pathways_messy %>%
  rowid_to_column("Process") %>%
  gather(Position, Name, -Process) %>%
  filter(is.na(Name) == FALSE) %>%
  arrange(Process) %>%
  separate(Position, into = c("Nothing", "Position"), sep = "X__", convert = TRUE) %>%
  select(Process, Name, Position)

kinase_list <- ref_kinase$Kinases
receptor_list <- ref_receptors$Receptors
TF_list <- ref_TF$TFs

pathways_type <- pathways_long %>%
  mutate(Type = case_when(
    Name %in% kinase_list ~ "Kinase",
    Name %in% receptor_list ~ "Receptor",
    Name %in% TF_list ~ "TF",
    TRUE ~ NA_character_
  ))


pathways <- pathways_type
# mutate(
#   # Start_R = Name[min(Position)],
#   # End_TF = Name[max(Position)],
#   # Middle_Ks = list(Name[(min(Position) + 1):(max(Position) - 1)]),
# ) %>%

use_data(pathways, overwrite = TRUE)
