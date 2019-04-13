# library(tidyverse)
# library(lensdna)
#
# data("pathways")
#
# # Get Y Rankings ----------------------------------------------------------
#
# receptor_path <- pathways %>%
#   filter(Type == "Receptor") %>%
#   select(Process, Name, Position) %>%
#   add_count(Process) %>%
#   rename(Receptor_Length = n) %>%
#   group_by(Process) %>%
#   mutate(Position = row_number()) %>%
#   ungroup() %>%
#   mutate(Position_Prop = case_when(
#     Receptor_Length == 1 ~ 0.5,
#     TRUE ~ (Position - 1)/(Receptor_Length - 1)
#   ))
#
# receptor_ranks <- receptor_path %>%
#   group_by(Name) %>%
#   summarise(ranking = mean(Position_Prop)*-1 +1.5,
#             n = n())
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# # Kinase ------------------------------------------------------------------
# kinase_path <- pathways %>%
#   filter(Type == "Kinase") %>%
#   select(Process, Name, Position) %>%
#   add_count(Process) %>%
#   rename(Kinase_Length = n) %>%
#   group_by(Process) %>%
#   mutate(Position = row_number()) %>%
#   ungroup() %>%
#   mutate(Position_Prop = Position/Kinase_Length)
#
#
# kinase_ranks <- kinase_path %>%
#   group_by(Name) %>%
#   summarise(ranking = mean(Position_Prop)*-1,
#             n = n())
#
# use_data(kinase_path, overwrite = TRUE)
# use_data(kinase_ranks, overwrite = TRUE)
#
#
# # Receptors ---------------------------------------------------------------
#
#
# use_data(receptor_path, overwrite = TRUE)
# use_data(receptor_ranks, overwrite = TRUE)
#
#
# kinase_jitter <- kinase_ranks %>%
#   mutate(jitter = runif(nrow(.))) %>%
#   select(-n)
#
#
#
# receptor_jitter <- receptor_ranks %>%
#   mutate(
#     jitter = runif(nrow(.))
#   ) %>%
#   select(-n)
#
# TF_Jitter <- pathways %>%
#   filter(Type == "TF") %>%
#   select(Process, Name, Position) %>%
#   add_count(Process) %>%
#   rename(Kinase_Length = n) %>%
#   group_by(Process) %>%
#   mutate(Position = row_number()) %>%
#   ungroup() %>%
#   mutate(Position_Prop = case_when(
#     Kinase_Length == 1 ~0.5,
#     TRUE ~ (Position-1)/(Kinase_Length-1)
#   )) %>%
#   group_by(Name) %>%
#   summarise(ranking = mean(Position_Prop)*-1 - 1.5,
#             n = n()) %>%
#   mutate(
#     jitter = runif(nrow(.))
#   ) %>%
#   select(-n)
#
# ranked_data <- receptor_jitter %>%
#   bind_rows(kinase_jitter) %>%
#   bind_rows(TF_Jitter)
#
# joined_data <- pairs_count %>%
#   left_join(ranked_data, c("Name" = "Name")) %>%
#   left_join(ranked_data, c("Name2" = "Name"))
#
# use_data(ranked_data, overwrite = TRUE)
# use_data(joined_data, overwrite = TRUE)
