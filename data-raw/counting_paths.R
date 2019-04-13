# kinase_path <- pathways %>%
#   filter(Type == "Kinase") %>%
#   select(Process, Name, Position) %>%
#   add_count(Process) %>%
#   rename(Kinase_Length = n) %>%
#   group_by(Process) %>%
#   mutate(Position = row_number()) %>%
#   ungroup() %>%
#   mutate(Position_Prop = Position/Kinase_Length)


pairs <- kinase_path %>%
  split(.$Process) %>%
  map_df(~{
    mutate(.x, Name2 = lead(Name)) %>%
      select(Name, Name2) %>%
      head(-1)
  })

pairs_count <- pairs %>%
  count(Name, Name2) %>%
  arrange(desc(n))


use_data(pairs_count, ooverwrite = TRUE)


all_pairs <- pathways %>%
  nest(-c(Process)) %>%
  mutate(data = map(data, ~{
    tbl <- tibble(
      From = .x$Name,
      To = lead(.x$Name)
      # TODO: Keep the list columns outside of the nested data frame to retain them
      # This night be doen trhough selecting Process, contain_r/k/tf and left joingin on to the new pairs data
    ) %>%
      head(-1)
  })) %>%
  unnest()

use_data(all_pairs, overwrite = TRUE)


# all_pairs %>% filter(map_lgl(Contain_TF, ~'Atf2' %in% .))
#
#
# map_lgl(all_pairs$Contain_K, ~{
#   'Map3k7' %in% .
#   })
