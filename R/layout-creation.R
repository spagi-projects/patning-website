get_x_layout <- function(df_pairs, layout = "nicely") {
  if (!igraph::is.igraph(df_pairs)) {
    graph <- df_pairs %>%
      dplyr::select(From, To) %>%
      tidygraph::as_tbl_graph()
  } else {
    graph <- tidygraph::as_tbl_graph(df_pairs)
  }

  if (layout == "kk") {
    L <- graph %>%
      igraph::layout_with_kk()
  } else {
    L <- graph %>%
      igraph::layout_nicely()
  }
  # Get the x layout to return
  Nodes <- graph %>%
    tidygraph::activate("nodes") %>%
    dplyr::pull()

  tbl <- tibble::tibble(
    Name = Nodes,
    rank_x = L[, 1] + rnorm(length(Name), sd = 0.1)
    # rank_x = as.vector(scale(L[, 1]))
  )
  return(tbl)
}



get_y_rankings_type <- function(data, type, offset_gap = 0.5) {
  if (type == "Receptor") {
    offset <- 1.5
  } else if (type == "Kinase") {
    offset <- 0
  } else {
    offset <- -1
  }

  # pathways
  data %>%
    dplyr::filter(Type == type) %>%
    dplyr::select(Process, Name, Position) %>%
    dplyr::add_count(Process) %>%
    dplyr::rename(Type_Length = n) %>%
    dplyr::group_by(Process) %>%
    dplyr::mutate(Position = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Position_Prop = dplyr::case_when(
      Type_Length == 1 ~ 0.5,
      TRUE ~ (Position - 1) / (Type_Length - 1)
    )) %>%
    dplyr::group_by(Name) %>%
    dplyr::summarise(
      rank_y = mean(Position_Prop) * -1 + (offset + offset_gap),
      n = dplyr::n()
    ) %>%
    dplyr::select(-n)
}


get_y_layout <- function(data, offset_gap = 0.5) {
  Receptor <- get_y_rankings_type(data, type = "Receptor", offset_gap = offset_gap)
  Kinase <- get_y_rankings_type(data, type = "Kinase", offset_gap = offset_gap)
  TF <- get_y_rankings_type(data, type = "TF", offset_gap = offset_gap)

  y_layout <- dplyr::bind_rows(Receptor, Kinase, TF)
  return(y_layout)
}
