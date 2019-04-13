# Get Y Rankings ----------------------------------------------------------
y_ranks <- get_y_layout(pathways, offset_gap = 0.5)


# Get X Rankings ----------------------------------------------------------
x_ranks <- get_x_layout(all_pairs, layout = "kk")

ranked_data_coords <- dplyr::full_join(x_ranks, y_ranks)

ranked_data <- reference %>%
  right_join(ranked_data_coords)


use_data(ranked_data, overwrite = TRUE)
