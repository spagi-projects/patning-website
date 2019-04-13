context("test-layout-tests")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("X Rankings Consistent", {
  library(tidygraph)
  library(igraph)
  library(tibble)
  gr1 <- create_notable("bull") %>%
    mutate(Name = letters[1:5])



  # Use the V notation from igraph
  i_layout <- layout_nicely(gr1)
  # V(gr1)$rank_x = i_layout[, 1]
  i_graph <- tibble(
    Name = gr1 %>%
      activate("nodes") %>%
      pull(),
    rank_x = i_layout[, 1]
  )

  lens_graph <- get_x_layout(gr1)
  expect_equal(lens_graph, i_graph)
})
