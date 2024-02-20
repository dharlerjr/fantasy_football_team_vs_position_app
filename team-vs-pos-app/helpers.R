

# A function factory for getting integer x-axis values.
# Courtesy of Joshua Cook
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# Revised integer breaks function
my_integer_breaks <- function(min = 1, max = 18, n = 5, ...) {
  breaks <- floor(pretty(min:max, n, ...))
  breaks <- append(breaks, min)
  return(breaks)
}


# gtTable Helper Function for League Overview
my_gt_fn <- function(x) {
  gt(x) %>%
    gt_theme_538() %>%
    gt_merge_stack(col1 = QB, col2 = qbRank, 
                   palette = c('black', 'blue')) %>%
    gt_merge_stack(col1 = RB, col2 = rbRank, 
                   palette = c('black', 'blue')) %>%
    gt_merge_stack(col1 = WR, col2 = wrRank, 
                   palette = c('black', 'blue')) %>%
    gt_merge_stack(col1 = TE, col2 = teRank, 
                   palette = c('black', 'blue')) %>%
    gt_merge_stack(col1 = K, col2 = kRank, 
                   palette = c('black', 'blue')) %>%
    gt_img_rows(team_wordmark) %>%
    opt_align_table_header("center") %>%
    cols_align("center") %>%
    cols_label(team_wordmark = "Team") %>%
    opt_row_striping() %>%
    tab_style(
      style = cell_text(size = px(16), font = "Arial"), 
      locations = list(cells_column_labels())) %>%
    cols_width(team_wordmark ~ px(120), QB  ~ px(60), RB ~ px(60), 
               WR ~ px(60), TE ~ px(60), K ~ px(60)) %>%
    data_color(
      columns = qbRank,
      target_columns = QB, 
      fn = scales::col_numeric(
        palette =  c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"),
        domain = c(1, 32))) %>%
    data_color(
      columns = rbRank,
      target_columns = RB, 
      fn = scales::col_numeric(
        palette =  c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"),
        domain = c(1, 32))) %>%
    data_color(
      columns = wrRank,
      target_columns = WR, 
      fn = scales::col_numeric(
        palette =  c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"),
        domain = c(1, 32))) %>%
    data_color(
      columns = teRank,
      target_columns = TE, 
      fn = scales::col_numeric(
        palette =  c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"),
        domain = c(1, 32))) %>%
    data_color(
      columns = kRank,
      target_columns = K, 
      fn = scales::col_numeric(
        palette =  c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"),
        domain = c(1, 32)))
}





