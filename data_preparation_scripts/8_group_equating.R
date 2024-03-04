grouped_equate2 <- function(x, y, xmin, xmax, ymin, ymax, use_weights = T) {

  # data preparation - equating is only conducted where no NA is present in substantive and grouping variable
  data_x <- x %>%
    select(x = 1, g_x = 2, weight = 3) %>%
    drop_na()
  data_y <- y %>%
    select(y = 1, g_y = 2, weight = 3) %>%
    drop_na()
  data_x_full <- x %>% select(x = 1, g_x = 2, weight = 3)
  data_y_full <- y %>% select(y = 1, g_y = 2, weight = 3)
  # toggle for using or not-using weights (replaces weights with 1)
  if (use_weights == F) {
    data_x <- data_x %>% mutate(weight = 1)
    data_y <- data_y %>% mutate(weight = 1)
  }
  # equating for the full sample
  full_sample_equate_model <- equate(
    x = freqtab(get_weighted_data(data_x_full),
      scales = xmin:xmax
    ),
    y = freqtab(get_weighted_data(data_y_full),
      scales = ymin:ymax
    ),
    type = "e",
    smoothmethod = "bump",
    jmin = 1e-6
  )
  full_results <- tibble(grouping_variable_value = "full", equate_model = list(full_sample_equate_model))


  # equating by group
  groupings <- data_x %>%
    distinct(g_x) %>%
    select(g_x)
  sub_sample_equatings <- map_dfr(groupings$g_x, ~ {
    group_x <- data_x %>% filter(g_x == .x)
    group_y <- data_y %>% filter(g_y == .x)
    group_x_weighted <- get_weighted_data(group_x)
    group_y_weighted <- get_weighted_data(group_y)
    sub_sample_equate_model <- equate(
      x = freqtab(group_x_weighted,
        scales = xmin:xmax
      ),
      y = freqtab(group_y_weighted,
        scales = ymin:ymax
      ),
      type = "e",
      smoothmethod = "bump",
      jmin = 1e-6
    )
    results <- tibble(grouping_variable_value = .x, equate_model = list(sub_sample_equate_model))
  })
  output <- bind_rows(sub_sample_equatings, full_results)
  output
}

get_weighted_data <- function(group_data) {
  group_data <- group_data %>% select(var = 1, weight = 3)
  freq_table <- group_data %>%
    group_by(var) %>%
    summarize(weighted_freq = sum(weight, na.rm = T) %>% round())
  map2(freq_table$var, freq_table$weighted_freq, ~ rep(.x, .y)) %>% flatten_dbl()
}
