# Function to abstract data retrival
multiple_group_equate <- function(dataset_a, dataset_b, instrument_pairs, grouping_variables, use_weights = T) { # weights are vectors/df of size nrow(dataset_X)

  task_df <- instrument_pairs %>%
    mutate(grouping_variables = list(grouping_variables)) %>%
    unnest(grouping_variables) %>%
    filter(name != grouping_variables) %>%
    modify_at(c("source", "target", "current_weight_a", "current_weight_b"), ~ map(.x, sym))

  dataset_a <- dataset_a %>%
    modify_at(vars(-grouping_variabels), as.numeric)
  dataset_b <- dataset_b %>%
    modify_at(vars(-grouping_variabels), as.numeric)

  equatings <- pmap(task_df, get_instruments, dataset_a, dataset_b, use_weights = T)

  result <- task_df %>%
    select(name, grouping_variables) %>%
    mutate(equatings) %>%
    unnest(equatings)
}



# use data from the excel spreadsheet to read in correct data and equate
get_instruments <- function(name,
                            source,
                            target,
                            filter_source,
                            source_upr,
                            source_lwr,
                            filter_target,
                            target_lwr,
                            target_upr,
                            source_invert,
                            source_max_1,
                            target_invert,
                            target_max_1,
                            current_weight_a,
                            current_weight_b,
                            grouping_variables,
                            dataset_a,
                            dataset_b,
                            use_weights = T) {
  # cat("\nNow working on:",..1,",", ..14)
  # check if source data needs to be filtered
  if (!!filter_source) {
    source <- filter_instruments(
      dataset = dataset_a,
      name = source,
      group = grouping_variables,
      lwr = source_lwr,
      upr = source_upr,
      weight = current_weight_a
    )
  } else {
    source <- dataset_a %>%
      select(
        item = !!source,
        group = !!grouping_variables,
        weight = !!current_weight_a
      ) %>%
      modify_at("item", as.numeric)
  }
  # check if target data needs to be filtered
  if (!!filter_target) {
    target <- filter_instruments(
      dataset = dataset_b,
      name = target,
      group = grouping_variables,
      lwr = target_lwr,
      upr = target_upr,
      weight = current_weight_b
    )
  } else {
    target <- dataset_b %>%
      select(
        item = !!target,
        group = !!grouping_variables,
        weight = !!current_weight_b
      ) %>%
      modify_at("item", as.numeric)
  }
  # invert if necessary
  if (source_invert) source <- source %>% mutate(item = source_max_1 - item)
  if (target_invert) target <- target %>% mutate(item = target_max_1 - item)
  # perform grouped equating
  results <- grouped_equate2(
    x = source,
    y = target,
    xmin = source_lwr + 1,
    xmax = source_upr - 1,
    ymin = target_lwr + 1,
    ymax = target_upr - 1,
    use_weights = use_weights
  )
  return(results)
}

# helper function to filter instruments
filter_instruments <- function(dataset, name, group, lwr, upr, weight) {
  filtered_data <- dataset %>%
    filter(!!name > !!lwr & !!name < !!upr) %>%
    select(item = !!name, group = !!group, weight = !!weight) %>%
    modify_at("item", as.numeric)
}

check_range_complete <- function(x, xmin, xmax) {
  stopifnot(xmin < xmax)
  range_variable <- xmin:xmax
  x_values <- unique(x) %>% sort()
  if_else(identical(range_variable, x_values), T, F)
}
