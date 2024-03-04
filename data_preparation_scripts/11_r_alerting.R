# Calculate r alerting for the instruments present in both GPANEL and EVS

# Uses instrument pairs excel table to create aligned gpanel and evs datasets
# (all items have the same polarity and missing values are coded as NAs)
align_instruments <- function(instrument_data, instrument_name, instrument_pairs, source_or_target = NULL) {
  if (is.null(source_or_target)) stop("Specify whether source or target is used.")
  instrument_data <- tibble(!!sym(instrument_name) := instrument_data %>% as.numeric())
  instrument_information <- instrument_pairs %>% select(contains(source_or_target))
  names(instrument_information) <- names(instrument_information) %>% str_remove(paste0(source_or_target, "_|_", source_or_target))
  instrument_information <- instrument_information %>% filter(!!sym(source_or_target) == instrument_name)
  if (instrument_information$filter == T) {
    instrument_data <- instrument_data %>%
      mutate(!!sym(instrument_name) := case_when(
        !!sym(instrument_name) < instrument_information$lwr ~ NA_real_,
        !!sym(instrument_name) > instrument_information$upr ~ NA_real_,
        T ~ !!sym(instrument_name)
      ))
  }
  if (instrument_information$invert == T) {
    instrument_data <- instrument_data %>% mutate(!!sym(instrument_name) := instrument_information$max_1 - !!sym(instrument_name))
  }
  instrument_data
}




# correlation with all other vars variables
evs_cor_data <- evs_analysis_data %>%
  select(contains(instrument_pairs$target)) %>%
  imap_dfc(align_instruments, instrument_pairs, "target") %>%
  mutate(raked_weights = evs_analysis_data$raked_weights) %>%
  bind_cols(evs_analysis_data %>% select(contains(grouping_variabels)) %>% imodify(as.numeric))

evs_svy <- svydesign(id = ~0, data = evs_cor_data, weights = ~raked_weights)
# substantive variables
evs_task_df <- tibble(evs_variables = c(instrument_pairs$target, grouping_variabels), vars = list(c(instrument_pairs$target, grouping_variabels))) %>% unnest(vars)

evs_task_df <- evs_task_df %>% filter(evs_variables != vars)

survey_cor <- function(variables, vars, statistic, current_design) {
  if (statistic == "r") {
    cor_matrix <- svyvar(formula(paste0("~", vars, "+", variables)), design = current_design, na.rm = T) %>%
      as.matrix() %>%
      cov2cor()
    cor_matrix[2, 1]
  }
}

plan(multisession, workers = 8)
evs_correlations <- evs_task_df %>%
  mutate(r = future_map2_dbl(evs_task_df$evs_variables, evs_task_df$vars, ~ survey_cor(.x, .y, statistic = "r", current_design = evs_svy)))
plan(sequential)
evs_correlations <- evs_correlations %>%
  left_join(instrument_pairs %>%
    dplyr::select(evs_variables = target, reference_instrument = name) %>%
    bind_rows(tibble(evs_variables = grouping_variabels, reference_instrument = grouping_variabels))) %>%
  left_join(instrument_pairs %>%
    dplyr::select(vars = target, criterion = name) %>%
    bind_rows(tibble(vars = grouping_variabels, criterion = grouping_variabels))) %>%
  rename(evs_criterion_vars = vars, evs_reference_vars = evs_variables) %>%
  mutate(survey = "evs")


# get allowed pairings for all itempairs (=within wave correlation)
gpanel_task_df <- tibble(
  gpanel_variables = c(instrument_pairs$source, grouping_variabels),
  vars = list(c(instrument_pairs$source, grouping_variabels))
) %>% unnest(vars)

# only use instruments from the same wave
gpanel_task_df <- gpanel_task_df %>%
  filter(str_sub(gpanel_variables, 1, 3) == str_sub(vars, 1, 3) |
    gpanel_variables %in% grouping_variabels |
    vars %in% grouping_variabels) %>%
  filter(gpanel_variables != vars) %>%
  filter(!(gpanel_variables %in% grouping_variabels & vars %in% grouping_variabels))

# turn character values into factorvalues (the same as in evs_analysis_data)
gpanel_grouping_fac <- map_dfc(grouping_variabels, ~ {
  current_gpanel_variable <- gpanel_analysis_data %>% select(all_of(.x))
  current_evs_variable <- evs_analysis_data %>% select(all_of(.x))
  factor(current_gpanel_variable %>% pull(), levels = levels(current_evs_variable %>% pull()))
})

names(gpanel_grouping_fac) <- paste0(grouping_variabels)

gpanel_cor_data <- gpanel_analysis_data %>%
  select(contains(instrument_pairs$source)) %>%
  imap_dfc(align_instruments, instrument_pairs, "source") %>%
  bind_cols(gpanel_grouping_fac) %>%
  bind_cols(gpanel_analysis_data %>% select(contains("rake")))


# correlations within wave
gpanel_cor <- function(variable_x, variable_y, ..., weights = T) {
  # get wavespecific weight variable
  if (str_detect(variable_x, "\\d")) {
    item_wave <- str_sub(variable_x, 1, 2)
  } else {
    item_wave <- str_sub(variable_y, 1, 2)
  }
  wave_weight <- tibble(var_names = names(gpanel_cor_data)) %>%
    filter(str_detect(var_names, paste0(item_wave, ".*raked"))) %>%
    pull()
  wave_data <- gpanel_cor_data %>%
    filter(!is.na(!!sym(wave_weight))) %>%
    mutate(
      across(where(~ is.labelled(.x)), as.double),
      across(where(~ is.factor(.x)), as.double)
    )
  wave_weight_formula <- paste0("~", wave_weight)
  # create wavespecific surveydesign with wspweight
  gpanel_svy <- svydesign(id = ~0, data = wave_data, weights = formula(wave_weight_formula))
  if (weights == F) gpanel_svy <- svydesign(id = ~0, data = wave_data)
  current_statistic <- list(...)[[1]]
  survey_cor(variable_x, variable_y, statistic = current_statistic, current_design = gpanel_svy)
}

gpanel_correlations <- gpanel_task_df %>%
  mutate(r = map2_dbl(gpanel_task_df$gpanel_variables, gpanel_task_df$vars, gpanel_cor, "r"))




gpanel_correlations <- gpanel_correlations %>%
  left_join(instrument_pairs %>%
    dplyr::select(
      gpanel_variables = source,
      reference_instrument = name
    ) %>%
    bind_rows(tibble(
      gpanel_variables = grouping_variabels,
      reference_instrument = grouping_variabels
    ))) %>%
  left_join(instrument_pairs %>%
    dplyr::select(vars = source, criterion = name) %>%
    bind_rows(tibble(
      vars = grouping_variabels,
      criterion = grouping_variabels
    ))) %>%
  rename(gpanel_criterion_vars = vars, gpanel_reference_vars = gpanel_variables) %>%
  mutate(survey = "gpanel")


# combine gpanel and evs data

r_alerting <- bind_rows(
  gpanel_correlations %>% select(survey, reference_instrument, criterion, r),
  evs_correlations %>% select(survey, reference_instrument, criterion, r)
)
r_alerting <- r_alerting %>%
  mutate(r = r %>% round(3)) %>%
  pivot_wider(names_from = "survey", values_from = c("r"), values_fn = mean) %>%
  mutate(r_diff = round(abs(gpanel - evs), 3)) %>%
  drop_na() %>%
  rename(r_gpanel = gpanel, r_evs = evs)
