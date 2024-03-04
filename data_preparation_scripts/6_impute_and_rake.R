
# get intstrument pairs and grouping_variables


instrument_pairs <- read_excel("list_of_instrument_pairs/instrument_pairs_16.xlsx") %>% mutate(
  filter_source = filter_source %>% as.logical(),
  filter_target = filter_target %>% as.logical(),
  source_invert = source_invert %>% as.logical(),
  target_invert = target_invert %>% as.logical()
)
grouping_variabels <- c("east_west", "sex", "age_cat", "edu")

all_weight_names <- get_labels(full_gpanel) %>%
  filter(str_detect(value, "weight")) %>%
  pull(name)
map_chr(str_sub(instrument_pairs$source, 1, 2), ~
  all_weight_names[str_detect(
    all_weight_names,
    paste0("^", .x, "za.*07a", "$")
  )])
grouping_variabels_int <- grouping_variabels %>% map_chr(~ paste0(.x, "_i"))


# impute and rake variables
#####################
## EVS


# getting variables to impute
evs_to_impute <- evs_17 %>%
  select(instrument_pairs$target, grouping_variabels_int) %>%
  mutate(across(everything(), ~ as.integer(.x)))




# impute
evs_imputed <- mice(evs_to_impute, m = 1, maxit = 1, method = "pmm")
imputed_grouping_vars_evs <- complete(evs_imputed) %>%
  select(all_of(grouping_variabels_int)) %>%
  rename_with(.cols = everything(), .fn = ~grouping_variabels)

# create factor with correct levels and labels & overwrite grouping vars with imputed grouping vars
evs_17 <- evs_17 %>% imodify(~ {
  if (.y %in% grouping_variabels) {
    factor(
      x = imputed_grouping_vars_evs %>% pull(!!sym(.y)),
      levels = evs_17 %>% pull(!!sym(paste0(.y, "_f"))) %>% fct_anon() %>% levels(),
      labels = levels(evs_17 %>% pull(!!sym(paste0(.y, "_f"))))
    )
  } else {
    .x
  }
})


# adding raked weights evs
evs_17_raked <- evs_17 %>%
  filter(age_cat != "unter 20") %>%
  mutate(raked_weights = create_weigths(evs_17 %>%
    filter(age_cat != "unter 20"),
  "dweight",
  grouping_variabels = grouping_variabels,
  pop_margins = pop_marg
  )) # returnes standardized and trimmed weights


# check results
# map(instrument_pairs$target,
#    ~evs_17_raked %>%
#      group_by(!!sym(.x)) %>%
#      summarize(unweighted_freq=n(),
#                raked_weights=sum(raked_weights,na.rm=T)))
#

#################
# GESIS Panel
# impute GESIS Panel
to_impute_gpanel <- full_gpanel %>%
  filter(age_cat != "unter 20") %>%
  select(contains(grouping_variabels_int), all_of(instrument_pairs$source)) %>%
  drop_na(east_west_i) # east west NAs are rows with only NAs
id_vec <- full_gpanel %>%
  filter(age_cat != "unter 20") %>%
  drop_na(east_west_i) %>%
  select(z000001a) # id vector to bind imputed values to full gesis panel later




# 1. get the full data with negative values as NA
to_impute_gpanel <- to_impute_gpanel %>%
  mutate(across(-all_of(grouping_variabels_int), ~ if_else(.x > 0, as.numeric(.x), NA_real_)))




# 2. impute
imputed <- mice(to_impute_gpanel, m = 1, maxit = 1, method = "pmm", )
imputed_grouping_vars <- complete(imputed) %>%
  as_tibble() %>%
  select(all_of(grouping_variabels_int)) %>%
  rename_with(~grouping_variabels) %>%
  bind_cols(id_vec)



# replace unimputed grouping variables with imputed variables
to_rake_gpanel <- full_gpanel %>%
  select(z000001a, all_of(c(instrument_pairs$source, all_weight_names))) %>%
  left_join(imputed_grouping_vars, by = "z000001a") %>%
  imodify(~ if (.y %in% grouping_variabels) {
    factor(
      x = .x,
      levels = full_gpanel %>% pull(!!sym(paste0(.y, "_f"))) %>% fct_anon() %>% levels(),
      labels = levels(full_gpanel %>% pull(!!sym(paste0(.y, "_f"))))
    )
  } else {
    .x
  }) %>%
  drop_na(east_west)


# raking for multiple waves
task_df <- map_df(
  instrument_pairs$source,
  ~ tibble(
    source = .x,
    weight = all_weight_names[str_detect(all_weight_names, str_sub(.x, 1, 2))]
  )
) %>% distinct(weight)

raked_weights <- map_df(task_df$weight, ~ {
  var <- sym(.x)
  data <- to_rake_gpanel %>% filter(!!var > 0)
  raked_weights <- create_weigths(
    df = data,
    name_of_design_weight = .x,
    grouping_variabels = grouping_variabels,
    pop_margins = pop_marg
  ) # returns standardized and trimmed weights

  results <- tibble(
    z000001a = data$z000001a,
    wave = paste0(.x, "_raked_weights"),
    raked_weights
  )
}) %>% pivot_wider(id_cols = z000001a, names_from = wave, values_from = raked_weights)




# add weights to full gpanel file
gpanel_raked <- full_gpanel %>%
  select(z000001a, all_of(c(grouping_variabels, instrument_pairs$source, all_weight_names))) %>%
  left_join(raked_weights)


# check results
task_df <- map_df(
  instrument_pairs$source,
  ~ tibble(
    source = .x,
    weight = all_weight_names[str_detect(all_weight_names, str_sub(.x, 1, 2))]
  )
)
