create_weigths <- function(df, name_of_design_weight, grouping_variabels, pop_margins, trimming = TRUE) { # takes prepared df with no na and only the variables to rake
  svy_design <- svydesign(
    ids = ~0,
    weights = as.formula(paste0("~", name_of_design_weight)),
    data = df
  )
  raked_design <- rake(svy_design, sample.margins = map(grouping_variabels, ~ as.formula(paste0("~", .x))), population.margins = pop_margins)
  unstandardized_weights <- weights(raked_design) # as proposed in Valliant 2018: 411-412
  standardized_raked_weights <- unstandardized_weights * nrow(df)

  # trimm weights if necessary
  if (trimming == TRUE) {
    standardized_raked_design <- svydesign(
      ids = ~0,
      weights = standardized_raked_weights,
      data = df
    )
    # determine weight outliers (3.5 times the median weight). No clear rule just a historical precedence (Valliant 2018: 411-412)
    upper_bound <- median(standardized_raked_weights) + (median(standardized_raked_weights) * 3.5)
    lower_bound <- median(standardized_raked_weights) - (median(standardized_raked_weights) * 3.5)
    # trim and retrieve trimmed weights
    return(standardized_raked_design %>% trimWeights(upper = upper_bound, lower = lower_bound, strict = F) %>% weights())
  }
  return(standardized_raked_weights)
}
