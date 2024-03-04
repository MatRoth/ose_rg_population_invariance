#add wavespecific weights to instrument pairs
instrument_pairs <- instrument_pairs %>%
  mutate(current_weight_a = map_chr(source,~{tibble(weight_names = all_weight_names) %>%
      filter(str_detect(weight_names,str_sub(.x,1,2))) %>% pull(weight_names) %>% paste0("_raked_weights")}),
      current_weight_b = "raked_weights")
#select required data
gpanel_analysis_data <- gpanel_raked %>% select(all_of(c(grouping_variabels,instrument_pairs$source,instrument_pairs$current_weight_a)))
evs_analysis_data <- evs_17_raked%>% select(all_of(c(grouping_variabels,instrument_pairs$target,"raked_weights")))

#get equatings
output<-multiple_group_equate(dataset_a = gpanel_analysis_data,
                              dataset_b = evs_analysis_data,
                              instrument_pairs,
                              grouping_variabels,
                              use_weights = T)

#rename full equatings 
output<-output %>%
  distinct(equate_model,.keep_all = T) %>% 
  mutate(grouping_variables = if_else(grouping_variable_value == "full","full",grouping_variables))
