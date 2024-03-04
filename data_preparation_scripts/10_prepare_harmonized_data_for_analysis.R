# helper function to get vector of unharmonized source data
source_val <- function(equate_model){
  freq_tab<-equate_model$x %>% as_tibble
  map2(freq_tab$total,freq_tab$n,~rep(.x,.y)) %>% flatten_dbl
}
# helper function to get vector of target data
target_val <- function(equate_model){
  freq_tab<-equate_model$y %>% as_tibble
  map2(freq_tab$total,freq_tab$n,~rep(.x,.y)) %>% flatten_dbl
}

# helper function to get vector of harmonized source data
harm_val <- function(equate_model){
  concordance <- equate_model$concordance %>% as_tibble
  freq_tab <- equate_model$x %>% as_tibble
  map2(concordance$yx,freq_tab$n,~rep(.x,.y)) %>% flatten_dbl
}


#helper function to get linear streched values of a source instrument in terms of a target instrument
stretchify <- function(source_response_vector,target_response_vector,method="full"){
  max_vec_source <- max(source_response_vector)
  min_vec_source <- min(source_response_vector)
  max_vec_target <- max(target_response_vector)
  min_vec_target <- min(target_response_vector)
  range_vec_source <- max_vec_source-min_vec_source
  range_vec_target <- max_vec_target-min_vec_target

  #standardize to range of source instrument
  difficulty_source<-(source_response_vector-min_vec_source)/range_vec_source
  #rescale to range of target instrument
  return((difficulty_source*range_vec_target)+min_vec_target)
}

# add all allowed linking and research population combinations 
combinations_nested_by_instrument<-output %>% rename(grouping_variables_translate=grouping_variables,
                                                     grouping_variable_value_translate = grouping_variable_value,
                                                     equate_model_translate = equate_model) %>% group_by(name) %>% nest()
all_allowed_equatings <- output %>%
  left_join(combinations_nested_by_instrument) %>%
  unnest(data)

plan(multisession,workers=3)
all_allowed_equatings  <- all_allowed_equatings  %>%
  mutate(harmonized_mean = future_map2_dbl(equate_model,equate_model_translate,~equate(source_val(.x),.y) %>% mean),
         target_mean     = future_map_dbl(equate_model,~target_val(.x) %>% mean),
         target_sd       = future_map_dbl(equate_model,~target_val(.x) %>% sd),
         cohens_d    = (harmonized_mean-target_mean)/target_sd)
plan(sequential)


all_allowed_equatings  <- all_allowed_equatings %>% 
  select(name,
         data = grouping_variable_value,
         equate_model_data = equate_model,
         equate_model_used = equate_model_translate,
         mean_x_in_y = harmonized_mean,
         mean_y = target_mean,
         sd_y = target_sd,
         cohen_d_to_target=cohens_d,
         translation_table_used = grouping_variable_value_translate) %>% 
  mutate(cohen_d_to_target_abs = cohen_d_to_target %>% abs)

# Adding similarity of linking and research population 
grouping_variables_levels<-evs_analysis_data %>%
  select(all_of(grouping_variabels)) %>%
  mutate(across(everything(),~list(levels(.x)))) %>%
  unique %>%
  pivot_longer(everything()) %>%
  unnest(value) %>% 
  bind_rows(c(name="full",value="full"))

grouping_variables_combinations <- grouping_variables_levels %>%
  mutate(new = list(grouping_variables_levels %>% select(name2 = name, value2=value))) %>% 
  unnest(new)

# Calculate simliarity of linking and research population by
# using the (weighted) respondents of the EVS to build (sub-)populations of the full German population.
check_similarity<-function(g1_name,g1_value,g2_name,g2_value,id_name,weight_name,data){
  f_data_g1 <- data %>% select(all_of(c(id_name,g1_name,weight_name))) %>% filter(!!sym(g1_name) == g1_value)
  names(f_data_g1) <- c("id_name","g1_name","weight_1")
  f_data_g2 <- data %>% select(all_of(c(id_name,g2_name,weight_name))) %>% filter(!!sym(g2_name) == g2_value)
  names(f_data_g2) <- c("id_name","g2_name","weight_2")
  joined_data <- inner_join(f_data_g1,f_data_g2, by = "id_name")
  #number of matches*2/number of all elements -> Sørensen–Dice coefficient
  (sum(joined_data$weight_1)*2)/(sum(f_data_g1$weight_1)+(sum(f_data_g2$weight_2)))
}



similarity<-pmap_dbl(list(g1_name=grouping_variables_combinations$name,
          g1_value=grouping_variables_combinations$value,
          g2_name=grouping_variables_combinations$name2,
          g2_value=grouping_variables_combinations$value2),
     check_similarity,"caseno","raked_weights",evs_17_raked %>% mutate(full = "full"))


grouping_variables_combinations_similarity <- grouping_variables_combinations %>%
  mutate(similarity) %>% 
  select(data = value,
         translation_table_used = value2,
         similarity) %>% 
  filter(data != "unter 20"&translation_table_used != "unter 20")

all_allowed_equatings<-all_allowed_equatings %>%
  left_join(grouping_variables_combinations_similarity,by = c("data", "translation_table_used"))
###

# Adding informative labels to linking and research population 
all_allowed_equatings <- all_allowed_equatings %>% 
  mutate(full_data = if_else(data =="full",paste0("Full population data"),"Subpopulation data"),
         harmonization_data = case_when(translation_table_used == "full" ~ paste0("full population recoding table"),
                                        data == translation_table_used ~ "fitting subpopulation recoding table",
                                        data == "full" & translation_table_used != "full" ~ "subpopulation recoding table",
                                        similarity > 0 ~ "subpopulation recoding table with partial similarity",
                                        T ~ "subpopulation recoding table with no similarity"),
         group_translation_table_combination = paste0(full_data," harmonized by\n",harmonization_data),
         streched_mean = map_dbl(equate_model_data,~stretchify(source_val(.x),target_val(.x)) %>% mean),
         cohen_d_to_target_abs_stretched = abs((streched_mean-mean_y)/sd_y))

# add group table combination shorthand
gtc_shorthand <- tibble(
  group_translation_table_combination = c(
    "Subpopulation data harmonized by\nfitting subpopulation recoding table",
    "Subpopulation data harmonized by\nsubpopulation recoding table with no similarity",
    "Full population data harmonized by\nsubpopulation recoding table",
    "Subpopulation data harmonized by\nfull population recoding table",
    "Full population data harmonized by\nfull population recoding table",
    "Subpopulation data harmonized by\nsubpopulation recoding table with partial similarity"
  ),
  gtc_shorthand = c("s_s_iden",
                    "s_s_d",
                    "f_s",
                    "s_f",
                    "f_f",
                    "s_s_inter")
)

analysis_data <- all_allowed_equatings %>%
  left_join(gtc_shorthand) %>%
  mutate(gtc_equal = if_else(gtc_shorthand %in% c("f_f","s_s_iden"),"identical","different"))

