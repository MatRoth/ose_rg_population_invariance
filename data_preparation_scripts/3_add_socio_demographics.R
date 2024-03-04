########  Create identical socio-demographic grouping variables for GPANEL and EVS
#### age
breaks_age <- c(0,19,29,39,49,59,69,100)
labels_age <- c("unter 20",
                "20 bis 29",
                "30 bis 39",
                "40 bis 49",
                "50 bis 59",
                "60 bis 69",
                "70 oder älter")

########
#gpanel age & east west
gpanel_age <- gpanel_orig %>%
  select(z000001a,a11d056b,d11d056b) %>% 
  mutate(across(c(a11d056b,d11d056b),~ifelse(.x<0,NA,.x))) %>% 
  pivot_longer(a11d056b:d11d056b) %>% 
  drop_na(value) %>% 
  select(z000001a,age=value)



#extract single row east_west variable 
#no east/west variable available in refreshment wave f11/12
gpanel_east_west <- gpanel_orig %>%
  select(z000001a,a12d021b,d12d025b)  %>% 
  mutate(across(c(a12d021b,d12d025b),~case_when(.x == 1 ~ "west",
                                                .x == 2 ~ "east"))) %>% 
  pivot_longer(a12d021b:d12d025b) %>% 
  drop_na(value) %>% 
  select(z000001a,east_west=value)
#
#append full_gpanel dataset with age variable
full_gpanel <- gpanel_orig %>%
  full_join(gpanel_age) %>% 
  full_join(gpanel_east_west)

### Add harmonized grouping variables 
evs_17 <- evs_17 %>% mutate(
  #add east west variable
  bundesland = v275c_N1 %>% as_factor() %>% as.character(),
  east_west = case_when(bundesland %in% c("DE: BERLIN",
                                          "DE: SACHSEN",
                                          "DE: SACHSEN-ANHALT",
                                          "DE: MECKLENBURG-VORPOMMERN",
                                          "DE: THÜRINGEN",
                                          "DE: BRANDENBURG") ~ "east",
                        TRUE ~ "west"),
  #add age variable
  age_cat = cut(age,
                breaks = breaks_age,
                labels = labels_age))
#add age variable
full_gpanel <- full_gpanel %>% mutate(age_cat = cut(2017-age,
                                                    breaks = breaks_age,
                                                    labels = labels_age))




#sex 
full_gpanel <- full_gpanel %>% mutate(wave_1_sex =  case_when(a11d054a < 1 ~NA_character_,
                                                              a11d054a == 1 ~"male",
                                                              a11d054a == 2 ~"female",
                                                              T ~NA_character_),
                                      wave_2_sex = case_when(d12d088a < 1 ~NA_character_,
                                                             d12d088a == 1 ~"male",
                                                             d12d088a == 2 ~"female",
                                                             T ~NA_character_)) %>% 
  unite("sex",wave_1_sex:wave_2_sex,na.rm=T,remove=F) %>% mutate(sex = if_else(sex %in% c("male","female"),sex,NA_character_))

evs_17 <- evs_17 %>%  mutate(sex = case_when(v225 == 1 ~ "male",
                                             v225 == 2 ~ "female",
                                             TRUE ~ NA_character_))
#education (no abi / abi )
full_gpanel <- full_gpanel %>% mutate(wave_1_edu = case_when(a11d082a <1 ~ NA_character_,
                                                             a11d082a <7 ~ "kein Abitur",
                                                             a11d082a %in% c(7,8) ~ "Abitur",
                                                             TRUE ~ NA_character_),
                                      wave_2_edu = case_when(d11d082a <1 ~ NA_character_,
                                                             d11d082a <7 ~ "kein Abitur",
                                                             d11d082a %in% c(7,8)  ~ "Abitur",
                                                             TRUE ~ NA_character_)) %>% 
  unite("edu",wave_1_edu:wave_2_edu,na.rm=T,remove =F) %>% mutate(edu = if_else(edu %in% c("kein Abitur", "Abitur"),edu,NA_character_))

evs_17 <- evs_17 %>% mutate(edu = case_when(v243_cs_DE1<0 ~NA_character_,
                                            v243_cs_DE1 %in% 27601:27605 ~ "kein Abitur",
                                            v243_cs_DE1 == 27606 ~ "Abitur",
                                            TRUE ~ NA_character_))


full_gpanel<-full_gpanel %>% mutate(age_cat_f =   factor(age_cat),
                                    edu_f =       factor(edu),
                                    sex_f =       factor(sex),
                                    east_west_f = factor(east_west),
                                    age_cat_i =   as.integer(age_cat_f),
                                    edu_i =       as.integer(edu_f),
                                    sex_i =       as.integer(sex_f),
                                    east_west_i = as.integer(east_west_f))

evs_17<-evs_17 %>% mutate(age_cat_f =   factor(age_cat),
                                    edu_f =       factor(edu),
                                    sex_f =       factor(sex),
                                    east_west_f = factor(east_west),
                                    age_cat_i =   as.integer(age_cat_f),
                                    edu_i =       as.integer(edu_f),
                                    sex_i =       as.integer(sex_f),
                                    east_west_i = as.integer(east_west_f))