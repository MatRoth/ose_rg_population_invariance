# Load data
# GESIS Panel
#extract gpanel dataset
gpanel_a1 <- read_sav("data/GPANEL_ZA5665_a1_v40-0-1.sav",user_na = T)
gpanel_d1 <- read_sav("data/GPANEL_ZA5665_d1_v40-0-1.sav",user_na = T)
gpanel_orig <- bind_rows(gpanel_a1,gpanel_d1)


## EVS 2017
evs_17 <- read_spss("data/ZA7500_v5-0-0.sav") %>% filter(country == 276)