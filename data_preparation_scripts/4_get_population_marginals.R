##########
# age, sex, bundesland (federal state of Germany)

socio_demo <- readxl::read_xlsx("data/official_statistics/12411-0013_age_sex_federalstate.xlsx")
socio_demo <- socio_demo[-c(1, 2, 5), ]
socio_demo[2, 1] <- "Geschlecht"
socio_demo[1, 1] <- "Bundesland"
fed_states <- socio_demo[1, ][-1] %>%
  str_extract("^.*") %>%
  na.omit() %>%
  map(~ rep(.x, 3)) %>%
  unlist()
col_names <- tibble(
  fed = fed_states %>% tolower(),
  sex = socio_demo[2, -1] %>% as.character() %>% tolower(),
  name = map2_chr(fed, sex, ~ paste0(.x, "_", .y))
)
socio_demo <- socio_demo[-c(1, 2), ]
colnames(socio_demo) <- c("age", col_names$name)
socio_demo <- socio_demo %>%
  pivot_longer(-age) %>%
  separate(col = name, into = c("state", "sex"), sep = "_") %>%
  mutate(
    age_num = parse_number(age),
    age_cat = cut(age_num,
      breaks = breaks_age,
      labels = labels_age
    )
  )
east <- c("brandenburg", "mecklenburg-vorpommern", "sachsen-anhalt", "sachsen", "tühringen", "berlin")
socio_demo <- socio_demo %>% mutate(east_west = if_else(state %in% east, "east", "west"))

# removing under 20
socio_demo <- socio_demo %>% filter(age_cat != "unter 20")



# read in education
edu <- readxl::read_xlsx("data/official_statistics//12211-9012_education.xlsx")
edu <- edu %>%
  separate(Alter, into = c("lower", "upper"), sep = "bis unter") %>%
  mutate(lower = parse_number(lower), upper = parse_number(upper) - 1)
edu[c(11, 22, 33), "upper"] <- 100
edu <- edu %>% mutate(age_cat = cut(upper,
  breaks = breaks_age,
  labels = labels_age
)) # taken from add_sociodemographics
# removing under 20
edu <- edu %>% filter(age_cat != "unter 20")

####### getting socio-demo population margins
# getting population size of east west/sex/age data
socio_demo_pop_size <- socio_demo %>%
  filter(sex == "insgesamt") %>%
  pull(value) %>%
  as.numeric() %>%
  reduce(sum)

# east west
east_west_marg_pop <- socio_demo %>%
  filter(sex == "insgesamt") %>%
  group_by(east_west) %>%
  summarize(Freq = sum(as.numeric(value)) / 67540025)

## sex
sex_marg_pop <- socio_demo %>%
  filter(sex != "insgesamt") %>%
  group_by(sex) %>%
  summarize(Freq = sum(as.numeric(value)) / 67540025) %>%
  mutate(sex = c("male", "female"))


# age
age_cat_marg_pop <- socio_demo %>%
  filter(sex == "insgesamt") %>%
  group_by(age_cat) %>%
  summarize(Freq = sum(as.numeric(value)) / 67540025)

# edu
# getting population size of edu data
edu_pop_size <- edu %>%
  filter(Geschlecht == "Insgesamt") %>%
  select(`Fachhochschul- oder Hochschulreife`, `Keine Hochschulreife`) %>%
  pivot_longer(everything()) %>%
  pull(value) %>%
  reduce(sum) * 1000

edu_marg_pop <- edu %>%
  filter(Geschlecht == "Insgesamt") %>%
  select(age_cat, `Fachhochschul- oder Hochschulreife`, `Keine Hochschulreife`) %>%
  pivot_longer(-c(age_cat)) %>%
  mutate(value = value * 1000) %>%
  group_by(name) %>%
  summarize(Freq = sum(value) / edu_pop_size) %>%
  mutate(edu = if_else(name == "Fachhochschul- oder Hochschulreife", "Abitur", "kein Abitur")) %>%
  select(-name) %>%
  select(edu, Freq)



pop_marg <- list(east_west_marg_pop, sex_marg_pop, age_cat_marg_pop, edu_marg_pop) %>% map(~ .x %>% mutate(Freq = Freq / sum(Freq)))
