prueba <- data_subset.df %>%
  as_survey(weights = weight) %>%
  group_by(gender) %>%
  summarise(prevalence = survey_mean(had_dispute, na.rm = T)) %>%
  drop_na()

prueba2 <- data_subset.df %>%
  #as_survey(weights = weight) %>%
  group_by(gender) %>%
  summarise(prevalence = mean(had_dispute, na.rm = T)) %>%
  drop_na()

prueba3 <- data_subset.df %>%
  mutate(
    prevalence = had_dispute*weight
  ) %>%
  summarize(prevalence = mean(prevalence, na.rm = T))
  
prueba3 <- data_subset.df %>% 
  filter(gender == "Female") %>%
  mutate(
    prevalence = had_dispute*weight,
    female_W = sum(weight)
  ) %>%
  summarise(final = sum(prevalence, na.rm =T) / female_W) %>%
  unique()
