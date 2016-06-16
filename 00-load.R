require(dyplr)

read.csv(file = 'data/train.csv', header = TRUE) %>%
  select(-Id) %>%
  mutate_each(funs(as.logical), starts_with('Soil_Type')) %>%
  mutate_each(funs(as.logical), starts_with('Wilderness')) %>%
  mutate(Cover_Type = as.factor(Cover_Type)) -> data