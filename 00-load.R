require(plyr)
require(dplyr)

addMissingFactorLevels <- function(col){
  if(is.factor(col) && length(levels(col)) == 1)
    return(factor(col, levels = unique(c(levels(col), "Yes", "No"))))
  return(col)
}

read.csv(file = 'data/train.csv', header = TRUE) %>% select(-Id) %>%
  mutate_each(funs(ifelse(.==1, "Yes", "No")), starts_with('Soil_Type')) %>%
  mutate_each(funs(as.factor), starts_with('Soil_Type')) %>%
  mutate_each(funs(addMissingFactorLevels), starts_with('Soil_Type')) %>%
  
  mutate_each(funs(ifelse(.==1, "Yes", "No")), starts_with('Wilderness')) %>%
  mutate_each(funs(as.factor), starts_with('Wilderness')) %>%
  mutate_each(funs(addMissingFactorLevels), starts_with('Wilderness')) %>%
  
  mutate(Cover_Type = paste("Cover", Cover_Type, sep = "")) %>%
  mutate(Cover_Type = as.factor(Cover_Type)) -> data
