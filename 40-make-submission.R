require(caret)
require(plyr)
require(dplyr)
require(e1071)

read.csv(file = 'data/test.csv', header = TRUE) %>%
  mutate_each(funs(ifelse(.==1, "Yes", "No")), starts_with('Soil_Type')) %>%
  mutate_each(funs(as.factor), starts_with('Soil_Type')) %>%
  mutate_each(funs(addMissingFactorLevels), starts_with('Soil_Type')) %>%
  
  mutate_each(funs(ifelse(.==1, "Yes", "No")), starts_with('Wilderness')) %>%
  mutate_each(funs(as.factor), starts_with('Wilderness')) %>%
  mutate_each(funs(addMissingFactorLevels), starts_with('Wilderness')) -> test_data

load("e1071_fit.Rdata")

start.time <- Sys.time()
e1071_fit_pred <- predict(e1071_fit$finalModel, newdata = select(test_data, -Id))
end.time <- Sys.time()
cat(sprintf("Time elapsed %f", end.time - start.time))

ids <- select(test_data, Id)
ids['Cover_Type'] <- as.numeric(e1071_fit_pred)
write.csv(ids, file = 'kaggle/e1071_fit.csv', row.names = FALSE)

start.time
end.time

predict(e1071_fit, select(test_data, -Id))

create_kaggle_submission(test_data$Id, tmp, 'kaggle/e1071_fit.csv')

