library(dyplr)
library(caret)
library(randomForest)

read.csv(file = 'data/train.csv', header = TRUE) %>%
select(-Id) %>%
mutate_each(funs(as.logical), starts_with('Soil_Type')) %>%
mutate_each(funs(as.logical), starts_with('Wilderness')) %>%
mutate(Cover_Type = as.factor(Cover_Type)) -> train

pre_proc_range <- preProcess(train, method = c('range'))
pre_proc_scale <- preProcess(train, method = c('center', 'scale'))

# apply preProcess to data
train_range <- predict(pre_proc_range, train)
train_scale <- predict(pre_proc_scale, train)

# divide traing set
intrain <- createDataPartition(y=train$Cover_Type, p = 0.7, list = FALSE)

# divide range traing set
test_range <- train_range[-intrain,]
train_range <- train_range[intrain,]

# divide scale traing set
test_scale <- train_scale[-intrain,]
train_scale <- train_scale[intrain,]


# 
rf_range <- randomForest(Cover_Type ~ ., data=train_range, importance=TRUE, ntree = 2000)
pred <- predict(rf_range, newdata = test_range)

rf_scale <- randomForest(Cover_Type ~ ., data=train_scale, importance=TRUE, ntree = 2000)
pred <- predict(rf_range, newdata = test_scale)

#============================

submission <- read.csv(file = 'data/test.csv', header = TRUE)
submission %>% select(Id) -> ids

submission %>%
  select(-Id) %>%
  mutate_each(funs(as.logical), starts_with('Soil_Type')) %>%
  mutate_each(funs(as.logical), starts_with('Wilderness')) -> submission

submission <- predict(pre_pro, submission)

submission_pred <- predict(rf, newdata = submission)

ids['Cover_Type'] <- submission_pred

write.csv(ids, file = 'data/submission.csv')


