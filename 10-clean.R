require(caret)

pre_proc_range <- preProcess(data, method = c('range'))
pre_proc_scale <- preProcess(data, method = c('center', 'scale'))

# apply preProcess to data
data_range <- predict(pre_proc_range, data)
data_scale <- predict(pre_proc_scale, data)

# divide traing set
intrain <- createDataPartition(y=data$Cover_Type, p = 0.7, list = FALSE)

# divide range traing set
test_range <- data_range[-intrain,]
train_range <- data_range[intrain,]

# divide scale traing set
test_scale <- data_scale[-intrain,]
train_scale <- data_scale[intrain,]

rm(intrain)