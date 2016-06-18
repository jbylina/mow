require(caret)
require(dplyr)

trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# suppress "These variables have zero variances: Soil_Type7Yes, Soil_Type15Yes"
# "Cover_Type ~ ." powoduje zamianę data frame na obiekt matrix. W konsekwencji zmienne typu factor są zamieniane na numeric
knn_fit <- suppressWarnings(train(Cover_Type ~ .,
                                  data = data,
                                  method = "knn",
                                  preProcess = c('center', 'scale'),
                                  trControl = trControl,
                                  tuneGrid=expand.grid(.k=1:25)))
