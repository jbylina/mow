require(caret)
require(dplyr)

trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# suppress "These variables have zero variances: Soil_Type7Yes, Soil_Type15Yes"
# "Cover_Type ~ ." powoduje zamianę data frame na obiekt matrix. W konsekwencji zmienne typu factor są zamieniane na numeric

print.noquote("knn_raw_fit - stared")
knn_raw_fit <- suppressWarnings(train(Cover_Type ~ .,
                                      data = data,
                                      method = "knn",
                                      trControl = trControl,
                                      tuneGrid = expand.grid(.k=1:25)))

print.noquote("knn_raw_fit - finished")
save(knn_raw_fit, file = "knn_raw_fit.Rdata")

knn_normalize_fit <- suppressWarnings(train(Cover_Type ~ .,
                                            data = data,
                                            method = "knn",
                                            preProcess = c('center', 'scale'),
                                            trControl = trControl,
                                            tuneGrid = expand.grid(.k=1:25)))

print.noquote("knn_normalize_fit - finished")
save(knn_normalize_fit, file = "knn_normalize_fit.Rdata")

knn_selected_fit <- suppressWarnings(train(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology +
                                           Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways +
                                           Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points,
                                           data = data,
                                           method = "knn",
                                           preProcess = c('center', 'scale'),
                                           trControl = trControl,
                                           tuneGrid = expand.grid(.k=1:25)))

print.noquote("knn_selected_fit - finished")
save(knn_selected_fit, file = "knn_selected_fit.Rdata")
