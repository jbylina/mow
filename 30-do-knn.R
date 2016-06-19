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


knn_range_fit <- suppressWarnings(train(Cover_Type ~ .,
                                            data = data,
                                            method = "knn",
                                            preProcess = c('range'),
                                            trControl = trControl,
                                            tuneGrid = expand.grid(.k=1:25)))

print.noquote("knn_range_fit - finished")
save(knn_range_fit, file = "knn_range_fit.Rdata")


knn_selected <- data[, c(1:10)]
pre_proc_range <- preProcess(knn_selected, method = c('range'))
knn_selected <- predict(pre_proc_range, knn_selected)
knn_selected$Elevation <- knn_selected$Elevation * 1733.5836228554
knn_selected$Aspect <- knn_selected$Aspect * 268.802531967241
knn_selected$Slope <- knn_selected$Slope * 192.87045143639
knn_selected$Horizontal_Distance_To_Hydrology <- knn_selected$Horizontal_Distance_To_Hydrology * 383.396957105765
knn_selected$Vertical_Distance_To_Hydrology <- knn_selected$Vertical_Distance_To_Hydrology * 301.920595572625
knn_selected$Horizontal_Distance_To_Roadways <- knn_selected$Horizontal_Distance_To_Roadways * 588.897319265439
knn_selected$Hillshade_9am <- knn_selected$Hillshade_9am * 302.820051815842
knn_selected$Hillshade_Noon <- knn_selected$Hillshade_Noon * 245.664563331312
knn_selected$Hillshade_3pm <- knn_selected$Hillshade_3pm * 257.573236936878
knn_selected$Horizontal_Distance_To_Fire_Points <- knn_selected$Horizontal_Distance_To_Fire_Points * 425.502238470866

knn_selected <- bind_cols(knn_selected, select(data, Cover_Type))

knn_selected_fit <- suppressWarnings(train(Cover_Type ~ Elevation + Aspect + Slope + Horizontal_Distance_To_Hydrology +
                                           Vertical_Distance_To_Hydrology + Horizontal_Distance_To_Roadways +
                                           Hillshade_9am + Hillshade_Noon + Hillshade_3pm + Horizontal_Distance_To_Fire_Points,
                                           data = knn_selected,
                                           method = "knn",
                                           trControl = trControl,
                                           tuneGrid = expand.grid(.k=1:25)))

print.noquote("knn_selected_fit - finished")
save(knn_selected_fit, file = "knn_selected_fit.Rdata")
