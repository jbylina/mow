require(caret)
require(klaR)
require(e1071)
require(dplyr)

e1071_nb  <- list(label = "Naive Bayes e1071",
                  library = "e1071",
                  loop = NULL,
                  type = c('Classification'),
                  parameters = data.frame(parameter = c('laplace'),
                                          class = c('numeric'),
                                          label = c('Laplace Correction')),
                  grid = function(x, y, len = NULL, search = "grid") 
                    expand.grid(laplace = 0),
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                    out <- naiveBayes( as.data.frame(x), y, laplace = param$laplace, ...)
                  },
                  predict = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    predict(modelFit , newdata)
                  },
                  prob = function(modelFit, newdata, submodels = NULL) {
                    if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
                    predict(modelFit, newdata, type = "raw")
                  },
                  predictors = function(x, ...) if(hasTerms(x)) predictors(x$terms) else x$varnames,
                  tags = c("Bayesian Model"),
                  levels = function(x) x$levels,
                  sort = function(x) x[order(x[,1]),])

trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
laplace <- seq(0, 5, 0.5)

# e1071_nb train
e1071_fit <- train(select(data, -Cover_Type),
                   data$Cover_Type,
                   method = e1071_nb,
                   trControl = trControl,
                   tuneGrid = expand.grid(.laplace = laplace))

# klaR train
klar_fit <- train(select(data, -Cover_Type),
                  data$Cover_Type,
                  method = 'nb',
                  trControl = trControl,
                  tuneGrid =  expand.grid(.fL = laplace, .usekernel = c(TRUE, FALSE), .adjust = 1))

# transform binary cols to one factor
data %>% select(starts_with('Soil_Type')) %>%
  cbind(Soil_Type = names(.[-1L])[max.col(.[-1L] == 1L)]) %>%
  select(Soil_Type) -> Soil_Type
data %>% select(starts_with('Wilderness_Area')) %>%
  cbind(Wilderness_Area = names(.[-1L])[max.col(.[-1L] == 1L)]) %>%
  select(Wilderness_Area) -> Wilderness_Area
data %>% select(-starts_with('Soil_Type'), -starts_with('Wilderness_Area')) %>%
  bind_cols(Soil_Type, Wilderness_Area) -> data_bayes
rm(Soil_Type, Wilderness_Area)


e1071_fit_2 <- train(select(data_bayes, -Cover_Type),
                     data_bayes$Cover_Type,
                     method = e1071_nb,
                     trControl = trControl,
                     tuneGrid = expand.grid(.laplace = 0.5))

