require(caret)
require(klaR)
require(e1071)
require(dplyr)
require(FSelector)
require(corrplot)
require(plyr)

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

save(e1071_fit, file = "e1071_fit.Rdata")

# klaR train
klar_fit <- train(select(data, -Cover_Type),
                  data$Cover_Type,
                  method = 'nb',
                  trControl = trControl,
                  tuneGrid =  expand.grid(.fL = laplace, .usekernel = c(TRUE, FALSE), .adjust = 1))

save(klar_fit, file = "klar_fit.Rdata")

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
                     tuneGrid = expand.grid(.laplace = laplace))

save(e1071_fit_2, file = "e1071_fit_2.Rdata")

require(ggplot2)

load("e1071_fit.Rdata")
load("e1071_fit_2.Rdata")
load("klar_fit.Rdata")

plot_conf_matrix(e1071_fit)
plot_conf_matrix(e1071_fit_2)
plot_conf_matrix(klar_fit)

e1071_fit$results$model <- 'e1071_fit'
e1071_fit$results$usekernel <- FALSE
e1071_fit_2$results$model <- 'e1071_fit_2'
e1071_fit_2$results$usekernel <- FALSE

klar_fit$results$model <- paste('klar_fit', 'usekernel', klar_fit$results$usekernel)
names(klar_fit$results)[names(klar_fit$results)=="fL"] <- "laplace"


union_fit <- union(e1071_fit$results, e1071_fit_2$results)
union_fit <- bind_rows(union_fit, klar_fit$results)
union_fit$model <- as.factor(union_fit$model)

bayes_plot <- ggplot(data = union_fit, aes(x=laplace, y=Accuracy, group= model, colour= model)) +
  geom_line() +
  theme(legend.text=element_text(size=9),
        legend.position="bottom") +
  scale_x_continuous(breaks = seq(min(union_fit$laplace), max(union_fit$laplace), by = 0.5)) +
  geom_point()

print(bayes_plot)

# klar po agregacji atrybutow binarnych
klar_fit_2 <- train(select(data_bayes, -Cover_Type),
                    data$Cover_Type,
                    method = 'nb',
                    trControl = trControl,
                    tuneGrid =  expand.grid(.fL = laplace, .usekernel = c(TRUE, FALSE), .adjust = 1))

save(klar_fit_2, file = "klar_fit_2.Rdata")

# najlepszy podzbior atrybutow wedlug cfs
subset <- cfs(Cover_Type~., data)
f <- as.simple.formula(subset, "Cover_Type")
print(f)
data %>% select(c(Elevation, Wilderness_Area4, Cover_Type)) -> data_cfs
klar_fit_cfs <- train(select(data_cfs, -Cover_Type),
                      data$Cover_Type,
                      method = 'nb',
                      trControl = trControl,
                      tuneGrid =  expand.grid(.fL = laplace, .usekernel = c(TRUE, FALSE), .adjust = 1))

save(klar_fit_cfs, file = "klar_fit_cfs.Rdata")

# selekcja atrybutow na podstawie information gain
weights <- information.gain(Cover_Type~., data)
print(weights)
sub_ig <- cutoff.k(weights, 50)
f_ig <- as.simple.formula(sub_ig, "Cover_Type")
print(f_ig)
data_ig <- model.frame(f_ig, data)
klar_fit_ig <- train(select(data_ig, -Cover_Type),
                     data$Cover_Type,
                     method = 'nb',
                     trControl = trControl,
                     tuneGrid =  expand.grid(.fL = laplace, .usekernel = c(TRUE, FALSE), .adjust = 1))

# selekcja na podstawie korelacji wzajemnej
data %>% select(-starts_with('Soil_Type'), -starts_with('Wilderness_Area')) -> d
data_scale <- scale(select(d, -Cover_Type), center=TRUE, scale=TRUE)
cor_mtx <- cor(data_scale)
corrplot(cor_mtx, method="circle")
data %>% select(-Hillshade_3pm, -Horizontal_Distance_To_Hydrology, -Horizontal_Distance_To_Roadways, -Hillshade_9am) -> data_cor
klar_fit_cor <- train(select(data_cor, -Cover_Type),
                      data$Cover_Type,
                      method = 'nb',
                      trControl = trControl,
                      tuneGrid =  expand.grid(.fL = laplace, .usekernel = c(TRUE, FALSE), .adjust = 1))

#==========================================================================================================================

read.csv(file = 'data/test.csv', header = TRUE) %>%
  mutate_each(funs(ifelse(.==1, "Yes", "No")), starts_with('Soil_Type')) %>%
  mutate_each(funs(as.factor), starts_with('Soil_Type')) %>%
  mutate_each(funs(addMissingFactorLevels), starts_with('Soil_Type')) %>%
  
  mutate_each(funs(ifelse(.==1, "Yes", "No")), starts_with('Wilderness')) %>%
  mutate_each(funs(as.factor), starts_with('Wilderness')) %>%
  mutate_each(funs(addMissingFactorLevels), starts_with('Wilderness')) -> test_data

load("klar_fit.Rdata")

start.time <- Sys.time()
klar_fit_pred <- predict(klar_fit$finalModel, newdata = select(test_data, -Id))
end.time <- Sys.time()
cat(sprintf("Time elapsed %f", end.time - start.time))

ids <- select(test_data, Id)
ids['Cover_Type'] <- as.numeric(klar_fit_pred$class)
write.csv(ids, file = 'kaggle/klar_fit.csv', row.names = FALSE)

start.time
end.time
