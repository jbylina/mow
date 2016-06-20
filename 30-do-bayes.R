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
require(dplyr)

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



