require(caret)
require(plyr)
require(dplyr)
require(randomForest)

# based on https://github.com/topepo/caret/blob/master/models/files/rf.R
rf     <-    list(label = "Random Forest",
                  library = "randomForest",
                  loop = NULL,
                  type = c("Classification", "Regression"),
                  parameters = data.frame(parameter = c('mtry', 'ntree'),
                                          class = c('numeric', 'numeric'),
                                          label = c('#Randomly Selected Predictors', '#Number of trees')),
                  grid = function(x, y, len = NULL, search = "grid") {
                    if(search == "grid") {
                      out <- expand.grid(mtry = caret::var_seq(p = ncol(x), 
                                                              classification = is.factor(y), 
                                                              len = len),
                                         ntree = c(1000, 1500, 2000, 2500))
                    } else {
                      out <- expand.grid(mtry = unique(sample(1:ncol(x), size = len, replace = TRUE)),
                                         ntree = c(1000, 1500, 2000, 2500))
                    }
                    out
                  },
                  fit = function(x, y, wts, param, lev, last, classProbs, ...) 
                    randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...),
                  predict = function(modelFit, newdata, submodels = NULL) 
                    if(!is.null(newdata)) predict(modelFit, newdata) else predict(modelFit),
                  prob = function(modelFit, newdata, submodels = NULL)
                    if(!is.null(newdata)) predict(modelFit, newdata, type = "prob") else predict(modelFit, type = "prob"),
                  predictors = function(x, ...) {
                    ## After doing some testing, it looks like randomForest
                    ## will only try to split on plain main effects (instead
                    ## of interactions or terms like I(x^2).
                    varIndex <- as.numeric(names(table(x$forest$bestvar)))
                    varIndex <- varIndex[varIndex > 0]
                    varsUsed <- names(x$forest$ncat)[varIndex]
                    varsUsed
                  },
                  varImp = function(object, ...){
                    varImp <- randomForest::importance(object, ...)
                    if(object$type == "regression")
                      varImp <- data.frame(Overall = varImp[,"%IncMSE"])
                    else {
                      retainNames <- levels(object$y)
                      if(all(retainNames %in% colnames(varImp))) {
                        varImp <- varImp[, retainNames]
                      } else {
                        varImp <- data.frame(Overall = varImp[,1])
                      }
                    }
                    
                    out <- as.data.frame(varImp)
                    if(dim(out)[2] == 2) {
                      tmp <- apply(out, 1, mean)
                      out[,1] <- out[,2] <- tmp  
                    }
                    out
                  },
                  levels = function(x) x$classes,
                  tags = c("Random Forest", "Ensemble Model", "Bagging", "Implicit Feature Selection"),
                  sort = function(x) x[order(x[,1]),],
                  oob = function(x) {
                    out <- switch(x$type,
                                  regression =   c(sqrt(max(x$mse[length(x$mse)], 0)), x$rsq[length(x$rsq)]),
                                  classification =  c(1 - x$err.rate[x$ntree, "OOB"],
                                                      e1071::classAgreement(x$confusion[,-dim(x$confusion)[2]])[["kappa"]]))
                    names(out) <- if(x$type == "regression") c("RMSE", "Rsquared") else c("Accuracy", "Kappa")
                    out
                  })

trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

#create model
rf_fit <- train(select(data, -Cover_Type),
                data$Cover_Type,
                method = rf,
                trControl = trControl)

save(rf_fit, file = "rf_fit.Rdata")

rf_var_importance <- randomForest(select(data, -Cover_Type),
                                  data$Cover_Type,
                                  mtry = rf_fit$bestTune$mtry,
                                  ntree = rf_fit$bestTune$ntree,
                                  importance = TRUE)

save(rf_var_importance, file = "rf_var_importance.Rdata")




require(randomForest)
require(ggplot2)

load("rf_fit.Rdata")
load("rf_var_importance.Rdata")

plot_conf_matrix(rf_fit)


plot(rf_fit)

layout(matrix(c(1,2),nrow=1),width=c(4,1)) 
par(mar=c(5,4,4,0)) #No margin on the right side
plot(rf_fit$finalModel, col=1:8, main = "Final model", log="y")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rf_fit$finalModel$err.rate),col=1:8,cex=0.8,fill=1:8)


varImpPlot(rf_var_importance, type = 2, n.var = 20)


varImpPlot(rf_var_importance, type = 1, n.var = 20)





