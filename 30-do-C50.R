require(caret)
require(plyr)
require(dplyr)
require(C50)

trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

rpart_fit <- train(select(data, -Cover_Type),
                   data$Cover_Type,
                   method = "C5.0",
                   trControl = trControl,
                   tuneGrid = expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1, 5, 10, 15, 20, 30, 40, 50), .model="tree"))

save(rpart_fit, file = "rpart_fit.Rdata")




require(ggplot2)
require(caret)
require(reshape2)

load("rpart_fit.Rdata")


plot_conf_matrix(rpart_fit)

res_plot <- ggplot(rpart_fit) +
  theme(legend.text=element_text(size=15),
        legend.position="bottom")
print(res_plot)

