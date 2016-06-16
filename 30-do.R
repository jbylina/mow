require(class)

clTrain<-train[, ncol(train)]
clTest<-test[, ncol(test)]

train<-select(train, -Cover_Type)
test<-select(test, -Cover_Type)

test<-test[, c(2:11)]
train<-train[, c(2:11)]

knn_pred <- knn(train = select(train_scale, -Cover_Type),
                test = select(test_scale, -Cover_Type),
                cl = train_scale$Cover_Type,
                k = 1,
                l = 0,
                prob = FALSE,
                use.all = TRUE)

mean(knn_pred==test_scale$Cover_Type)
conf_mat <-table(test_scale$Cover_Type, as.numeric(knn_pred))
