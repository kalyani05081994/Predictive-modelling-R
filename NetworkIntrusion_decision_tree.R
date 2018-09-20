train.data <- read.csv("C:/Users/00008020/Desktop/R_projects/data_excel/net_work_intrusion/train.csv")
test.data <- read.csv("C:/Users/00008020/Desktop/R_projects/data_excel/net_work_intrusion/test.csv")

#service_levels <- union(levels(test.data$service), levels(train.data$service))
#test.data$service <- with(test.data, factor(service, levels = service_levels)) 
#train.data$service <- with(train.data, factor(service, levels = service_levels)) 

#str(train.data$service)
#str(test.data$service)

#check dimensions of train & test set
dim(train.data)

anyNA(train.data)


summary(train.data)

set.seed(3033)
library(caret)
library(rpart.plot)

#https://www.rdocumentation.org/packages/caret/versions/6.0-80/topics/trainControl
#Training as criterion as INFORMATION GAIN
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(3333)



#https://www.rdocumentation.org/packages/caret/versions/6.0-80/topics/train
dtree_fit <- train(class ~., data = train.data, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)

 dtree_fit

prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)

test_pred <- predict(dtree_fit, newdata = test.data)

test_pred1 <- predict(dtree_fit, newdata = train.data)

confusionMatrix(test_pred1, train.data$class )  #check accuracy


#*****************Training as criterion as GINI INDEX**************************
set.seed(3333)
dtree_fit_gini <- train(class ~., data = train.data, method = "rpart",
                        parms = list(split = "gini"),
                        trControl=trctrl,
                        tuneLength = 10)
dtree_fit_gini
prp(dtree_fit_gini$finalModel, box.palette = "Blues", tweak = 1.2)


test_pred_gini <- predict(dtree_fit_gini, newdata = test.data)

test_pred_gini1 <- predict(dtree_fit_gini, newdata = train.data)
confusionMatrix(test_pred_gini1, train.data$class )  #check accuracy


#*****************************Random Forest*************************************
#NOTE - This may go out of memory (cannot allocate memmory for large vector)

library(randomForest)

set.seed(415)
head(train.data)
str(train.data)

#remove service column as the levels cannot be reduced to meaningful leveles below 32
train.data <- train.data[,-3]
test.data <- test.data[,-3]

fit <- randomForest(as.factor(class) ~ ., data=train.data, importance=TRUE, ntree=2000)

# shows whivh variables are important based on accuracy and gini index
varImpPlot(fit)

#may take  longer as 2000 trees need to predic and decide the best
Prediction <- predict(fit, test.data)


