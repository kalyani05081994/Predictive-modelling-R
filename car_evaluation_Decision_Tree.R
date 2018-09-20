data_url <- c("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data")
download.file(url = data_url, destfile = "car.data")
#we have unsupervised data:labels are unknown
car_df <- read.csv("car.data", sep = ',', header = FALSE)
car_df


str(car_df)
head(car_df)

set.seed(3033)

#install.packages("caret")#provides all types of regression models
library(caret)

#install.packages("rpart.plot")
library(rpart.plot)

intrain <- createDataPartition(y = car_df$V7, p= 0.7, list = FALSE)
training <- car_df[intrain,]
testing <- car_df[-intrain,]

#check dimensions of train & test set
dim(training); dim(testing);

anyNA(car_df)#false or true value for NA presence

summary(car_df)


#Training as criterion as INFORMATION GAIN
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(V7 ~., data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
dtree_fit#in results kappa should be highest,highest value is selected and cp should be lowest,
#cp:complexity parameter:its value puts constraint on growth of decision tree,internal default value is there if the cp value goes below that the tree stops
#accuracy value contributes in selection of model.highest value is used to select model
#cp:difference between r-squared values of models.
#in random forest number of decision trees to include is a flexible attribute
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)


testing[1,]

predict(dtree_fit, newdata = testing[1,])

test_pred <- predict(dtree_fit, newdata = testing)
confusionMatrix(test_pred, testing$V7 )  #check accuracy

#Training as criterion as GINI INDEX
set.seed(3333)
dtree_fit_gini <- train(V7 ~., data = training, method = "rpart",
                          parms = list(split = "gini"),
                          trControl=trctrl,
                          tuneLength = 10)
dtree_fit_gini
prp(dtree_fit_gini$finalModel, box.palette = "Blues", tweak = 1.2)


test_pred_gini <- predict(dtree_fit_gini, newdata = testing)
confusionMatrix(test_pred_gini, testing$V7 )  #check accuracy



