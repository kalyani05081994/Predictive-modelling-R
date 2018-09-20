#install.packages("e1071")
library("e1071")

#shows first few (5 usually) rows in dataset
head(iris,5)

#Can use a data.frame (or any R object for the matter) without specifying the object name
#e.g. instead of "iris$Species" we can use simply "Species"
attach(iris)

x <- subset(iris, select=-Species)
y <- Species

svm_model <- svm(Species ~ ., data=iris)
summary(svm_model)

#Alternatively pass x, y instead
svm_model1 <- svm(x,y)
summary(svm_model1)

"
Small C makes the cost of misclassificaiton low ('soft margin', thus allowing more of them for the sake of wider 'cushion'
Large C makes the cost of misclassification high ('hard margin', thus forcing the algorithm to explain the input data stricter and potentially overfit.
"

pred <- predict(svm_model1, x)

# timer for see the time taken by predictions
system.time(pred <- predict(svm_model1, x))

table(pred,y)

svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(svm_tune)

svm_model_after_tune <- svm(Species ~ ., data=iris, kernel="radial", cost=1, gamma=0.5)
summary(svm_model_after_tune)

pred <- predict(svm_model_after_tune,x)
system.time(predict(svm_model_after_tune,x))

"Use this variation [probability=TRUE] when Y is dichotomous (1/0, YES/NO type categorical variable)
Similar to Logistic Model

*******

svm_model2 <- svm(x,y,probability=TRUE)
predict(svm_model2,probability=TRUE)

*******
"

plot(svm_model_after_tune, iris, Petal.Width ~ Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))

"
## a simple plot example with less than 2 variables 
data(cats, package = 'MASS')
m <- svm(Sex~., data = cats)
plot(m, cats)
"