# Classification Tree with rpart
library("rpart")
library("gmodels")

normalize <- function(x) {
  #You can normalize the data. However, as classification tree doesn't use the distance, normalization is of less use.
  mean_x <- mean(x)
  stdev_x <- sd(x)*sqrt((length(x)-1)/(length(x)))
  
  num <- x - mean_x
  denom <- stdev_x
  return (num/denom)
}

iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE) #There are great sample data offered by UCI. Let's use this!

names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
#Unlike KNN, you don't need to seperate the label from the training data because the machine needs to learn
iris.training <- iris[ind==1, 1:5]
#However, just like KNN, you need to seperate the label from the test data
iris.test <- iris[ind==2, 1:4]
iris.testLabels <- iris[ind==2, 5]

# grow tree 
# y ~ x1 + x2 + x3+ x4
# y: what we want to know
# x1, x2, x3, x4: what we know
fit <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
             method="class", data=iris.training)

# plot tree 
plot(fit, uniform=TRUE, main="Classification Tree for Iris")
text(fit, use.n=TRUE, all=TRUE, cex=.65, pos=1)

# create attractive postscript plot of tree 
iris_pred <- predict(fit, iris.test, type = "class")

#Confusion Matrix
CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)
