#K-nearest Analysis algorithm
library("class") #If you don't have it, please install
library("gmodels")

normalize <- function(x) {
  #If we don't normalized it, some distance is far longer than others, which dominates the model.
  mean_x <- mean(x)
  stdev_x <- sd(x)*sqrt((length(x)-1)/(length(x)))
  
  num <- x - mean_x
  denom <- stdev_x
  return (num/denom)
}

iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE) #There are great sample data offered by UCI. Let's use this!

#Unfortunately, this 
names(iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

#If you want to normalize all data that you have, "lapply" is the greatest way to apply the function to all your data. I want to normalize all my data

iris_norm <- as.data.frame(lapply(iris[1:4], normalize))

#Now, we are going to split up the data into two sets. - Training and test
#Training allows the computer to learn the pattern of the data
#Test allows us to validate how accurate our model is.

set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
iris.training <- iris_norm[ind==1, 1:4]
iris.test <- iris_norm[ind==2, 1:4]
iris.trainLabels <- iris[ind==1, 5]
iris.testLabels <- iris[ind==2, 5]

#K-nearest Analysis
#K=3, Use nearest 3 points to classify unknown subject
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)

#Confusion Matrix
CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)
