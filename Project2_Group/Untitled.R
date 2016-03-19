mydata <- read.table('bank.csv', sep = ';',header = TRUE)

library(mboost)
library(adabag)
library(sampling)
library(caret)
library(nnet)
library(e1071)
library(rpart)

f <- y~.

#s <- strata(mydata, 'y', size = c(7984,1057), method = 'srswor')
s <- strata(mydata, 'y', size = c(800,104), method = 'srswor')
training <- mydata[-s$ID_unit,]
testing <- mydata[s$ID_unit,]

# ANN
annmodel <- multinom(f, data = training, maxit = 500, trace=T)
pred <- predict(annmodel,testing)
confusionMatrix(pred, testing$y)


# SVM
svmfit <- svm(f, data = training, kernel = "linear", type="C", cost = 1, scale = FALSE)
pred <- predict(svmfit,testing)
confusionMatrix(pred, testing$y)

# Decision Tree
library(rpart.plot)
#tc <- trainControl("cv", 5)
#(train.rpart <- train(f, data = training, method = "rpart", trControl=tc))
#pred <- predict(train.rpart,testing)
my.tree <- rpart(f, data = training, method = "class")
pred <- predict(my.tree,testing,type = 'class')
confusionMatrix(pred, testing$y)

# adabag
adabag.tree <- boosting(f, data = training, mfinal = 100,control = rpart.control(maxdepth = 5, minsplit = 30))
adabag.tree <- bagging(f, data = training, mfinal = 100,control = rpart.control(maxdepth = 5, minsplit = 30))
adabag.tree <- boosting(f, data = training, mfinal = 150)
#adabag.tree <- bagging.cv(f, data = training, mfinal = 10,control = rpart.control(maxdepth = 2, minsplit = 20))
# adaboost.tree <- boosting(f, data = training, mfinal = 100)
pred <- predict(adabag.tree,testing)
confusionMatrix(pred$class, testing$y)

#mboost
mboostmd <- mboost(f, data = training)
