optdigits.tra <- read.table("optdigits.tra",sep=",")
optdigits.tes <- read.table("optdigits.tes", sep = ",")
library(neuralnet)


#maxs <- apply(traindata, 2, max)
#mins <- apply(traindata, 2, min)
#scaled <- as.data.frame(scale(traindata, center = mins, scale = maxs - mins))
# nn <- neuralnet(f, data = traindata, hidden = c(3,2), err.fct = "ce", linear.output = FALSE)

mytest <- traindata[1:10, -c(1,40,65)]

compute(nn, mytest)$net.result

nn <- neuralnet(V65 ~ V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+
                  V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+
                  V31++V32+V33+V34+V35+V36+V37+V38+V39+V41+V42+V43+V44+V45+V46+
                  V47+V48+V49+V50+V51+V52+V53+V54+V55+V56+V57+V58+V59+V60+V61+
                  V62+V63+V64,data=traindata, hidden = c(5,3), linear.output = F )


nnet_traindata <- optdigits.tra
nnet_traindata <- cbind(nnet_traindata,traindata$V65 == '0')
nnet_traindata <- cbind(nnet_traindata,traindata$V65 == '1')
nnet_traindata <- cbind(nnet_traindata,traindata$V65 == '2')
nnet_traindata <- cbind(nnet_traindata,traindata$V65 == '3')
nnet_traindata <- cbind(nnet_traindata,traindata$V65 == '4')
nnet_traindata <- cbind(nnet_traindata,traindata$V65 == '5')
nnet_traindata <- cbind(nnet_traindata,traindata$V65 == '6')
nnet_traindata <- cbind(nnet_traindata,traindata$V65 == '7')
nnet_traindata <- cbind(nnet_traindata,traindata$V65 == '8')
nnet_traindata <- cbind(nnet_traindata,traindata$V65 == '9')


names(nnet_traindata)[66] <- 'zero'
names(nnet_traindata)[67] <- 'one'
names(nnet_traindata)[68] <- 'two'
names(nnet_traindata)[69] <- 'three'
names(nnet_traindata)[70] <- 'four'
names(nnet_traindata)[71] <- 'five'
names(nnet_traindata)[72] <- 'six'
names(nnet_traindata)[73] <- 'seven'
names(nnet_traindata)[74] <- 'eight'
names(nnet_traindata)[75] <- 'nine'



nn <- neuralnet(zero+one+two+three+four+five+six+seven+eight+nine ~
                  V2+V3+V4+V5+V6+V7+V8+V10+V11+V12+V13+V14+V15+V16+
                  V18+V19+V20+V21+V22+V23+V24+V26+V27+V28+V29+V30+
                  V31+V34+V35+V36+V37+V38+V39+V42+V43+V44+V45+V46+
                  V47+V50+V51+V52+V53+V54+V55+V56+V58+V59+V60+V61+
                  V62+V63+V64,data=nnet_traindata, hidden = c(5,3) )

# nnet

# optdigits.tra$V65 <- as.factor(optdigits.tra$V65)
# optdigits.tes$V65 <- as.factor(optdigits.tes$V65)
traindata <- optdigits.tra
n <- names(traindata)
f <- as.formula(paste("V65 ~ ", paste(n[!n %in% "V65"], collapse = " + ")))

mymodel <- multinom(f, data = optdigits.tra, maxit = 500, trace=T)

library(caret)
topmodels <- varImp(mymodel)
topmodels$Variables <- row.names(topmodels)
topmodels <- topmodels[order(-topmodels$Overall),]

preds1 <- predict(mymodel, type = "probs", newdata = optdigits.tes)
preds2 <- predict(mymodel, type = "class", newdata = optdigits.tes)

postResample(optdigits.tes$V65, preds2)
library(Metrics)
table(optdigits.tes$V65, preds2)

totalError <- c()
cv <- 10
cvDivider <- floor(nrow(optdigits.tra)/(cv+1))

ptm <- proc.time()
for (cv in seq(1:cv)){
  # assign chunk to data test
  dataTestIndex <- c((cv * cvDivider):(cv*cvDivider + cvDivider))
  dataTest <- optdigits.tra[dataTestIndex,]
  # everything else to train
  dataTrain <- optdigits.tra[-dataTestIndex,]
  myModel <- multinom(f, data = optdigits.tra, maxit = 500, trace = T)
  pred <- predict(myModel, dataTest)

  # classification error
  err <- ce(dataTest$V65, pred)
  totalError <- c(totalError, err)
}
proc.time() - ptm

# SVM
optdigits.tra <- read.table("optdigits.tra",sep=",")
optdigits.tes <- read.table("optdigits.tes", sep = ",")
traindata <- optdigits.tra
traindata$V65[traindata$V65 != 8] <- -1
traindata$V65[traindata$V65 == 8] <- 1
traindata$V65 <- as.factor(traindata$V65)
# scale the data and reset col1 and col40 to 0
traindata[,1:64] <- apply(traindata[,1:64], 2, scale)
traindata[,1] <- 0
traindata[,40] <- 0

testdata <- optdigits.tes
testdata$V65[testdata$V65 != 8] <- -1
testdata$V65[testdata$V65 == 8] <- 1
testdata$V65 <- as.factor(testdata$V65)
# scale the data and reset col1,col33 and col40 to 0
testdata[,1:64] <- apply(testdata[,1:64],2,scale)
testdata[,1] <- 0
testdata[,33] <- 0
testdata[,40] <- 0


library(e1071)
svmfit <- svm(f, data = traindata, kernel = "linear", type="C", cost = 1, scale = FALSE)
svmfit <- svm(f, data = traindata, kernel = "polynomial", type="C",cost = 1, scale = FALSE)
svmfit <- svm(f, data = traindata, kernel = "radial", type="C", cost = 1, scale = FALSE)
svmfit <- svm(f, data = traindata, kernel = "sigmoid", type="C", cost = 1, scale = FALSE)
summary(svmfit)
pred <- predict(svmfit, testdata)
(result <- postResample(testdata$V65, pred))

tune.out1 <- tune(svm, f, data = traindata, kernel = "linear", type = "C", ranges = list(cost = c(0.1,1,10)), scale = FALSE)
bestmodel1 <- tune.out1$best.model
summary(bestmodel1)

svmfit1 <- svm(f, data = traindata, kernel = "linear", type="C", cost = 0.1, scale = FALSE)
svmfit2 <- svm(f, data = traindata, kernel = "linear", type="C", cost = 1, scale = FALSE)
svmfit3 <- svm(f, data = traindata, kernel = "linear", type="C", cost = 10, scale = FALSE)
pred1 <- predict(svmfit1, testdata)
# table(predict = pred1, truth = testdata$V65)
(result1 <- postResample(testdata$V65, pred1))
pred2 <- predict(svmfit2, testdata)
(result2 <- postResample(testdata$V65, pred2))
pred3 <- predict(svmfit3, testdata)
(result3 <- postResample(testdata$V65, pred3))

svmfit1 <- svm(f, data = traindata, kernel = "polynomial", type="C", degree = 3, scale = FALSE)
svmfit2 <- svm(f, data = traindata, kernel = "polynomial", type="C", degree = 6, scale = FALSE)
svmfit3 <- svm(f, data = traindata, kernel = "polynomial", type="C", degree = 10, scale = FALSE)
pred1 <- predict(svmfit1, testdata)
# table(predict = pred1, truth = testdata$V65)
(result1 <- postResample(testdata$V65, pred1))
pred2 <- predict(svmfit2, testdata)
(result2 <- postResample(testdata$V65, pred2))
pred3 <- predict(svmfit3, testdata)
(result3 <- postResample(testdata$V65, pred3))


svmfit1 <- svm(f, data = traindata, kernel = "radial", type="C", gamma = 0.01, scale = FALSE)
svmfit2 <- svm(f, data = traindata, kernel = "radial", type="C", gamma = 0.1, scale = FALSE)
svmfit3 <- svm(f, data = traindata, kernel = "radial", type="C", gamma = 0.5, scale = FALSE)
pred1 <- predict(svmfit1, testdata)
# table(predict = pred1, truth = testdata$V65)
(result1 <- postResample(testdata$V65, pred1))
pred2 <- predict(svmfit2, testdata)
(result2 <- postResample(testdata$V65, pred2))
pred3 <- predict(svmfit3, testdata)
(result3 <- postResample(testdata$V65, pred3))


svmfit1 <- svm(f, data = traindata, kernel = "sigmoid", type="C", coef0 = 0, scale = FALSE)
svmfit2 <- svm(f, data = traindata, kernel = "sigmoid", type="C", coef0 = 1, scale = FALSE)
svmfit3 <- svm(f, data = traindata, kernel = "sigmoid", type="C", coef0 = 2, scale = FALSE)
pred1 <- predict(svmfit1, testdata)
# table(predict = pred1, truth = testdata$V65)
(result1 <- postResample(testdata$V65, pred1))
pred2 <- predict(svmfit2, testdata)
(result2 <- postResample(testdata$V65, pred2))
pred3 <- predict(svmfit3, testdata)
(result3 <- postResample(testdata$V65, pred3))

#pca
# remove all 0 value columns and V65
traindata <- optdigits.tra
mytest <- traindata[, -c(1,40)]
pca <- prcomp(mytest[,-63], retx=TRUE, center=TRUE, scale=TRUE)
summary(pca)
newdata <- pca$x[,1:2]
newdata <- data.frame(newdata)
# set the target variable
newdata[,3] <- traindata[,65]
newdata$V3[newdata$V3 != 8] <- -1
newdata$V3[newdata$V3 == 8] <- 1
newdata$V3 <- as.factor(newdata$V3)
library(e1071)
svmfit <- svm(V3 ~ PC1+PC2, data = newdata, kernel = "polynomial", type="C", degree = 6, scale = FALSE)
plot(svmfit, newdata)
