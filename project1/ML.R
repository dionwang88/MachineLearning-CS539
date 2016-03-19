# get dataset file
data <- read.csv('communities.data', header = FALSE)
# get dataset variable names
name_data <- read.delim('names', header = FALSE, sep = ' ')
# set dataset column names
names(data) <- name_data[,2]
# drop ‘communityname’ column
data['communityname'] <- NULL

# inpulate missing value with column mean
for(i in 1:ncol(data)){
  # transfer every column type to numeric
  data[,i] <- as.numeric(data[,i])
  data[data[,i] == '?',i] <- mean(data[,i], na.rm = TRUE)
}

# set random seed is 100
set.seed(100)
# split data to training and validation set, training set is 60%
train_ind <- sample(seq_len(nrow(data)), size = (0.6 * nrow(data)))
TS <- data[train_ind, ]
VS <- data[-train_ind, ]

# 1. Baseline
ptm <- proc.time()
fit_model <- lm(data = TS, ViolentCrimesPerPop ~ .)
proc.time() - ptm

summary(fit_model)

# validate the training model
pred <- predict.lm(fit_model, VS[,1:126])

SSE <- sum((VS$ViolentCrimesPerPop - pred)^2)
RMSE <- sqrt(mean((VS$ViolentCrimesPerPop - pred)^2))
RSE <- sum((VS$ViolentCrimesPerPop - pred)^2)/sum((mean(VS$ViolentCrimesPerPop)-VS$ViolentCrimesPerPop)^2)
SST <- sum((mean(VS$ViolentCrimesPerPop)-VS$ViolentCrimesPerPop)^2)
R_square <- 1-SSE/SST

# 2. Sequential Subset Selection
ptm <- proc.time()
model_step <- step(fit_model,direction = 'both')
proc.time() - ptm

summary(model_step)

# validate the training model
prediction <- predict(model_step, newdata = VS[,1:126])

SSE <- sum((VS$ViolentCrimesPerPop - prediction)^2)
RMSE <- sqrt(mean((VS$ViolentCrimesPerPop - prediction)^2))
RSE <- sum((VS$ViolentCrimesPerPop - prediction)^2)/sum((mean(VS$ViolentCrimesPerPop)-VS$ViolentCrimesPerPop)^2)
SST <- sum((mean(VS$ViolentCrimesPerPop)-VS$ViolentCrimesPerPop)^2)
R_square <- 1-SSE/SST

# 3. Ranking Attributes
# load the library
library(mlbench)
library(caret)

# get the top 50 important variables
varimp <- varImp(fit_model)
varimp[,'names'] <- rownames(varimp)
imp <- varimp[order(varimp$Overall,decreasing = TRUE),]
top50 <- head(imp,50)$names

var <- top50[1]
for (i in 2:length(top50)){
  var <- paste(var,top50[i],sep = "+")
}
# print the top 50 variable names
var 

ptm <- proc.time()
rank_model <- lm(data = TS,ViolentCrimesPerPop ~ PctPopUnderPov+NumStreet+PctIlleg+pctWRetire+RentLowQ+PctKids2Par+NumImmig+PctNotSpeakEnglWell+PctHousNoPhone+PersPerRentOccHous+MalePctNevMarr+PolicBudgPerPop+PersPerOccupHous+whitePerCap+MedOwnCostPctInc+PolicReqPerOffic+PctVacMore6Mos+PctLess9thGrade+NumUnderPov+PctPolicAsian+county+PctSpeakEnglOnly+pctWFarmSelf+LemasPctOfficDrugUn+PctVacantBoarded+LemasTotalReq+PctPolicBlack+PolicOperBudg+PctHousOccup+racepctblack+PctOccupMgmtProf+MedOwnCostPctIncNoMtg+PctPersDenseHous+PolicCars+MedRentPctHousInc+HousVacant+PctEmplManu+PersPerOwnOccHous+MedRent+medIncome+MedNumBR+PctUsePubTrans+PctHousLess3BR+PctSameState85+PctPersOwnOccup+agePct12t29+PctPolicHisp+pctWSocSec+MedYrHousBuilt+perCapInc )
proc.time() - ptm

summary(rank_model)

prediction <- predict(rank_model, newdata = VS[,1:126])

SSE <- sum((VS$ViolentCrimesPerPop - prediction)^2)
RMSE <- sqrt(mean((VS$ViolentCrimesPerPop - prediction)^2))
RSE <- sum((VS$ViolentCrimesPerPop - prediction)^2)/sum((mean(VS$ViolentCrimesPerPop)-VS$ViolentCrimesPerPop)^2)
SST <- sum((mean(VS$ViolentCrimesPerPop)-VS$ViolentCrimesPerPop)^2)
R_square <- 1-SSE/SST

# 4. PCA
ptm <- proc.time()
pca <- prcomp(TS[,1:126], retx=TRUE, center=TRUE, scale=TRUE)

summary(pca)

# get the transfered pca data
newdata <- pca$x[,1:28]
newdata <- data.frame(newdata)
# set the target variable
newdata[,29] <- TS[,127]
fitmodel <- lm(data = newdata, V29 ~ .)
proc.time() - ptm

# transfer validation data to principle components
pred.vs <- predict(pca, VS[,1:126])
pred.vs <- data.frame(pred.vs)
pred.vs <- pred.vs[,1:28]

# validate the data
prediction <- predict(fitmodel, newdata = pred.vs)
SSE <- sum((VS$ViolentCrimesPerPop - prediction)^2)
RMSE <- sqrt(mean((VS$ViolentCrimesPerPop - prediction)^2))
RSE <- sum((VS$ViolentCrimesPerPop - prediction)^2)/sum((mean(VS$ViolentCrimesPerPop)-VS$ViolentCrimesPerPop)^2)
SST <- sum((mean(VS$ViolentCrimesPerPop)-VS$ViolentCrimesPerPop)^2)
R_square <- 1-SSE/SST

# 5.Factor Analysis
library(psych)
# set the factor number is 30
ptm <- proc.time()
fa_fit <- factor.pa(TS[,1:126], nfactors=30)
fa_data <- predict(object = fa_fit, data = TS[,1:126])
# set 'ViolentCrimesPerPop' column
fa_data <- data.frame(fa_data)
fa_data[,31] <- TS[,127]
colnames(fa_data)[31] <- "ViolentCrimesPerPop"
fit_fa_model <- lm(data = fa_data, ViolentCrimesPerPop ~ .)
proc.time() - ptm

# transfer Validation data using FA model
fa_VS_data <- predict(object = fa_fit, data = VS[,1:126])
fa_VS_data <- data.frame(fa_VS_data)
fa_VS_data[,31] <- VS[,127]
colnames(fa_VS_data)[31] <- "ViolentCrimesPerPop"
prediction <- predict(fit_fa_model, newdata = fa_VS_data[,1:30])

SSE <- sum((VS$ViolentCrimesPerPop - prediction)^2)
RMSE <- sqrt(mean((VS$ViolentCrimesPerPop - prediction)^2))
RSE <- sum((VS$ViolentCrimesPerPop - prediction)^2)/sum((mean(VS$ViolentCrimesPerPop)-VS$ViolentCrimesPerPop)^2)
SST <- sum((mean(VS$ViolentCrimesPerPop)-VS$ViolentCrimesPerPop)^2)
R_square <- 1-SSE/SST
