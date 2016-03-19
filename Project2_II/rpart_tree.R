library(rpart)
library(party)
library(caret)
library(rattle)

library(rpart.plot)
library(RColorBrewer)

tc <- trainControl("cv", 4)
frmla <- heartDisease~.
my.tree <- rpart(frmla, data = mydata, method = "class")
fancyRpartPlot(my.tree)

(train.rpart <- train(frmla, data = mydata, method = "rpart", trControl=tc))

rpartTune <- train(frmla, data=mydata,method="rpart",tuneLength = 12, trControl=tc)
plot(rpartTune)