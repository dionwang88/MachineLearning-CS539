mydata <- read.table('heart.dat')
mydata_names <- c('age', 'sex', 'chestPain', 'bloodPressure', 'cholestoral', 'bloodSugar', 'elecResults', 'maxHeartRate', 'inducedAngina','oldpeak', 'slopeST','numVessels','thal', 'heartDisease')
names(mydata) <- mydata_names
mydata$sex <- as.factor(mydata$sex)
mydata$bloodSugar <- as.factor(mydata$bloodSugar)
mydata$inducedAngina <- as.factor(mydata$inducedAngina)
mydata$chestPain <- as.factor(mydata$chestPain)
mydata$elecResults <- as.factor(mydata$elecResults)
mydata$thal <- as.factor(mydata$thal)
mydata$heartDisease <- as.factor(mydata$heartDisease)

