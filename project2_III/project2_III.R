# load data and rename the data columns
mydata <- read.table('house-votes-84.data.txt', sep = ",")
col_name <- c("class","hadicapped","wpcs", "aotbr","pff","esa"
              ,"rgis","astb","atnc","mm","imigration","scc"
              ,"eduspend","srts","crime","dfe","eaasa")
names(mydata) <- col_name

# stratified sampling data: 75% training and 25% testing
library(sampling)
table(mydata$class)
s <- strata(mydata, 'class', size = c(42,67), method = 'srswor')
training <- mydata[-s$ID_unit,]
testing <- mydata[s$ID_unit,]

# I. Naive Bayes Models:
library(e1071)

fmla <- class ~ hadicapped+wpcs+aotbr+pff+esa+rgis+astb+atnc+mm+imigration+scc+eduspend+srts+crime+dfe+eaasa
naivemodel <- naiveBayes(fmla, data = training)
naivemodel
naivepred <- predict(naivemodel, testing)
table(naivepred, testing$class)

# II. Bayesian Network:
#1.
library(gRain)
library(bnlearn)

bnmodel <- tabu(training, score = "k2")
bnmodel
plot(bnmodel)
pred<-predict(bnmodel,"class", testing)
confusionMatrix(testing$class,pred)

# Section G: Obervable Markov Models and Hidden Markov Models
library(markovchain)
s <- c('a','b','c')
byRow <- TRUE
mcMatrix <- matrix(data = c(0.4,0.3,0.3,0.2,0.6,0.2,0.1,0.1,0.8),byrow = byRow, nrow = 3, dimnames = list(s,s))
mcTest <- new("markovchain", states = s, byrow = byRow, transitionMatrix = mcMatrix, name = "mymc")

a = 0;b = 0;c = 0;total=100;
for(i in 1:total){
  char = sample(c('a','b','c'),1, prob = c(0.5, 0.3, 0.2), replace = F)
  if(char == 'a'){
    a = a +1
  }else if(char == 'b'){
    b = b + 1
  }else{
    c = c + 1
  }
  # generate a sequence has 1000 states
  rmarkovchain(n = 1000, object = mcTest, t0 = char)

}
print(a/total);print(b/total);print(c/total)
library(HMM)
initialState = c(0.5, 0.3, 0.2)
hmm <- initHMM(c('s1','s2','s3'), c('a','b','c'), c(0.5, 0.3, 0.2), matrix(data = c(0.4,0.3,0.3,0.2,0.6,0.2,0.1,0.1,0.8),nrow = 3))
seq <- simHMM(hmm, 1000)

out <- seq$observation
len <- length(out)
aa=0;ab=0;ac=0;ba=0;bb=0;bc=0;ca=0;cb=0;cc=0;lena=0;lenb=0;lenc=0;
for(i in 1:(len-1)){
  t <- out[i]; t1 <- out[i+1];
  if(t == 'a'){
    lena = lena + 1
    if(t1 == 'a'){
      aa = aa + 1
    }else if(t1 == 'b'){
      ab = ab + 1
    }else{
      ac = ac + 1
    }
  }else if(t == 'b'){
    lenb = lenb + 1
    if(t1 == 'a'){
      ba = ba + 1
    }else if(t1 == 'b'){
      bb = bb + 1
    }else{
      bc = bc + 1
    }
  }else{
    lenc = lenc + 1
    if(t1 == 'c'){
      ca = ca + 1
    }else if(t1 == 'b'){
      cb = cb + 1
    }else{
      cc = cc + 1
    }
  }
}
matrix(c(aa/lena,ab/lena,ac/lena,ba/lenb,bb/lenb,bc/lenb,ca/lenc,cb/lenc,cc/lenc),nrow=3)


observations <- c('P','P','P','P','C','C','P','P','P','C','C','C','P','C','C','C','C','C','P','P','P','C','P','C','P')
states <- c("A","B", "R")
emis <- c("P","C")
transProbs <- matrix(c(0.2,0.7,0.1,0.4,0.3,0.3,0.1,0.8,0.1),3)
initProbs <- matrix(c(0.6,0.3,0.1),1)
emissionProbs <- matrix(c(0.4,0.6,0.5,0.5,0.1,0.9),3)
hmm = initHMM(States=states, Symbols=emis, startProbs=initProbs, transProbs=transProbs,emissionProbs=emissionProbs)
logForwardProbabilities = forward(hmm,observations)
print(exp(logForwardProbabilities))


logBackwardProbabilities = backward(hmm,observations)
print(exp(logBackwardProbabilities))


hmm <- initHMM(States=states, Symbols=emis, startProbs=initProbs, transProbs=transProbs, emissionProbs=emissionProbs)
simHMM <- simHMM(hmm, 2000)

bw <- baumWelch(hmm,simHMM$observation,100)
