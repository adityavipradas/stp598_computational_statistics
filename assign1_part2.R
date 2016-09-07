#***STP598 Computational Statistics***
#***Assignment 1_Part 2***
#***Aditya Vipradas***

library(ISLR)
set.seed(2)

#validation set approach
k <- 2

#repeat for number of times
n <- 3

#create error vector
err <- rep(0,n)

#loop over number of turns
for (i in 1:n){
  
  #declare number of folds
  folds <- sample(1:2, length(Default$balance), replace=TRUE)
  
  #training set
  train <- Default[folds==1,]
  
  #testing set
  test <- Default[folds==2,]
  
  #learn from training set
  logit.fit = glm(default~balance+income,family="binomial",data=train)
  
  #predict for test data
  logit.pred = predict(logit.fit, data=test, type='response')
  print(logit.pred)
  #validation error
  testCorrect = sum(logit.pred == test$default)
  
  #append in the err vector
  err[i] = testCorrect/length(test$default)
}
print(err)
