#***STP598 Computational Statistics***
#***Assignment 1_Part 2***
#***Aditya Vipradas***

library(ISLR)
#set.seed(2)

#validation set approach
#k <- 2
#for 10-fold cross-validation
k <- 10 

#repeat for number of times
n <- 3

#create error vector
err <- rep(0,n)
errStud <- rep(0,n)
foldErr <- rep(0,k)
foldErrStudent <- rep(0,k)

#loop over number of turns
for (i in 1:n){
  
  #declare number of folds
  folds <- sample(1:k, length(Default$balance), replace=TRUE)
  
  #loop over folds
  for (j in 1:k){
  
    #training set
    train <- Default[folds!=j,]
  
    #testing set
    test <- Default[folds==j,]
  
    #learn from training set
    logit.fit <- glm(default~balance+income,family="binomial",data=train)
    logit.fit.student <- glm(default~student+balance+income,family="binomial",data=train)
  
    #predict for test data
    logit.pred <- predict(logit.fit, newdata=subset(test,select=c(3,4)), type='response')
    logit.pred.student <- predict(logit.fit.student, newdata=subset(test,select=c(2,3,4)), type='response')
  
    #validation error
    #threshold is 0.5
    logit.pred <- ifelse(logit.pred > 0.5,1,0)
    logit.pred.student <- ifelse(logit.pred.student > 0.5,1,0)
    temp.test <- ifelse(test$default == "Yes",1,0)
  
    #append in the err vector
    foldErr[j] <- mean(logit.pred != temp.test)
    foldErrStudent[j] <- mean(logit.pred.student != temp.test)
  }
  err[i] <- mean(foldErr)
  errStud[i] <- mean(foldErrStudent)
}
print(err)
print(errStud)