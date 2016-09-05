#***STP598 Computational Statistics***
#***Assignment 1***
#***Aditya Vipradas***

library(boot)
library(quantreg)
set.seed(1)
#number of rivers
nR <- 31

#number of poly fit degrees
nD <- 8

#k-fold cross validation
k <- 10

#declare zero matrix
Mat.ls.mse.test <- matrix(0,nD,nR)
Mat.ls.mae.test <- matrix(0,nD,nR)
Mat.lad.mse.test <- matrix(0,nD,nR)
Mat.lad.mae.test <- matrix(0,nD,nR)

Mat.ls.mse.train <- matrix(0,nD,nR)
Mat.ls.mae.train <- matrix(0,nD,nR)
Mat.lad.mse.train <- matrix(0,nD,nR)
Mat.lad.mae.train <- matrix(0,nD,nR)

#extract the data from csv file
setwd("E:/asu/asu_courses/semester3/stats/assignments")
data_ext <- read.csv("River_Temp.csv",header=T, sep=",")

#create null vectors
mse.ls.test <- rep(0, times=k)
mae.ls.test <- rep(0, times=k)
mse.lad.test <- rep(0, times=k)
mae.lad.test <- rep(0, times=k)

mse.ls.train <- rep(0, times=k)
mae.ls.train <- rep(0, times=k)
mse.lad.train <- rep(0, times=k)
mae.lad.train <- rep(0, times=k)

for (i in 1:nR){
  wT <- data_ext[data_ext[,1]==i,2]
  aT <- data_ext[data_ext[,1]==i,3]
  #create k folds
  folds <- sample(1:k, length(wT), replace=TRUE)
  for (j in 1:nD){
    for (m in 1:k){
      ls.fit <- glm(aT[folds!=m]~poly(wT[folds!=m],j))
      
      pred.ls.test <- predict(ls.fit , data.frame(wT[folds==m]))
      mse.ls.test[m] <- mean((aT[folds==m]-pred.ls.test)^2)
      mae.ls.test[m] <- mean(abs(aT[folds==m]-pred.ls.test))
      
      pred.ls.train <- predict(ls.fit , data.frame(wT[folds!=m]))
      mse.ls.train[m] <- mean((aT[folds!=m]-pred.ls.train)^2)
      mae.ls.train[m] <- mean(abs(aT[folds!=m]-pred.ls.train))
      
      lad.fit = rq(aT[folds!=m]~poly(wT[folds!=m],j))
      
      pred.lad.test = predict(lad.fit , data.frame(wT[folds==m]))
      mse.lad.test[m] <- mean((aT[folds==m]-pred.lad.test)^2)
      mae.lad.test[m] <- mean(abs(aT[folds==m]-pred.lad.test))
      
      pred.lad.train = predict(lad.fit , data.frame(wT[folds!=m]))
      mse.lad.train[m] <- mean((aT[folds!=m]-pred.lad.train)^2)
      mae.lad.train[m] <- mean(abs(aT[folds!=m]-pred.lad.train))
    }
    Mat.ls.mse.test[j,i] <- mean(mse.ls.test)
    Mat.ls.mae.test[j,i] <- mean(mae.ls.test)
    Mat.lad.mse.test[j,i] <- mean(mse.lad.test)
    Mat.lad.mae.test[j,i] <- mean(mae.lad.test)
    
    Mat.ls.mse.train[j,i] <- mean(mse.ls.train)
    Mat.ls.mae.train[j,i] <- mean(mae.ls.train)
    Mat.lad.mse.train[j,i] <- mean(mse.lad.train)
    Mat.lad.mae.train[j,i] <- mean(mae.lad.train)
  }
}
Pdeg = c(1:nD)
plot(Mat.ls.mae.train[,21], type = "o",col = "red", xlab = "Degree of Flexibility", ylab = "MAE", 
     main = "MAE Train and Test Error", ylim = c(2,8))

lines(Mat.ls.mae.test[,21], type = "o",col = "blue")
print(Mat.ls.mae.train)
print(Mat.ls.mae.test)


