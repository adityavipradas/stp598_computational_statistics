#***STP598 Computational Statistics***
#***Assignment 1***
#***Aditya Vipradas***

library(quantreg)
set.seed(1)
#number of rivers
nR <- 31

#number of poly fit degrees
nD <- 8

#k-fold cross validation
k <- 10

#declare zero matrix
Mat.ls.smape.test <- matrix(0,nD,nR)
Mat.ls.mae.test <- matrix(0,nD,nR)
Mat.lad.smape.test <- matrix(0,nD,nR)
Mat.lad.mae.test <- matrix(0,nD,nR)

Mat.ls.smape.train <- matrix(0,nD,nR)
Mat.ls.mae.train <- matrix(0,nD,nR)
Mat.lad.smape.train <- matrix(0,nD,nR)
Mat.lad.mae.train <- matrix(0,nD,nR)

#extract the data from csv file
setwd("E:/asu/asu_courses/semester3/stats/assignments/1/part1")
data_ext <- read.csv("River_Temp.csv",header=T, sep=",")

#create null vectors
smape.ls.test <- rep(0, times=k)
mae.ls.test <- rep(0, times=k)
smape.lad.test <- rep(0, times=k)
mae.lad.test <- rep(0, times=k)

smape.ls.train <- rep(0, times=k)
mae.ls.train <- rep(0, times=k)
smape.lad.train <- rep(0, times=k)
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
      smape.ls.test[m] <- median(200*abs(aT[folds==m]-pred.ls.test)/(aT[folds==m]+pred.ls.test))
      mae.ls.test[m] <- mean(abs(aT[folds==m]-pred.ls.test))
      
      pred.ls.train <- predict(ls.fit , data.frame(wT[folds!=m]))
      smape.ls.train[m] <- median(200*abs(aT[folds!=m]-pred.ls.train)/(aT[folds!=m]+pred.ls.train))
      mae.ls.train[m] <- mean(abs(aT[folds!=m]-pred.ls.train))
      
      lad.fit = rq(aT[folds!=m]~poly(wT[folds!=m],j))
      
      pred.lad.test = predict(lad.fit , data.frame(wT[folds==m]))
      smape.lad.test[m] <- median(200*abs(aT[folds==m]-pred.lad.test)/(aT[folds==m]+pred.lad.test))
      mae.lad.test[m] <- mean(abs(aT[folds==m]-pred.lad.test))
      
      pred.lad.train = predict(lad.fit , data.frame(wT[folds!=m]))
      smape.lad.train[m] <- median(200*abs(aT[folds!=m]-pred.lad.train)/(aT[folds!=m]+pred.lad.train))
      mae.lad.train[m] <- mean(abs(aT[folds!=m]-pred.lad.train))
    }
    
    plotX = wT[folds!=m]
    plot(plotX,aT[folds!=m],col='green',main=paste("River",i,"(Degree of least squares fit:",j,")"), xlab='water temperature', ylab='air temperature')
    lines(plotX[order(plotX)], pred.ls.train[order(plotX)], col='black')
    dev.copy(png,paste("River",i,"LS_Fit",j,".png"))
    dev.off()
    
    plot(plotX,aT[folds!=m],col='green',main=paste("River",i,"(Degree of quantreg fit:",j,")"), xlab='water temperature', ylab='air temperature')
    lines(plotX[order(plotX)], pred.lad.train[order(plotX)], col='black')
    dev.copy(png,paste("River",i,"QR_Fit",j,".png"))
    dev.off()
    
    Mat.ls.smape.test[j,i] <- mean(smape.ls.test)
    Mat.ls.mae.test[j,i] <- mean(mae.ls.test)
    Mat.lad.smape.test[j,i] <- mean(smape.lad.test)
    Mat.lad.mae.test[j,i] <- mean(mae.lad.test)
    
    Mat.ls.smape.train[j,i] <- mean(smape.ls.train)
    Mat.ls.mae.train[j,i] <- mean(mae.ls.train)
    Mat.lad.smape.train[j,i] <- mean(smape.lad.train)
    Mat.lad.mae.train[j,i] <- mean(mae.lad.train)
  }
  plot(Mat.ls.mae.train[,i], type = "o",col = "red", xlab = "Degree of Flexibility", ylab = "MAE", main = paste("LS Train MAE for River",i))
  dev.copy(png,paste("River",i,"(LSTrainMAE).png"))
  dev.off()
  
  plot(Mat.ls.mae.test[,i], type = "o",col = "blue", xlab = "Degree of Flexibility", ylab = "MAE", main = paste("LS Test MAE for River",i))
  dev.copy(png,paste("River",i,"(LSTestMAE).png"))
  dev.off()
  
  plot(Mat.ls.smape.train[,i], type = "o",col = "red", xlab = "Degree of Flexibility", ylab = "SMAPE", main = paste("LS Train SMAPE for River",i))
  dev.copy(png,paste("River",i,"(LSTrainSMAPE).png"))
  dev.off()
  
  plot(Mat.ls.smape.test[,i], type = "o",col = "blue", xlab = "Degree of Flexibility", ylab = "SMAPE", main = paste("LS Test SMAPE for River",i))
  dev.copy(png,paste("River",i,"(LSTestSMAPE).png"))
  dev.off()
  
  plot(Mat.lad.mae.train[,i], type = "o",col = "red", xlab = "Degree of Flexibility", ylab = "MAE", main = paste("LAD Train MAE for River",i))
  dev.copy(png,paste("River",i,"(LADTrainMAE).png"))
  dev.off()
  
  plot(Mat.lad.mae.test[,i], type = "o",col = "blue", xlab = "Degree of Flexibility", ylab = "MAE", main = paste("LAD Test MAE for River",i))
  dev.copy(png,paste("River",i,"(LADTestMAE).png"))
  dev.off()
  
  plot(Mat.lad.smape.train[,i], type = "o",col = "red", xlab = "Degree of Flexibility", ylab = "SMAPE", main = paste("LAD Train SMAPE for River",i))
  dev.copy(png,paste("River",i,"(LADTrainSMAPE).png"))
  dev.off()
  
  plot(Mat.lad.smape.test[,i], type = "o",col = "blue", xlab = "Degree of Flexibility", ylab = "SMAPE", main = paste("LAD Test SMAPE for River",i))
  dev.copy(png,paste("River",i,"(LADTestSMAPE).png"))
  dev.off()
}
# print(Mat.ls.smape.train)
# print(Mat.ls.smape.test)


