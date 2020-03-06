remove(list=ls());
#install.packages("mlbench");
library(mlbench)
# install.packages("caTools")
library(caTools)

utils::data(PimaIndiansDiabetes2)
Diab2<-PimaIndiansDiabetes2;


# MICE ##################################################################

sapply(Diab2, function(x) sum(is.na(x))) #sapply returns a list of the same length as X, each element
                                        #of which is the result of applying FUN to the corresponding 
                                        #element of X.sapply is a user-friendly version and

#install.packages("mice")
library(mice)

init = mice(Diab2, maxit=0) 
init
meth=init$method
predM=init$predictorMatrix

#meth[c("glucose","pressure","triceps", "insulin","mass")] <-"norm"

# run the multiple imputation
set.seed(101)
imputed = mice(Diab2, method=meth, predictorMatrix=predM, m=5)   # Q: is better to use pmm or norm method?
imput<-complete(imputed)


############################################################

# Split the dataset in train (80%) and test (20%)

sample<-sample.split(imput,SplitRatio=0.8)
train.set<-subset(imput, sample==TRUE)
test.set <- subset(imput, sample==FALSE)

train.x <- train.set[,1:8]
test.x <- test.set[, 1:8]
train.y <- train.set[,9]
test.y <- test.set[,9]

classes <- factor(train.set$diabetes)

predicted.classes <- knn(train.x, test.x, classes, k=3)


#############################################################

# Error

ks <- c(1,3,5,7,9,11,15,17,23,25,35,45,55,83,101,151)
nks <- length(ks)

misclass.train <- numeric(length=nks)   # 0 vector of doubles
misclass.test <- numeric(length=nks)
names(misclass.test) <- ks

for (i in seq(along=ks)){
   mod.train <- knn(train.x, train.x,classes, k=ks[i])
   mod.test <- knn(train.x, test.x, classes, k=ks[i])
   misclass.train[i] <- sum(mod.train!=train.y)/nrow(train.set) 
   misclass.test[i] <- sum(mod.test!=test.y)/nrow(test.set)
  
}

x11()
plot(misclass.train,xlab="Number of NN",ylab="Test error",type="n",xaxt="n",ylim=c(0,0.4)) #type="n" non disegna nulla, xaxt="n" non mette valori sull'asse x
axis(1, 1:length(ks), as.character(ks)) 
lines(misclass.test,type="b",col='blue',pch=20)   
lines(misclass.train,type="b",col='red',pch=20)
legend("bottomright",lty=1,col=c("red","blue"),legend = c("train ", "test "))
title('Test performace vs Train performace')


# ###########################################################
# 
# # Decision Boundaries
# 
# col1<-c('blue', 'magenta')
# 
# library(grid)  #ci serve per plottare una griglia
# plot.knn <- function(kneighbours){
#   grid=numeric(8)
#   grid.x2 <- seq(min(imput[,2]), max(imput[,2]), 1)
#   grid.x8 <- seq(min(imput[,8]), max(imput[,8]), 1)
#   grid <- expand.grid(0, grid.x2,0,0,0,0,0,grid.x8)
#   colnames(grid) <- colnames(train.x)
#   predicted.classes <- knn(train.x, grid, classes, k=kneighbours)
#   x11()
#   plot(imput$glucose, imput$age, pch=20, col=col1[as.numeric(imput$diabetes)], xlab='glucose', ylab='age')
#   points(grid$glucose, grid$age, pch='.', col=col1[as.numeric(predicted.classes)])  # draw grid
#   legend("topleft", legend=levels(imput$diabetes),fill =col1)
#   title(c("Classification with k=", kneighbours))
#   
#   predicted.matrix <- matrix(as.numeric(predicted.classes), length(grid.x2), length(grid.x8))
#   contour(grid.x2, grid.x8, predicted.matrix, levels=c(1.5), drawlabels=FALSE,add=TRUE) 
#   }
# 
# plot.knn(1)
# plot.knn(3)
# plot.knn(5)
# plot.knn(10)
# 

# PCA

#install.packages("ggfortify")
library(ggfortify)

test.pred <- cbind(test.x, predicted.classes)

autoplot(prcomp(test.x), data = test.pred, colour = 'predicted.classes')

