remove(list=ls());
#install.packages("mlbench");
library(mlbench)
# install.packages("caTools")
library(caTools)

utils::data(PimaIndiansDiabetes2)
Diab2<-PimaIndiansDiabetes2;


###################################################################

# MICE

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
set.seed(123)
imputed = mice(Diab2, method=meth, predictorMatrix=predM, m=5)   # Q: is better to use pmm or norm method?
imput<-complete(imputed)


# Split the dataset in train (80%) and test (20%)

sample<-sample.split(imput,SplitRatio=0.8)
train.set<-subset(imput, sample==TRUE)
test.set <- subset(imput, sample==FALSE)


#############################################################
###################       KNN       #########################bufera
#############################################################

train.x <- train.set[,1:8]
test.x <- test.set[, 1:8]
train.y <- train.set[,9]
test.y <- test.set[,9]

classes <- factor(train.set$diabetes)

predicted.classes <- knn(train.x, test.x, classes, k=3)

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

#x11()
plot(misclass.train,xlab="Number of NN",ylab="Test error",type="n",xaxt="n",ylim=c(0,0.4)) #type="n" non disegna nulla, xaxt="n" non mette valori sull'asse x
axis(1, 1:length(ks), as.character(ks)) 
lines(misclass.test,type="b",col='blue',pch=20)   
lines(misclass.train,type="b",col='red',pch=20)
legend("bottomright",lty=1,col=c("red","blue"),legend = c("train ", "test "))
title('Test performace vs Train performace')


###########################################################

# Decision Boundaries

col1<-c('blue', 'magenta')

library(grid)  
plot.knn <- function(kneighbours){
  grid.x2 <- seq(min(imput[,2]), max(imput[,2]), 1)
  grid.x6 <- seq(min(imput[,6]), max(imput[,6]), 1)
  grid <- expand.grid(mean(imput[,1]),grid.x2,mean(imput[,3]),mean(imput[,4]),mean(imput[,5]),grid.x6,mean(imput[,7]),mean(imput[,8]))
  colnames(grid) <- colnames(train.x)
  predicted.classes <- knn(train.x, grid, classes, k=kneighbours)
  x11()
  plot(imput$glucose, imput$mass, pch=20, col=col1[as.numeric(imput$diabetes)], xlab='glucose', ylab='mass')
  points(grid$glucose, grid$mass, pch='.', col=col1[as.numeric(predicted.classes)])  # draw grid
  legend("topleft", legend=levels(imput$diabetes),fill =col1)
  title(c("Classification with k=", kneighbours))

  predicted.matrix <- matrix(as.numeric(predicted.classes), length(grid.x2), length(grid.x6))
  contour(grid.x2, grid.x6, predicted.matrix, levels=c(1.5), drawlabels=FALSE,add=TRUE)
  }

plot.knn(1)
plot.knn(3)
plot.knn(5)
plot.knn(8)


##########################################################################################

#                                  Linear regression

##########################################################################################

imput.x<-imput[,-9]

ml<-lm(as.numeric(imput$diabetes)~pregnant+glucose+pressure+triceps+insulin+mass+pedigree+age, data=imput[,-9])
summary(ml)
ml<-lm(as.numeric(imput$diabetes)~pregnant+glucose+ pressure+  insulin+mass+pedigree+age, data=imput[,-9])
summary(ml)
ml<-lm(as.numeric(imput$diabetes)~pregnant+glucose+ pressure+ mass+pedigree+age, data=imput[,-9])
summary(ml)
ml<-lm(as.numeric(imput$diabetes)~pregnant+glucose+ mass+pedigree+age, data=imput[,-9])
summary(ml)
ml<-lm(as.numeric(imput$diabetes)~pregnant+glucose+ mass+pedigree, data=imput[,-9])
summary(ml)

# classifier

f_col <- function(x){ if (x<1.5) 'blue' else 'magenta'}

lsm <- function(regmod, data){
  val <- round(predict(regmod, data))
  pr.cl<-sapply(val,f_col)
  return 
}

# Boundaries

grid.x2 <- seq(min(imput[,2]), max(imput[,2]), 1)
grid.x6 <- seq(min(imput[,6]), max(imput[,6]), 1)
grid <- expand.grid(mean(imput[,1]),grid.x2,grid.x6,mean(imput[,7]))
colnames(grid) <- colnames(train.x[c(1,2,6,7)])

predicted.classes <- lsm(ml,grid)
pr.cl.val <- as.numeric(data.frame(predicted.classes)$predicted.classes)

x11()
plot(imput$glucose, imput$mass, pch=20, col=col1[as.numeric(imput$diabetes)], xlab='glucose', ylab='mass')
points(grid$glucose, grid$mass, pch='.', col=predicted.classes)  # draw grid
legend("topleft", legend=levels(imput$diabetes),fill =col1)
title(c("Classification with linear regression"))

predicted.matrix <- matrix(pr.cl.val, length(grid.x2), length(grid.x6))
contour(grid.x2, grid.x6, predicted.matrix, levels=c(1.5), drawlabels=FALSE,add=TRUE) # but it would be more reasonable draw a linear function


####################################################

# PCA


#install.packages("ggfortify")
library(ggfortify)
library(devtools)
install_github("vqv/ggbiplot")

pca<- prcomp(train.x)
pca.values <- pca$x

test.pred <- cbind(test.x, predicted.classes)

autoplot(prcomp(test.x), data = test.pred, colour = 'predicted.classes')

# Decision boudaries in PCA

plot.knn <- function(kneighbours){
  grid.PC1 <- seq(min(pca.values[,1]), max(pca.values[,1]), 5)
  grid.PC2 <- seq(min(pca.values[,2]), max(pca.values[,2]), 1)
  grid <- expand.grid(grid.PC1,grid.PC2)
  colnames(grid) <- c('PC1','PC2')
  predicted.classes <- knn(pca.values[,c(1,2)], grid, classes, k=kneighbours)
  plot(pca.values[,1], pca.values[,2], pch=20, col=col1[as.numeric(train.set$diabetes)], xlab='PC1', ylab='PC2')
  points(grid$PC1, grid$PC2, pch='.', col=col1[as.numeric(predicted.classes)])  # draw grid
  legend("topleft", legend=levels(imput$diabetes),fill =col1)
  title(c("PCA Classification with k=", kneighbours))
  
  predicted.matrix <- matrix(as.numeric(predicted.classes), length(grid.PC1), length(grid.PC2))
  contour(grid.PC1, grid.PC2, predicted.matrix, levels=c(1.5), drawlabels=FALSE,add=TRUE)
}

plot.knn(1)

###########

# CART

install.packages('tidyverse')
install.packages('caret')
install.packages('rpart')

library(tidyverse)
library(caret)
library(rpart)

# Build the model
model1 <- rpart(diabetes ~., data = train.set, method = "class")

# Plot the trees
par(xpd = NA) # Avoid clipping the text in some device
plot(model1)
text(model1, digits = 3)

# Make predictions on the test data
predicted.classes <- model1 %>% 
  predict(test.set, type = "class")
head(predicted.classes)

# Compute model accuracy rate on test data
mean(predicted.classes == test.set$diabetes)


### Now prune the tree
install.packages("e1071")

# Fit the model on the training set
set.seed(123)
model2 <- train(
  diabetes ~., data = train.set, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 20
)
# Plot model accuracy vs different values of
# cp (complexity parameter)
plot(model2)

# Print the best tuning parameter cp that
# maximizes the model accuracy
model2$bestTune

# Plot the final tree model
par(xpd = NA) # Avoid clipping the text in some device
plot(model2$finalModel)
text(model2$finalModel,  digits = 3)

# Decision rules in the model
model2$finalModel

# Make predictions on the test data
predicted.classes <- model2 %>% predict(test.set)
# Compute model accuracy rate on test data
mean(predicted.classes == test.set$diabetes)
