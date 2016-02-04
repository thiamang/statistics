#install.packages("caret")
#install.packages("ellipse")
#install.packages("e1071")
#install.packages("rpart")
install.packages("kernlab")
library (rpart)
library(caret)
library(ellipse)
library(e1071)
data("iris")
dataset <- iris
head(dataset)

#training set and validation set
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
validation <- dataset[-validation_index,]
dataset <- dataset[validation_index,]

#attributes
dim(dataset)
sapply(dataset, class)
levels(dataset$Species)

#Quick Summary
percentage <- prop.table(table(dataset$Species))*100
cbind(freq=table(dataset$Species), percentage=percentage)
summary(dataset)

#Quick Summary Graphs
#Box Plot
par(mfrow=c(1,4))
x <- dataset[,1:4]
y <- dataset[,5]
for (i in 1:4){
  boxplot(x[i],main=names(iris)[i])
}
plot(y)
#Multivariate Plots
par(mfrow=c(1,1))
featurePlot(x=x, y=y, plot="ellipse")
featurePlot(x=x, y=y, plot="box")
scales <- list(x=list(relation="free"),y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

#Test Harness
#Cross Validation
control <- trainControl(method = "cv", number=10)
metric <- "Accuracy"

#Linear Models - Let us draw a stright line through everything!
set.seed(7)
fit.lda <-train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
#Non Linear
#CART
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
#kNN
fit.knn<- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
#SVM
fit.svm <-train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
#Random Forest
fit.rf<- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

#Accuracy of Results
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)
dotplot(results)
print(fit.lda)

#Predictions
predictions<-predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
