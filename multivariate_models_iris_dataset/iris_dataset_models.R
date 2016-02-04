#install.packages("caret")
library(caret)
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
featurePlot(x, y, "ellipse")
featurePlot(x=x, y=y, plot="ellipse")
