#install.packages("neuralnet")
library(neuralnet)
set.seed(500)
library(MASS)
data<-Boston
head(Boston)

#Check that no datapoint is misssing.
apply(data, 2, function(x) sum(is.na(x)))
index <- sample(1:nrow(data), round(0.75*nrow(data)))
train<- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm<- predict(lm.fit, test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
MSE.lm

#Scaling data before building NN. Scaling using min, max. Next I will build scaling using z scores. 
max <- apply(data, 2, max)
min <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center=min, scale = max - min))
head(scaled)
train_ <- scaled[index,]
test_ <- scaled[-index,]
dim(data)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
f  #To see the formula
#nn <- neuralnet(f, data=train_, hidden=c(5,3), linear.output = T)
nn <- neuralnet(f, data=train_, linear.output = T)
summary(nn)
nn #Look at how many steps it took
plot(nn)

#Let's predict

pr.nn <- compute(nn, test_[,1:13])
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
print(paste(MSE.lm,MSE.nn))

#Plotting
par(mfrow=c(1,2))
plot(test$medv, pr.nn_, col='red', main='Real vs predicted NN', pch=18, cex=0.7)
abline(0,1, lwd=2)
legend('bottomright', legend='NN', pch = 18, col='red', bty='n')

plot(test$medv, pr.lm, col='blue', main='Real vs predicted lm', pch=18, cex=0.7)
abline(0,1, lwd=2)
legend('bottomright', legend='LM', pch = 18, col='blue', bty='n')

par(mfrow=c(1,1))
plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))
