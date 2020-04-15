# import data
data("chicken", package="astsa")
plot(chicken,xlab="Date", ylab="Price(cents)", col="darkblue")
data<-chicken

# Library
library(rnn)
library(Metrics)
library(ggplot2)

require(quantmod)
data<-as.zoo(data)
x1<-Lag(data,k=1)
x2<-Lag(data,k=2)
x3<-Lag(data,k=3)
x4<-Lag(data,k=4)
x<-cbind(x1,x2,x3,x4,data)
x<-log(x)
head(round(x,2))

x <-x[-(1:4), ]
head(round(x , 2 ) )

x<-data.matrix(x)
range_data<-function(x){(x-min(x))/(max(x)-min(x))}

min_data<-min(x)
max_data<-max(x)
x<-range_data(x)
x1<-as.matrix(x[,1])
x2<-as.matrix(x[,2])
x3<-as.matrix(x[,3])
x4<-as.matrix(x[,4])
y<-as.matrix(x[,5])
n_train<-170
y_train<-as.matrix(y[1:n_train])
x1_train<-as.matrix(t(x1[1:n_train,]))
x2_train<-as.matrix(t(x2[1:n_train,]))
x3_train<-as.matrix(t(x3[1:n_train,]))
x4_train<-as.matrix(t(x4[1:n_train,]))
x_train<-array(c(x1_train, x2_train, x3_train, x4_train), dim=c(dim(x1_train),4))

require(rnn)
set.seed(2018)
model1<-trainr(Y=t(y_train),
               X=x_train,learningrate=0.05,
               hidden_dim=3,
               numepochs=500,
               network_type="rnn",
               sigmoid="logistic")

error_1<-t(model1$error)
rownames(error_1)<-1:nrow(error_1)
colnames(error_1)<-"error"
plot(error_1)
pred1_train<-t(predictr(model1,x_train))
round(cor(y_train,pred1_train),5)

plot(y_train,pred1_train,ylab="pred1_train")
cor(y_train, pred1_train)

x1_test<-as.matrix(t(x1[(n_train+1):nrow(x1),]))
x2_test<-as.matrix(t(x2[(n_train+1):nrow(x2),]))
x3_test<-as.matrix(t(x3[(n_train+1):nrow(x3),]))
x4_test<-as.matrix(t(x4[(n_train+1):nrow(x4),]))
y_test<-as.matrix(y[(n_train+1):nrow(x4)])

x_test<-array(c(x1_test, x2_test, x3_test, x4_test), dim=c(dim(x1_test),4))

pred1_test<-t(predictr(model1,x_test))

unscale_data<-function(x,max_x,min_x)
{x*(max_x-min_x)+min_x}

## Kembalikan ke Skala Awal unt Training
train_actual<-unscale_data(pred1_train, max_data, min_data)
train_actual<-exp(train_actual)
train_actual<-ts(matrix(train_actual), end=c(2016,1), frequency=12)

y_ori<-unscale_data(y_train, max_data, min_data)
y_ori<-exp(y_ori)
y_ori<-ts(matrix(y_ori), end=c(2016,1), frequency=12) 
result_all_train<-cbind(y_ori, round(train_actual,2))


## Kembalikan ke Skala Awal unt Testing
pred1_actual<-unscale_data(pred1_test, max_data, min_data)
pred1_actual<-exp(pred1_actual)
pred1_actual<-ts(matrix(pred1_actual), end=c(2016,7), frequency=12)

y_actual<-unscale_data(y_test, max_data, min_data)
y_actual<-exp(y_actual)
y_actual<-ts(matrix(y_actual), end=c(2016,7), frequency=12) 
result_all_test<-cbind(y_actual, round(pred1_actual,2))

## Contoh2 Cara Evaluasi Training dan Testing
plot(y_ori, col="blue", type="l",main ="Training : Actual vs Predicted Curve", lwd = 2) 
lines(train_actual, type = "l", col = "red", lwd = 1)

plot(y_actual, col="blue", type="l",main ="Training : Actual vs Predicted Curve", lwd = 2) 
lines(pred1_actual, type = "l", col = "red", lwd = 1)

# Kinerja Model : r, R-square, MSE, MAPE

#cor(result_all_test[,1], result_all_test[,2])
#result_all_test[,1] = data aktual
#result_all_test[,2] = data prediksi