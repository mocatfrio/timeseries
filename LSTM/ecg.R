# import data
require ( "wmtsa" ) 
plot(ecg ,xlab="Date" , ylab=" Signal " , col="darkblue")

# convert data as numeric
data <- as.numeric(ecg)
library(rnn)
require(quantmod)
# lag
data <- as.zoo(data)
x1 <- Lag(data, k=1)
x2 <- Lag(data, k=2)
x3 <- Lag(data, k=3)
x4 <- Lag(data, k=4)
# combine lag
x <- cbind(x1, x2, x3, x4, data)
head(round(x , 3 ) )

# remove the missing values
x <- x[-(1:5),]

# scale data
x <- data.matrix(x)
range_data <- function(x) {
  (x-min(x))/(max(x)-min(x))
}
min_data <- min(x)
max_data <- max(x)
x <- range_data(x)

# create train and test datasets
x1 <- as.matrix(x[,1]) 
x2 <- as.matrix(x[,2]) 
x3 <- as.matrix(x[,3]) 
x4 <- as.matrix(x[,4]) 
y <- as.matrix(x[,5]) 

# determine train data
n_train <- 1950
# split train dataset
y_train <- as.matrix(y[1:n_train])
x1_train <- as.matrix(t(x1[1:n_train,]))
x2_train <- as.matrix(t(x2[1:n_train,]))
x3_train <- as.matrix(t(x3[1:n_train,]))
x4_train <- as.matrix(t(x4[1:n_train,]))
x_train <- array(c(x1_train, x2_train, x3_train, x4_train), dim=c(dim(x1_train), 4))

# specify model
require(rnn)
set.seed(256)
model1 <- trainr(
  Y = t(y_train), 
  X = x_train,
  learningrate = 0.01,
  hidden_dim = 4,
  numepochs = 400,
  momentum = 0.99,
  network_type = 'lstm',
  sigmoid = "tanh"
)

# evaluate performance
error_1 <- t(model1$error)
rownames(error_1) <- 1:nrow(error_1)
colnames(error_1) <- "error"

plot(error_1, ylab="Training Error", xlab="Epochs")

# get predict values
pred1_train <- t(predictr(model1, x_train))
require(Metrics)
round (rmse(y_train, pred1_train),3)
round(cor(y_train, pred1_train),3)[,1]
plot(as.ts(pred1_train, ylab="Signal"))

# test set performance
x1_test <- as.matrix(t(x1[(n_train + 1) : nrow(x1),]))
x2_test <- as.matrix(t(x2[(n_train + 1) : nrow(x2),]))
x3_test <- as.matrix(t(x3[(n_train + 1) : nrow(x3),]))
x4_test <- as.matrix(t(x4[(n_train + 1) : nrow(x4),]))
y_test <- as.matrix(y[(n_train + 1) : nrow(x4)])

x_test <- array(c(x1_test, x2_test, x3_test, x4_test), dim = c(dim(x1_test),4))
dim(x_test)

pred1_test <- t(predictr(model1, x_test))


# unscale data
unscale_data <- function(x, max_x, min_x)
{x*(max_x - min_x) + min_x}

train_actual <- unscale_data(pred1_train, max_data, min_data)
train_actual <- exp(train_actual)
train_actual <- ts(matrix(train_actual), end=c(2016,1), frequency=12)

y_ori <- unscale_data(y_train, max_data, min_data)
y_ori <- exp(y_ori)
y_ori <- ts(matrix(y_ori), end=c(2016,1), frequency=12)

pred1_actual <- unscale_data(pred1_test, max_data, min_data)
pred1_actual <- exp(pred1_actual)
pred1_actual <- ts(matrix(pred1_actual), end=c(2016,7), frequency=12)

y_actual <- unscale_data(y_test, max_data, min_data)
y_actual <- exp(y_actual)
y_actual <- ts(matrix(y_actual), end=c(2016,7), frequency=12)

# Visual inspection
result_all_train <- cbind(y_ori, round(train_actual, 2))
colnames(result_all_train) <- c("actual", "Model1")
plot(result_all_train)

result_all_test <- cbind(y_actual, round(pred1_actual, 2))
colnames(result_all_test) <- c("actual", "Model1")
plot(result_all_test)

plot(y_ori, col="blue", type="l",main ="Training : Actual vs Predicted Curve", lwd = 2) 
lines(train_actual, type = "l", col = "red", lwd = 1)

plot(y_actual, col="blue", type="l",main ="Testing : Actual vs Predicted Curve", lwd = 2) 
lines(pred1_actual, type = "l", col = "red", lwd = 1)

korelasi_train <-cor(y_ori, train_actual)
r_sq_train <- korelasi_train^2

korelasi_test <-cor(y_actual, pred1_actual)
r_sq_test <- korelasi_test^2

mape_train <-(sum(abs(result_all_train[,1] - result_all_train[,2])/result_all_train[1]/n_train))
mse_train <- sum((result_all_train[,1] - result_all_train[,2])^2)/n_train

mape_test <-(sum(abs(result_all_test[,1] - result_all_test[,2])/result_all_test[1]/length(y_test)))
mse_test <- sum((result_all_test[,1] - result_all_test[,2])^2)/length(y_test)





