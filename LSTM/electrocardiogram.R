# import data
require ( "wmtsa" ) 
plot(ecg ,xlab="Date" , ylab=" Signal " , col="darkblue")

# convert data as numeric
data <- as.numeric(ecg)

require(quantmod)
# lag
data <- as.zoo(data)
x1 <- Lag(data, k=1)
x2 <- Lag(data, k=2)
x3 <- Lag(data, k=3)
x4 <- Lag(data, k=4)
# combine lag
x <- cbind(x1, x2, x3, x4, data)

# remove the missing values
x <- x[-(1:4),]

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
set.seed(2018)
model1 <- trainr(
  Y = t(y_train), 
  X = t(x_train),
  learningrate = 0.05,
  hidden_dim = 3,
  numepochs = 300,
  network_type = 'lstm',
  sigmoid = "tanh"
  )

# evaluate performance
