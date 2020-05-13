library(xlsx)
library(Metrics)
library(ggplot2)

data <- read.xlsx(("D:/Kuliah/S2/Semester 2/Time Series/Timeseries/Data-spatio-temporal-angin.xlsx"),1, header=TRUE)
head(data)
data$Tanggal <- NULL
head(data)
cor(data)

# require(quantmod)
# data<-as.zoo(data)
# x1<-Lag(data,k=1)

# data_diff = apply(data, 2, diff)
# data_diff <- as.data.frame(data_diff)

split_data <- round(nrow(data) * 0.8)
x_train <- data[1:split_data, ]
x_test <- data[-c(1:split_data),]

weight = matrix(c(1,1,1,1,
                  1,1,1,1,
                  1,1,1,1,
                  1,1,1,1), ncol = 4, nrow = 4)

weight = weight/(ncol(data)-1)

library(gstar)
fit <- gstar(x_train, weight = weight, p = 1, d = 0, est = 'OLS')
summary(fit)

performance(fit)
performance(fit, x_test)
predict(fit, n = 5)

plot(fit)
plot(fit, n_predict = 5)
plot(fit, testing = x_test)

