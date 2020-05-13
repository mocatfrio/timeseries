library(xlsx)
library(Metrics)
library(ggplot2)
library(gstar)

data <- read.xlsx(("data-temperature-2019-2020.xlsx"),1, header=TRUE)
head(data)
tail(data)

nrow(data)

data$Tanggal <- NULL
head(data)

cor(data)

# fill null value with avg
data$Juanda[is.na(data$Juanda)] <- mean(data$Juanda, na.rm = TRUE)
data$Perak.I[is.na(data$Perak.I)] <- mean(data$Perak.I, na.rm = TRUE)
data$Perak.II[is.na(data$Perak.II)] <- mean(data$Perak.II, na.rm = TRUE)
format(round(data, 2), nsmall = 2)

cor(data)

## split into training and testing (80:20)
split_data <- round(nrow(data) * 0.8)
x_train <- data[1:split_data, ]
x_test <- data[-c(1:split_data),]

# weight uniform
weight_uniform = matrix(c(0,1,1,
                          1,0,1,
                          1,1,0), ncol = 3, nrow = 3)

weight_uniform = weight_uniform/(ncol(data) - 1) #the sum of weight is equal to 1 every row
weight_uniform

# weight based on correlation
weight_cor = cor(data)
weight_cor = weight_cor/(ncol(data) - 1) #the sum of weight is equal to 1 every row
weight_cor

# fungsi hitung euclidean
euclidean_dist <- function(locA, locB){
    dist <- sqrt((locB[1]-locA[1])^2 + (locB[2]-locA[2])^2)
    return(dist)
}

# define jarak
loc_perak_1 = c(-7.22360, 112.72390)
loc_perak_2 = c(-7.20530, 112.73530)
loc_juanda = c(-7.38460, 112.78330)

# r1 jarak Perak I - Perak II
r1 <- euclidean_dist(loc_perak_1, loc_perak_2)
# r2 jarak Perak I - Juanda
r2 <- euclidean_dist(loc_perak_1, loc_juanda)
# r2 jarak Perak II - Juanda
r3 <- euclidean_dist(loc_perak_2, loc_juanda)

cat("Jarak Perak I - Perak II:", r1, "\n")
cat("Jarak Perak I - Juanda:", r2, "\n")
cat("Jarak Perak II - Juanda:", r3, "\n")

# hitung weight
w12 <- r2/(r1+r2)
w13 <- r1/(r1+r2)
w21 <- r3/(r1+r3)
w23 <- r1/(r1+r3)
w31 <- r3/(r2+r3)
w32 <- r2/(r2+r3)

weight_dist = matrix(c(0,w12,w13,
                       w21,0,w23,
                       w31,w32,0), ncol = 3, nrow = 3)

weight_dist = weight_dist/(ncol(data) - 1) #the sum of weight is equal to 1 every row
weight_dist

fit <-  gstar(x_train, weight = weight_uniform, p = 1, d = 0, est = "OLS")
summary(fit)

fit2 <-  gstar(x_train, weight = weight_cor, p = 1, d = 0, est = "OLS")
summary(fit2)

fit3 <-  gstar(x_train, weight = weight_dist, p = 1, d = 0, est = "OLS")
summary(fit3)

# performace of gstar with weight uniform
performance(fit)
performance(fit, x_test) 

#forecast 5 data ahead
predict(fit, n = 5) 

#plot with 5 forecasting data
plot(fit)
plot(fit, n_predict = 5) 
plot(fit, testing = x_test)

# performace of gstar with correlation as weight
performance(fit2)
performance(fit2, x_test)

#forecast 5 data ahead
predict(fit2, n = 5) 

#plot with 5 forecasting data
plot(fit2)
plot(fit2, n_predict = 5) 
plot(fit2, testing = x_test)

# performace of gstar with weight based on distance
performance(fit3)
performance(fit3, x_test)

#forecast 5 data ahead
predict(fit3, n = 5) 

#plot with 5 forecasting data
plot(fit3)
plot(fit3, n_predict = 5) 
plot(fit3, testing = x_test)


