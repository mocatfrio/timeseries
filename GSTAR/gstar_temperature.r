
# import libraries
library(xlsx)
library(Metrics)
library(ggplot2)
library(gstar)

# import data
data <- read.xlsx(("data-temperature-2019-2020.xlsx"),1, header=TRUE)
head(data)
tail(data)

# mendapatkan jumlah data
nrow(data)

# drop kolom tanggal
data$Tanggal <- NULL
head(data)

# mendapatkan korelasi
cor(data)

# karena hasilnya NA, maka perlu mengisi null value dengan rata-rata
data$Juanda[is.na(data$Juanda)] <- mean(data$Juanda, na.rm = TRUE)
data$Perak.I[is.na(data$Perak.I)] <- mean(data$Perak.I, na.rm = TRUE)
data$Perak.II[is.na(data$Perak.II)] <- mean(data$Perak.II, na.rm = TRUE)

# mendapatkan korelasi
cor(data)

# split data menjadi training and testing (80:20)
split_data <- round(nrow(data) * 0.8)
x_train <- data[1:split_data, ]
x_test <- data[-c(1:split_data),]

cat("Train set:",  nrow(x_train), "\n")
cat("Test set:",  nrow(x_test))

################
# bobot seragam
################

weight_uniform = matrix(c(0,1,1,
                          1,0,1,
                          1,1,0), ncol = 3, nrow = 3)

# jumlah bobot adalah 1 untuk setiap baris
weight_uniform = weight_uniform/(ncol(data) - 1)

weight_uniform

################
# bobot korelasi
################

weight_cor = cor(data)

# jumlah bobot adalah 1 untuk setiap baris
weight_cor = weight_cor/(ncol(data) - 1)

weight_cor

################
# bobot invers jarak
################

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

# hitung bobot berdasar invers jarak
w12 <- r2/(r1+r2)
w13 <- r1/(r1+r2)
w21 <- r3/(r1+r3)
w23 <- r1/(r1+r3)
w31 <- r3/(r2+r3)
w32 <- r2/(r2+r3)

weight_dist = matrix(c(0,w12,w13,
                       w21,0,w23,
                       w31,w32,0), ncol = 3, nrow = 3)

# jumlah bobot adalah 1 untuk setiap baris
weight_dist = weight_dist/(ncol(data) - 1)

weight_dist

################
# bobot biner
################

# define fungsi untuk mendapatkan min value
is_less_than <- function(distA, distB){
    if(distA < distB) {
        return (1)
    } else {
        return (0)
    }
}

# menggunakan jarak yang telah dihitung sebelumnya pada poin 2.3
wb12 <- is_less_than(r1, r2)
wb13 <- is_less_than(r2, r1)
wb21 <- is_less_than(r1, r3)
wb23 <- is_less_than(r3, r1)
wb31 <- is_less_than(r2, r3)
wb32 <- is_less_than(r3, r2)

weight_biner = matrix(c(0,wb12,wb13,
                        wb21,0,wb23,
                        wb31,wb32,0), ncol = 3, nrow = 3)

weight_biner

# train model dengan bobot seragam
fit <-  gstar(x_train, weight = weight_uniform, p = 1, d = 0, est = "OLS")
summary(fit)

# train model dengan bobot korelasi
fit2 <-  gstar(x_train, weight = weight_cor, p = 1, d = 0, est = "OLS")
summary(fit2)

# train model dengan bobot invers jarak
fit3 <-  gstar(x_train, weight = weight_dist, p = 1, d = 0, est = "OLS")
summary(fit3)

# train model dengan bobot biner
fit4 <-  gstar(x_train, weight = weight_biner, p = 1, d = 0, est = "OLS")
summary(fit4)

# cek performa model dengan bobot seragam
performance(fit, x_test) 

# cek performa model dengan bobot korelasi
performance(fit2, x_test) 

# cek performa model dengan bobot invers jarak
performance(fit3, x_test) 

# cek performa model dengan bobot biner
performance(fit4, x_test) 

# forecast 5 data ahead
predict(fit, n = 5) 

# plot with 5 forecasting data
plot(fit)
plot(fit, n_predict = 5) 
plot(fit, testing = x_test)

# forecast 5 data ahead
predict(fit2, n = 5) 

# plot with 5 forecasting data
plot(fit2)
plot(fit2, n_predict = 5) 
plot(fit2, testing = x_test)

# forecast 5 data ahead
predict(fit3, n = 5) 

# plot with 10 forecasting data
plot(fit3)
plot(fit3, n_predict = 5) 
plot(fit3, testing = x_test)

# forecast 5 data ahead
predict(fit4, n = 5) 

# plot with 5 forecasting data
plot(fit4)
plot(fit4, n_predict = 5) 
plot(fit4, testing = x_test)
