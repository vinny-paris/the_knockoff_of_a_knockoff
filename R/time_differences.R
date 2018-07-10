blas_1_p <- c("13m16.411s", "14m43.141s", "14m11.051s")
blas_16_p <- c("5m47.180s", "5m37.891s", "5m30.492s")
blas_36_p <- c("6m39.119s", "5m53.157s", "6m28.804s")

blas_1_s <- c("20m26.889s", "21m11.593s", "20m5.464s")
blas_16_s <- c("9m5.364s", "8m51.618s", "8m48.722s")
blas_36_s <- c("8m51.750s", "8m57.555s", "9m33.537s")

diti <- data.frame(blas_1_p, blas_16_p, blas_36_p, blas_1_s, blas_16_s, blas_36_s)
min_con <- function(x) as.duration(ms(x))
cor_data <- apply(diti, 2, min_con)
data <- melt(cor_data)[,2:3]
data$type <- c(rep("par", 9), rep("serial",9))
names(data) <- c('prog', 'seconds', 'type')
ggplot(data, aes(prog, seconds)) + geom_point(aes(col = type)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Run Times") + scale_color_manual(values = c('green', 'blue'))        


diff <- data.frame(data[10:18, 2] - data[1:9, 2])
diff$cores <- as.factor(c(rep(1, 3), rep(16, 3), rep(36, 3)))
names(diff) <- c('difference', 'cores')
ggplot(diff, aes(cores, difference)) + geom_point(aes(col = cores)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Difference in Run Times") + scale_color_manual(values = c('green', 'blue', 'red'))        


#mostly for laughs
data$cores <- kronecker(rep(c(1, 16, 36), 2), rep(1, 3))
mod <- aov(seconds ~ cores + type + cores*type, data)
summary(mod)
plot(mod)






                                              