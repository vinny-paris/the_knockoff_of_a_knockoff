blas_1_p_wmc <- c("13m16.411s", "14m43.141s", "14m11.051s")
blas_16_p_wmc <- c("5m47.180s", "5m37.891s", "5m30.492s")
blas_36_p_wmc <- c("6m39.119s", "5m53.157s", "6m28.804s")

blas_1_s <- c("20m26.889s", "21m11.593s", "20m5.464s")
blas_16_s <- c("9m5.364s", "8m51.618s", "8m48.722s")
blas_36_s <- c("8m51.750s", "8m57.555s", "9m33.537s")

blas_1_p <- c('13m53s', '14m30s', '14m27s')
blas_16_p <- c('5m20s', '5m22s', '5m22s')
blas_36_p <- c('5m22s', '5m22s', '5m29s')

blas_1_s_wmc <- c('20m00s', '20m05s', '18m52s')
blas_16_s_wmc <- c('8m46s', '8m36s', '8m28s')
blas_36_s_wmc <- c('8m52s', '9m15s', '9m04s')




diti <- data.frame(blas_1_p_wmc, blas_16_p_wmc, blas_36_p_wmc, blas_1_s, blas_16_s, blas_36_s, blas_1_p, blas_16_p, blas_36_p, blas_1_s_wmc, blas_16_s_wmc, blas_36_s_wmc)
min_con <- function(x) as.duration(ms(x))
cor_data <- apply(diti, 2, min_con)
data <- melt(cor_data)[,2:3]
data$type <- c(rep(c(rep("par", 9), rep("serial",9)), 2))
data$MC <- c(rep('wmc', 9), rep('default', 18), rep('wmc', 9))
data$cores <- kronecker(c(1, 16,36), c(1, 1, 1))
names(data) <- c('prog', 'seconds', 'pbdMPI', 'MC', 'cores')


ggplot(data, aes(interaction(MC, pbdMPI, cores), seconds))  + 
  geom_boxplot(aes(group = interaction(pbdMPI, MC, cores)))  +
  geom_jitter(aes(col = pbdMPI, shape
  = MC, size = as.factor(cores))) + theme(axis.text.x = element_text(angle = 90,
   hjust = 1)) + ggtitle("Run Times") + 
  scale_color_manual(values = c('green', 'blue'))   




diff <- data.frame(data[10:18, 2] - data[1:9, 2])
diff$cores <- as.factor(c(rep(1, 3), rep(16, 3), rep(36, 3)))
names(diff) <- c('difference', 'cores')

ggplot(diff, aes(cores, difference)) + geom_point(aes(col = cores)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Difference in Run Times") + scale_color_manual(values = c('green', 'blue', 'red'))        


#mostly for laughs
data$cores <- kronecker(rep(c(1, 16, 36), 2), rep(1, 3))
mod <- aov(seconds ~ MC*type*cores, data)
summary(mod)
plot(mod)






                                              