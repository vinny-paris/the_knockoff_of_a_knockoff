library(stringr)
library(ggplot2)


final <- NULL

files <- list.files()
files <- files[grep('\\.o', files)]
files <- files[files != "time_cc_openblas2_mc1_2.o388840"]
for(file in files){
data <- readLines(file)
data <- data[!grepl('\\[1\\]', data)]

rank0 <- data[20:24]
d <- str_split(rank0, " ")
library(purrr)
holding <- t(map_dfc(d, function(x) x[x != ""]))
colnames(holding) <- holding[1,]
holding <- as.data.frame(holding[-1,])
holding$comm_rank <- "rank_0"


rank1 <- data[27:31]
d <- str_split(rank1, " ")
helding <- t(map_dfc(d, function(x) x[x != ""]))
colnames(helding) <- helding[1,]
helding <- as.data.frame(helding[-1,])
helding$comm_rank <- "rank_1"


stuff <- str_split(file, "_")
blas <- stuff[[1]][3]
mc <- stuff[[1]][4]

hilding <- rbind(holding, helding)

hilding$mc <- mc
hilding$blas <- blas

final <- rbind(final, hilding)
}
final$time <- as.numeric(levels(final$time))[final$time]
final$blas <- factor(final$blas, levels = c('openblas1', 'openblas2', 'openblas4', 'openblas8', 'openblas16', 'openblas36'))
final$mc <- factor(final$mc, levels = c('mc1', 'mc2', 'mc4', 'mc8', 'mc16', 'mc32'))
ggplot(final, aes(Section, time)) + geom_point() + geom_boxplot() + theme(axis.text.x = element_text(angle = 90,
                                                                                                     hjust = 1))


ggplot(final, aes(interaction(mc, blas), time)) + geom_jitter(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90,
                                                                                                                  hjust = 1))

ggplot(subset(final, Section == 'code3'), aes(interaction(mc, blas), time)) + geom_jitter(aes(col = blas))+ theme(axis.text.x = element_text(angle = 90,
                                                                                                                                             hjust = 1))

ggplot(subset(final, Section == 'code3' & comm_rank == 'rank_0'), aes(interaction(mc, blas), time)) + geom_point(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90,
                                                                                                                                                                      hjust = 1))
ggplot(subset(final, Section == 'code3' & comm_rank == 'rank_1'), aes(interaction(mc, blas), time)) + geom_point(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90,
                                                                                                                                                                      hjust = 1))

ggplot(subset(final, Section == 'code3' & comm_rank == 'rank_0'), aes(interaction(blas, mc), time)) + geom_point(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
                                                                                                                                                                    
ggplot(subset(final, Section == 'code3' & comm_rank == 'rank_1'), aes(interaction(blas, mc), time)) + geom_point(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
                                                                                                                                                                     
 



















ggplot(subset(final, Section == 'code2' & comm_rank == 'rank_0'), aes(interaction(mc, blas), time)) + geom_point(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90,
                                                                                                                                                                     hjust = 1))
ggplot(subset(final, Section == 'code2' & comm_rank == 'rank_1'), aes(interaction(mc, blas), time)) + geom_point(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90,
                                                                                                                                                                     hjust = 1))
ggplot(subset(final, Section == 'code2' & comm_rank == 'rank_0'), aes(interaction(blas, mc), time)) + geom_point(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(subset(final, Section == 'code2' & comm_rank == 'rank_1'), aes(interaction(blas, mc), time)) + geom_point(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


total_only <- subset(final, Section == 'total')
total_diff <- total_only$time[(1:104)*2] - total_only$time[(1:104)*2-1]
head(total_diff)
plot(total_diff)





ggplot(subset(final, Section == 'total' & comm_rank == 'rank_0'), aes(interaction(mc, blas), time)) + geom_point(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90,
                                                                                                                                                                     hjust = 1))
ggplot(subset(final, Section == 'total' & comm_rank == 'rank_1'), aes(interaction(mc, blas), time)) + geom_point(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90,
                                                                                                                                                                     hjust = 1))
ggplot(subset(final, Section == 'total' & comm_rank == 'rank_0'), aes(interaction(blas, mc), time)) + geom_point(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(subset(final, Section == 'total' & comm_rank == 'rank_1'), aes(interaction(blas, mc), time)) + geom_point(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))










ggplot(subset(final, Section == 'total' & comm_rank == 'rank_1'), aes(interaction(mc), time)) + geom_boxplot() + geom_point(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(subset(final, Section == 'code3' & comm_rank == 'rank_1'), aes(interaction(mc), time)) + geom_boxplot() + geom_point(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(subset(final, Section == 'code2' & comm_rank == 'rank_0'), aes(interaction(mc), time)) + geom_boxplot() + geom_point(aes(col = blas)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

blas_mean <- aggregate(subset(final, Section == 'code2' & comm_rank == 'rank_1')$time, by = list(blas), mean)

sub_r1 <- subset(final, Section == 'code3' & comm_rank == 'rank_1')
blas_mean_r1 <- aggregate(sub_r1, by=list(sub_r1$blas), mean)$time
sub_r0 <- subset(final, Section == 'code3' & comm_rank == 'rank_0')
blas_mean_r0 <- aggregate(sub_r0, by=list(sub_r0$blas), mean)$time

sub_r0







                                                                                                                                                                    