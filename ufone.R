library(ggplot2)
library(gridExtra)

cor(vina)
plot(vina)

file.link <- '/Users/huhwhat/Documents/data/demo/naivebayes/mldemo_vina_normd_newcodetest'
vina.original <- read.table(file.link, sep = ',')

vina <- vina.original
vina <- subset(vina.original, V6 < 2 & V4 < 1 & V3 < 0.1 & V5 < 0)

V1.plot <- ggplot(vina, aes(x = V1)) + geom_histogram()
V2.plot <- ggplot(vina, aes(x = V2)) + geom_density()
V3.plot <- ggplot(vina, aes(x = V3)) + geom_histogram()
V4.plot <- ggplot(vina, aes(x = V4)) + geom_histogram()
V5.plot <- ggplot(vina, aes(x = V5)) + geom_density()
V6.plot <- ggplot(vina, aes(x = V6)) + geom_histogram()

grid.arrange(V1.plot, V2.plot, V3.plot, V4.plot, V5.plot, V6.plot)