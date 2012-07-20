file.link <- '/Users/huhwhat/Documents/data/demo/naivebayes/winequality-white.csv'
wine <- read.table(file.link, sep = ';', header = TRUE)

library(ggplot2)
library(gridExtra)

fixed.acidity.plot <- ggplot(wine, aes(x = fixed.acidity)) + geom_density()
volatile.acidity.plot <- ggplot(wine, aes(x = volatile.acidity)) + geom_density()
citric.acid.plot <- ggplot(wine, aes(x = citric.acid)) + geom_density()
residual.sugar.plot <- ggplot(wine, aes(x = residual.sugar)) + geom_density()
chlorides.plot <- ggplot(wine, aes(x = chlorides)) + geom_density()
free.sulfur.dioxide.plot <- ggplot(wine, aes(x = free.sulfur.dioxide)) + geom_density()
total.sulfur.dioxide.plot <- ggplot(wine, aes(x = total.sulfur.dioxide)) + geom_density()
density.plot <- ggplot(wine, aes(x = density)) + geom_density()
pH.plot <- ggplot(wine, aes(x = pH)) + geom_density()
sulphates.plot <- ggplot(wine, aes(x = sulphates)) + geom_density()
alcohol.plot <- ggplot(wine, aes(x = alcohol)) + geom_density()
quality.plot <- ggplot(wine, aes(x = quality)) + geom_histogram()

grid.arrange(fixed.acidity.plot, volatile.acidity.plot, citric.acid.plot, 
             residual.sugar.plot, chlorides.plot, free.sulfur.dioxide.plot,
             total.sulfur.dioxide.plot, density.plot, pH.plot, 
             sulphates.plot, alcohol.plot, quality.plot)

wine.2 <- subset(wine, fixed.acidity < 10 & volatile.acidity < 0.6 & 
                 citric.acid < 0.8 & residual.sugar < 25 & chlorides < 0.1 & 
                 free.sulfur.dioxide < 100 & total.sulfur.dioxide < 300 & 
                 density < 1.01 & sulphates < 0.9) 

fixed.acidity.plot <- ggplot(wine.2, aes(x = fixed.acidity)) + geom_density()
volatile.acidity.plot <- ggplot(wine.2, aes(x = volatile.acidity)) + geom_density()
citric.acid.plot <- ggplot(wine.2, aes(x = citric.acid)) + geom_density()
residual.sugar.plot <- ggplot(wine.2, aes(x = residual.sugar)) + geom_density()
chlorides.plot <- ggplot(wine.2, aes(x = chlorides)) + geom_density()
free.sulfur.dioxide.plot <- ggplot(wine.2, aes(x = free.sulfur.dioxide)) + geom_density()
total.sulfur.dioxide.plot <- ggplot(wine.2, aes(x = total.sulfur.dioxide)) + geom_density()
density.plot <- ggplot(wine.2, aes(x = density)) + geom_density()
pH.plot <- ggplot(wine.2, aes(x = pH)) + geom_density()
sulphates.plot <- ggplot(wine.2, aes(x = sulphates)) + geom_density()
alcohol.plot <- ggplot(wine.2, aes(x = alcohol)) + geom_density()
quality.plot <- ggplot(wine.2, aes(x = quality)) + geom_histogram()

grid.arrange(fixed.acidity.plot, volatile.acidity.plot, citric.acid.plot, 
             residual.sugar.plot, chlorides.plot, free.sulfur.dioxide.plot,
             total.sulfur.dioxide.plot, density.plot, pH.plot, 
             sulphates.plot, alcohol.plot, quality.plot)

output.file.link <- '/Users/huhwhat/Documents/data/demo/misc/wine2'
write.table(wine.2, file = output.file.link, row.names = FALSE)

wine.3 <- subset(wine.2, select = - residual.sugar)

output.file.link <- '/Users/huhwhat/Documents/data/demo/misc/wine3'
write.table(wine.3, file = output.file.link, row.names = FALSE)

fixed.acidity.plot <- ggplot(wine.2, aes(x = fixed.acidity)) + geom_density() + facet_wrap(~ quality)
volatile.acidity.plot <- ggplot(wine.2, aes(x = volatile.acidity)) + geom_density() + facet_wrap(~ quality)
citric.acid.plot <- ggplot(wine.2, aes(x = citric.acid)) + geom_density() + facet_wrap(~ quality)
residual.sugar.plot <- ggplot(wine.2, aes(x = residual.sugar)) + geom_density() + facet_wrap(~ quality)
chlorides.plot <- ggplot(wine.2, aes(x = chlorides)) + geom_density() + facet_wrap(~ quality)
free.sulfur.dioxide.plot <- ggplot(wine.2, aes(x = free.sulfur.dioxide)) + geom_density() + facet_wrap(~ quality)
total.sulfur.dioxide.plot <- ggplot(wine.2, aes(x = total.sulfur.dioxide)) + geom_density() + facet_wrap(~ quality)
density.plot <- ggplot(wine.2, aes(x = density)) + geom_density() + facet_wrap(~ quality)
pH.plot <- ggplot(wine.2, aes(x = pH)) + geom_density() + facet_wrap(~ quality)
sulphates.plot <- ggplot(wine.2, aes(x = sulphates)) + geom_density() + facet_wrap(~ quality)
alcohol.plot <- ggplot(wine.2, aes(x = alcohol)) + geom_density() + facet_wrap(~ quality)

grid.arrange(fixed.acidity.plot, volatile.acidity.plot, citric.acid.plot, 
             residual.sugar.plot, chlorides.plot, free.sulfur.dioxide.plot,
             total.sulfur.dioxide.plot, density.plot, pH.plot, 
             sulphates.plot, alcohol.plot)

cor(subset(wine, select = - quality), method = 'kendall')
plot(prcomp(subset(wine, select = - quality)))
wine.2.pca <- princomp(subset(wine.2, select = - quality))
wine.2.pca <- princomp(wine.2)
loadings(wine.2.pca)
biplot(wine.2.pca, cex = 0.75)

free.sulfur.dioxide.plot <- ggplot(wine.2, aes(x = free.sulfur.dioxide)) + geom_density() + facet_wrap(~ quality)
total.sulfur.dioxide.plot <- ggplot(wine.2, aes(x = total.sulfur.dioxide)) + geom_density() + facet_wrap(~ quality)

grid.arrange(free.sulfur.dioxide.plot, total.sulfur.dioxide.plot)

wine.4 <- subset(wine.2, quality >= 5 & quality <= 7)
wine.4.pca <- princomp(subset(wine.4, select = - quality))
loadings(wine.4.pca)
biplot(wine.4.pca, cex = 0.75)

output.file.link <- '/Users/huhwhat/Documents/data/demo/misc/wine4'
write.table(wine.4, file = output.file.link, row.names = FALSE)

wine.5 <- wine.2
wine.5$quality[wine.5$quality == 4] <- 3
wine.5$quality[wine.5$quality == 5] <- 6
wine.5$quality[wine.5$quality == 7] <- 6
wine.5$quality[wine.5$quality == 8] <- 9

output.file.link <- '/Users/huhwhat/Documents/data/demo/misc/wine5'
write.table(wine.5, file = output.file.link, row.names = FALSE)

ggplot(subset(wine.2, quality >=5 & quality <= 7), 
       aes(x = total.sulfur.dioxide, y = free.sulfur.dioxide)) +
  aes(shape = factor(quality)) + geom_point(aes(color = factor(quality))) +
  scale_color_brewer(palette='Set1')

plot(wine, mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))