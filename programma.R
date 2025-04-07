# 1. uzdevums 
kordat <- read.table(file = "C:\\Users\\Liva\\Documents\\data\\variants10.txt", 
                     header = TRUE, stringsAsFactors = FALSE, dec = ",", strip.white = TRUE, row.names = 1)

# 2. uzdevums 
kordat[, 9:ncol(kordat)] <- lapply(kordat[, 9:ncol(kordat)], as.factor)

# 3. un 4. uzdevums 
izvade <- lapply(kordat[, 9:ncol(kordat)], table)
capture.output(izvade, file = "C:\\Users\\Liva\\Documents\\data\\results.txt")

# 5. uzdevums 
sl.by.b <- tapply(kordat$Slope, kordat$b, c)
capture.output(sl.by.b, file = "C:\\Users\\Liva\\Documents\\data\\results.txt", append = TRUE)

#6. uzdevums
kordat$Average <- rowMeans(kordat[, c("Slope", "Intercept", "adj.r.squared")], na.rm = TRUE)

#7. uzdevums
standartnovirzes <- sapply(kordat[, sapply(kordat, is.numeric)], function(x) tapply(x, kordat$f, sd, na.rm = TRUE))
capture.output(standartnovirzes, file = "C:\\Users\\Liva\\Documents\\data\\results.txt", append = TRUE)

#8.uzdevums
adj <- kordat$adj.r.squared
prockordat <- if (any(adj > 0)) kordat[adj > 0.7, ] else kordat[adj > -0.3, ]

#9.uzdevums
prockordat$Slope <- 1 - 1 / prockordat$Slope

#10.uzdevums
capture.output(prockordat, file = "C:\\Users\\Liva\\Documents\\data\\results.txt", append = TRUE)

#11. uzdevums
svg("C:\\Users\\Liva\\Documents\\data\\scatter.svg")
plot(kordat$MAD, kordat$Average, xlab = "MAD", ylab = "Average", main = "Izkliedes grafiks")
dev.off()

#12. uzdevums
svg("C:\\Users\\Liva\\Documents\\data\\boxplot.svg")
boxplot(Intercept ~ f, data = kordat, col = rainbow(length(levels(kordat$f))),
        main = "Intercept pēc f faktora", xlab = "f", ylab = "Intercept")
dev.off()