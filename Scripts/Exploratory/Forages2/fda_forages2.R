library(rchemo)

## forages: data are already preprocessed (snv + savgol 2d)

data(forages)
names(forages)
Xtrain <- forages$Xtrain
ytrain <- forages$ytrain
Xtest <- forages$Xtest
ytest <- forages$ytest
ntrain <- nrow(Xtrain)
ntest <- nrow(Xtest)
ntot <- ntrain + ntest
c(ntot = ntot, ntrain = ntrain, ntest = ntest)

table(ytrain)
table(ytest)

plotsp(Xtrain,
    col = sample(1:ntrain), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Absorbance")

## Preliminary PCA
nlv <- 10
fm0 <- pcasvd(Xtrain, nlv = nlv)
Ttrain <- fm0$T
Ttest <- transform(fm0, Xtest)
headm(Ttrain)
headm(Ttest)

## FDA on the PCA scores

nlv <- 3
fm <- fda(Ttrain, ytrain, nlv = nlv)
#fm <- fdasvd(Ttrain, ytrain, nlv = nlv)
names(fm)
headm(fm$T)
summary(fm)

zTtest <- transform(fm, Ttest)
headm(zTtest)

i <- 1
plotxy(fm$T[, i:(i + 1)], group = ytrain, ellipse = FALSE, 
    zeroes = TRUE, pch = 16, cex = 1.5, ncol = 2)
points(fm$Tcenters, pch = 8, col = "blue", cex = 1.5)

