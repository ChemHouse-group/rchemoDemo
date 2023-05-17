library(rchemo)

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

nlv <- 20
gamma = .001
fm <- kplsrda(Xtrain, ytrain, nlv = 15, 
    gamma = gamma) 
res <- predict(fm, Xtest)
names(res)
res$posterior
pred <- res$pred
pred
err(pred, ytest)

## Confusion matrix
tab <- table(ytest, pred)
tab / sum(tab)
round(tab / rowSums(tab), digits = 2)

