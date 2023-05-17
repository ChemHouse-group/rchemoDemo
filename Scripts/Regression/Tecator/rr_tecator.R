library(rchemo)

path <- "D:/Mes Donnees/Users/Applications/Nirs/Packages/ChemHouse/rchemoDemo/"
db <- paste(path, "Data/tecator.rda", sep = "")
db
load(db)
names(dat)
X <- dat$X
Y <- dat$Y
headm(X)
headm(Y)
typ <- Y$typ
namy <- names(Y)[1:3]
namy
n <- nrow(X)

table(typ)

plotsp(X,
    col = sample(1:n), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Absorbance")

Xp <- savgol(snv(X), n = 15, p = 3, m = 2) 

plotsp(Xp,
    col = sample(1:n), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Absorbance")

## Train, Test
s <- which(typ == "train")
Xtrain <- Xp[s, ]
Ytrain <- Y[s, ]
Xtest <- Xp[-s, ]
Ytest <- Y[-s, ]
headm(Xtrain)
headm(Xtest)
ntrain <- nrow(Xtrain)
ntest <- nrow(Xtest)
ntot <- ntrain + ntest
c(ntot = ntot, ntrain = ntrain, ntest = ntest)

j <- 2  # y-variable
nam <- namy[j]
nam 
ytrain <- Ytrain[, nam]
ytest <- Ytest[, nam]

lb = 1e-3
fm <- rr(Xtrain, ytrain, lb = lb) 
names(fm)

res <- predict(fm, Xtest)
names(res)
res$pred

predict(fm, Xtest, lb = .01)$pred
predict(fm, Xtest, lb = c(.01, .01, .001))$pred

pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)
r2(pred, ytest)
bias(pred, ytest)
mse(pred, ytest)

plotxy(cbind(pred, ytest),
    pch = 16, col = "red3", cex = 1.3,
    main = "Test predictions",
    xlab = "Predicted", ylab = "Observed")   
abline(0, 1, col = "grey")
fm0 <- lowess(ytest ~ pred, f = 1/4)
lines(fm0$x, fm0$y, col = "blue", lwd = 2)

r <- residreg(pred, ytest)
plotxy(cbind(ytest, r),
    pch = 16, col = "red3", cex = 1.3,
    main = "Test set",
    xlab = "Observed", ylab = "Residual")   
abline(h = 0, col = "grey")
fm0 <- lowess(r ~ pred, f = 1/4)
lines(fm0$x, fm0$y, col = "blue", lwd = 2)
