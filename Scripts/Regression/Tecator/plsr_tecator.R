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
wl_num <- as.numeric(names(X))

table(typ)

plotsp(X,
    col = sample(1:n), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Absorbance")

Xp <- savgol(snv(X), n = 15, p = 3, m = 2) 

plotsp(Xp,
    col = sample(1:n), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Absorbance")

## Splitting Tot = Train + Test
## The model is fitted on Train, and
## the generalization error will be estimated on Test.
## Here the splitting of Tot is provided by the dataset
## (= variable "typ"), but data Tot could be splitted 
## a posteriori (e.g. random sampling, systematic 
## sampling, etc.) 
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

## Work on the second y-variable 
j <- 2
nam <- namy[j]
nam 
ytrain <- Ytrain[, nam]
ytest <- Ytest[, nam]

## Model fitting
nlv <- 15
fm <- plskern(Xtrain, ytrain, nlv = nlv) ;
names(fm)

headm(fm$T)
headm(fm$P)
headm(fm$C)
fm$weights

res <- coef(fm)
#res <- coef(fm, nlv = 3)
res
plot(wl_num, abs(res$B), type = "l")
abline(h = 0, col = "grey")


## Projections

Ttest <- transform(fm, Xtest)
#Ttest <- transform(fm, Xtest, nlv = 3)
headm(Ttest)

## Predictions
res <- predict(fm, Xtest)
names(res)
res$pred

predict(fm, Xtest, nlv = 2)$pred
predict(fm, Xtest, nlv = 0:2)$pred

pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)    # RMSEP_Test
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

## To compute RMSEP_C
pred <- predict(fm, Xtrain)$pred
rmsep(pred, ytrain)
r2(pred, ytrain)
plotxy(cbind(pred, ytrain),
    pch = 16, col = "red3", cex = 1.3,
    main = "Training set",
    xlab = "Predicted", ylab = "Observed")   
abline(0, 1, col = "grey")

