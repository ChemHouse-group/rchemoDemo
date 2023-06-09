library(rchemo)

path <- "D:/Mes Donnees/Users/Applications/Nirs/Packages/ChemHouse/rchemoDemo/"
db <- paste(path, "Data/forages2.rda", sep = "")
db
load(db)
names(dat)
X <- dat$X
Y <- dat$Y
headm(X)
headm(Y)
y <- Y$typ
test <- Y$test
namy <- names(Y)[1:2]
namy
n <- nrow(X)

table(y, test)

s <- which(test == 1)
Xtrain <- X[-s, ]
ytrain <- y[-s]
Xtest <- X[s, ]
ytest <- y[s]
headm(Xtrain)
headm(Xtest)
ntrain <- nrow(Xtrain)
ntest <- nrow(Xtest)
ntot <- ntrain + ntest
c(ntot = ntot, ntrain = ntrain, ntest = ntest)

plotsp(Xtrain,
    col = sample(1:ntrain), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Absorbance")

nlv <- 20
fm <- plsrda(Xtrain, ytrain, nlv = nlv)     # Usual "PLSDA"
#fm <- plslda(Xtrain, ytrain, nlv = nlv)    # PLS-LDA
#fm <- plslda(Xtrain, ytrain, nlv = nlv, prior = "prop")  
#fm <- plsqda(Xtrain, ytrain, nlv = nlv)    # PLS-QDA
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

