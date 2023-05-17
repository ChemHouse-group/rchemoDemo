library(rchemo)

path <- "D:/Mes Donnees/Users/Applications/Nirs/Packages/ChemHouse/rchemoDemo/"
db <- paste(path, "Data/challenge2018.rda", sep = "")
db
load(db)
names(dat)
X <- dat$X
Y <- dat$Y
headm(X)
headm(Y)
y <- Y$conc
typ <- Y$typ
test <- Y$test
n <- nrow(X)

table(typ, test)
unique(Y[, c("typ", "label")])

m <- 50
plotsp(X[sample(1:n, m), ],
    col = sample(1:n, m), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Reflectance")

Xp <- savgol(snv(X), n = 21, p = 3, m = 2) 

m <- 50
plotsp(Xp[sample(1:n, m), ],
    col = sample(1:n, m), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Reflectance")

## Train, Test
s <- which(test == 1)
Xtrain <- Xp[-s, ]
ytrain <- y[-s]
Xtest <- Xp[s, ]
ytest <- y[s]
headm(Xtrain)
headm(Xtest)
ntrain <- nrow(Xtrain)
ntest <- nrow(Xtest)
ntot <- ntrain + ntest
c(ntot = ntot, ntrain = ntrain, ntest = ntest)

## Cal, Val
nval <- 300   
nval
s <- sample(1:ntrain, nval, replace = TRUE)
#s <- sampcla(rep(1, ntrain), ytrain, m = nval)$test
s
Xcal <- Xtrain[-s, ]
ycal <- ytrain[-s]
Xval <- Xtrain[s, ]
yval <- ytrain[s]
headm(Xcal)
headm(Xval)
ncal <- ntrain - nval
c(ntot = ntot, ntrain = ntrain, ntest = ntest, 
    ncal = ncal, nval = nval)

nlv <- 0:60
res <- gridscorelv(Xcal, ycal, Xval, yval, 
    score = rmsep, fun = plskern,
    nlv = nlv, verb = TRUE)
headm(res)
plotscore(res$nlv, res$y1, lwd = 2,
    xlab = "Nb. LVs", ylab = "RMSEP_VAL")
u <- res[res$y1 == min(res$y1), ][1, , drop = FALSE]
u

fm <- plskern(Xtrain, ytrain, nlv = u$nlv)
pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)
plot(pred, ytest,
     pch = 16, col = "red",
     xlab = "Predicted", ylab = "Observed")
abline(0, 1, col = "grey", lwd = 2)


