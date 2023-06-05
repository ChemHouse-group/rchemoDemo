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

## Cal, Val
nval <- 100
## Or:
#pct <- .30 ; nval <- round(pct * ntrain)   
## Different choices to select Val
## (1) Random sampling
s <- sample(1:ntrain, nval, replace = FALSE)
s
## (2) Kennard-Stone sampling
## Output 'train' contains higher variability
## than output 'test'
res <- sampks(Xtrain, k = nval)
s <- res$train   # = the most variable data
s
## (3) Duplex sampling
res <- sampdp(Xtrain, k = nval)
s <- res$train
s
## (4) Systematic sampling over y
res <- sampcla(ytrain, m = 25)
s <- res$test
s
table(ytrain[s])
## Selection
Xcal <- Xtrain[-s, ]
ycal <- ytrain[-s]
Xval <- Xtrain[s, ]
yval <- ytrain[s]
headm(Xcal)
headm(Xval)
ncal <- ntrain - nval
c(ntot = ntot, ntrain = ntrain, ntest = ntest, 
    ncal = ncal, nval = nval)

gamma <- 10^(-5:1)
pars <- mpars(gamma = gamma)
nlv <- 1:40
res <- gridscorelv(Xcal, ycal, Xval, yval, 
    score = err, fun = kplsrda,
    pars = pars, nlv = nlv, verb = TRUE)
group <- res$gamma
plotscore(res$nlv, res$y1, group, lwd = 2,
    xlab = "Nb. LVs", ylab = "ERR_CV")
u <- res[res$y1 == min(res$y1), ][1, , drop = FALSE]
u

fm <- kplsrda(Xtrain, ytrain, 
    gamma = u$gamma, nlv = u$nlv)
pred <- predict(fm, Xtest)$pred
err(pred, ytest)


