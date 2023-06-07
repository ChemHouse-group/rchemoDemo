library(rchemo)

## See ?gridscore
## Functions gridscore... have a similar syntax as functions gridcv...
## gridscore   : generic function that can be run on all functions
## gridscorelv : specific to (and much faster for) models with latent variables (e.g PLSR)
## gridscorelb : specific to (and much faster for) models with ridge regulaization (e.g RR)

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

## Build the splitting Train = Cal + Val
## The model will be fitted on Cal and 
## optimized on Val
nval <- 30
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
names(res)
s <- res$train   # = the most variable data
s
## (3) Duplex sampling
res <- sampdp(Xtrain, k = nval)
s <- res$train
s
## (4) Systematic sampling over y
res <- sampcla(rep(1, ntrain), ytrain, m = nval)
s <- res$test
s
## Selection
Xcal <- Xtrain[-s, ]
Ycal <- Ytrain[-s, ]
Xval <- Xtrain[s, ]
Yval <- Ytrain[s, ]
headm(Xcal)
headm(Xval)
ncal <- ntrain - nval
c(ntot = ntot, ntrain = ntrain, ntest = ntest, 
    ncal = ncal, nval = nval)

j <- 2  # y-variable
nam <- namy[j]
nam 
ytrain <- Ytrain[, nam]
ytest <- Ytest[, nam]
ycal <- Ycal[, nam]
yval <- Yval[, nam]

nlv <- 0:20
res <- gridscorelv(Xcal, ycal, Xval, yval, 
    score = rmsep, fun = plskern,
    nlv = nlv, verb = TRUE)
headm(res)
plotscore(res$nlv, res$y1, lwd = 2,
    xlab = "Nb. LVs", ylab = "RMSEP_VAL")
## Best model
u <- res[res$y1 == min(res$y1), ][1, , drop = FALSE]
u

## Final predictions
fm <- plskern(Xtrain, ytrain, nlv = u$nlv)
pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)
plot(pred, ytest,
     pch = 16, col = "red",
     xlab = "Prediction", ylab = "Observed")
abline(0, 1, col = "grey", lwd = 2)

## Parcimony approach
res_sel <- selwold(res$y1, res$nlv, smooth = FALSE, 
    alpha = .05, plot = TRUE)
names(res_sel)
res_sel$opt     # Nb. LVs correponding to the minimal error rate
res_sel$sel     # Nb LVs selected with the Wold's criterion
fm = plskern(Xtrain, ytrain, nlv = res_sel$sel) 
pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)

## !!! Remark
## Function "gridscore" is generic for all the functions.
## Here, it could be used instead of "gridscorelv" 
## but this is not time-efficient for LV-based methods.
## Commands below return the same results as 
nlv <- 1:20
pars <- mpars(nlv = nlv)
pars
res <- gridscore(Xcal, ycal, Xval, yval, 
    score = rmsep, fun = plskern,
    pars = pars, verb = TRUE)
res
plotscore(res$nlv, res$y1, lwd = 2,
    xlab = "Nb. LVs", ylab = "RMSEP_VAL")
u <- res[res$y1 == min(res$y1), ][1, , drop = FALSE]
u
fm <- plskern(Xtrain, ytrain, nlv = u$nlv)
pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)



