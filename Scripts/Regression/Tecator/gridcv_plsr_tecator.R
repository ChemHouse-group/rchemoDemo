library(rchemo)

## See ?gridcv
## gridcv   : generic function that can be run on all functions
## gridcvlv : specific to (and much faster for) models with latent variables (e.g PLSR)
## gridcvlb : specific to (and much faster for) models with ridge regulaization (e.g RR)

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

## The CV is done within Train, and
## the generalization error is estimated on Test
## Different choices of building the segments 
## within Train
## (1) Replicated K-fold CV
K <- 3
segm <- segmkf(ntrain, K = K, nrep = 10)
segm
## (2) Or replicated "Test-set" CV 
## ==> splitting Train = Cal + Val
## e.g. Val = 30% of traing (Cal = 70%)
pct <- .30
m <- round(pct * ntrain)
segm = segmts(ntrain, m = m, nrep = 30)
segm

i <- 1  # segment within a replication
k <- 1  # replication
segm[i]
segm[i][k]

nlv <- 0:20
rescv <- gridcvlv(Xtrain, ytrain, segm, 
    score = rmsep, fun = plskern,
    nlv = nlv, verb = TRUE)
names(rescv)
res <- rescv$val # Average results
plotscore(res$nlv, res$y1, lwd = 2,
    xlab = "Nb. LVs", ylab = "RMSEP_CV")
## Best model
u <- res[res$y1 == min(res$y1), ][1, , drop = FALSE]
u

## Final predictions
fm <- plskern(Xtrain, ytrain, nlv = u$nlv)
pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)
plot(pred, ytest,
     pch = 16, col = "red",
     xlab = "Predicted", ylab = "Observed")
abline(0, 1, col = "grey", lwd = 2)

## Variability of the performance 
## between folds and replications
res <- rescv$val_rep 
headm(res)
group <- paste(res$segm, "-", res$rep, sep = "")
plotscore(res$nlv, res$y1, group, steplab = 2,
    legend = FALSE, main = "CV Folds",
    xlab = "Nb. LVs", ylab = "RMSEP")

## Parcimony approach
res <- rescv$val
res_sel <- selwold(res$y1, res$nlv, smooth = FALSE, 
    alpha = .05, plot = TRUE)
names(res_sel)
res_sel$opt     # Nb. LVs correponding to the minimal error rate
res_sel$sel     # Nb LVs selected with the Wold's criterion
fm = plskern(Xtrain, ytrain, nlv = res_sel$sel) 
pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)

## !!! Remark
## Function "gridcv" is generic for all the functions.
## Here, it could be used instead of "gridcvlv" 
## but this is not time-efficient for LV-based methods.
## Commands below return the same results as 
nlv <- 1:20
pars <- mpars(nlv = nlv)
pars
res <- gridcv(Xtrain, ytrain, segm, 
    score = rmsep, fun = plskern,
    pars = pars, verb = TRUE)$val
res
plotscore(res$nlv, res$y1, lwd = 2,
    xlab = "Nb. LVs", ylab = "RMSEP_CV")
u <- res[res$y1 == min(res$y1), ][1, , drop = FALSE]
u
fm <- plskern(Xtrain, ytrain, nlv = u$nlv)
pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)


