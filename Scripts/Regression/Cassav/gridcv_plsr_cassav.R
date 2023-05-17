library(rchemo)

path <- "D:/Mes Donnees/Users/Applications/Nirs/Packages/ChemHouse/rchemoDemo/"
db <- paste(path, "Data/cassav.rda", sep = "")
db
load(db)
names(dat)
X <- dat$X
Y <- dat$Y
headm(X)
headm(Y)
y <- Y$tbc
year <- Y$year
n <- nrow(X)

table(year)

plotsp(X,
    col = sample(1:n), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Absorbance")

n <- 15 ; p <- 3 ; m <- 2
Xp <- savgol(snv(X), n = n, p = p, m = m) 

plotsp(Xp,
    col = sample(1:n), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Absorbance")

## Train, Test
s <- which(year == 2013)
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

K <- 3
segm <- segmkf(ntrain, K = K, nrep = 10)
segm
#pct <- .30
#m <- round(pct * ntrain)
#segm = segmts(ntrain, m = m, nrep = 30)
segm

nlv <- 0:40
rescv <- gridcvlv(Xtrain, ytrain, segm, 
    score = rmsep, fun = plskern,
    nlv = nlv, verb = TRUE)
names(rescv)
res <- rescv$val
plotscore(res$nlv, res$y1, lwd = 2,
    xlab = "Nb. LVs", ylab = "RMSEP_CV")
u <- res[res$y1 == min(res$y1), ][1, , drop = FALSE]
u

fm <- plskern(Xtrain, ytrain, nlv = u$nlv)
pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)
plot(pred, ytest,
     pch = 16, col = "red",
     xlab = "Predicted", ylab = "Observed")
abline(0, 1, col = "grey", lwd = 2)

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


