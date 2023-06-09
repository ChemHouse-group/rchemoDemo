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

K <- 3
segm <- segmkf(ntrain, K = K, nrep = 1)
segm
#pct <- .30
#m <- round(pct * ntrain)
#segm = segmts(ntrain, m = m, nrep = 3)
segm

gamma <- 10^(-5:1)
nlv <- 1:40
pars <- mpars(gamma = gamma)
rescv <- gridcvlv(Xtrain, ytrain, segm, 
    score = err, fun = kplsrda,
    pars = pars, nlv = nlv, verb = TRUE)
names(rescv)
res <- rescv$val
group <- res$gamma
plotscore(res$nlv, res$y1, group, lwd = 2,
    xlab = "Nb. LVs", ylab = "ERR_CV")
u <- res[res$y1 == min(res$y1), ][1, , drop = FALSE]
u

fm <- kplsrda(Xtrain, ytrain, 
        gamma = u$gamma, nlv = u$nlv)
pred <- predict(fm, Xtest)$pred
err(pred, ytest)

