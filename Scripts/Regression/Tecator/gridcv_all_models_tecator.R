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

segm <- segmkf(n = ntrain, K = 3, nrep = 10) # replicated K-fold
#segm <- segmts(n = nttrain, m = round(.33 * ntrain), nrep = 30) # replicated "test-set"
segm

#### PLSR
nlv <- 0:20
res <- gridcvlv(Xtrain, ytrain, segm, 
    score = rmsep, fun = plskern,
    nlv = nlv, verb = TRUE)$val
names(res)
u <- res[res$y1 == min(res$y1), ][1, , drop = FALSE]
u
plotscore(res$nlv, res$y1,
    xlab = "Nb. LVs", ylab = "RMSEP_CV")
fm <- plskern(Xtrain, ytrain, nlv = u$nlv)
pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)
plot(pred, ytest,
    pch = 16, col = "red",
    xlab = "Predicted", ylab = "Observed")
abline(0, 1, col = "grey", lwd = 2)

#### RR
lb <- 10^(-6:-1) 
res <- gridcvlb(Xtrain, ytrain, segm = segm, 
    score = rmsep, fun = rr,
    lb = lb, verb = TRUE)$val
u <- res[res$y1 == min(res$y1), ][1, , drop = FALSE]
u
fm <- rr(Xtrain, ytrain, lb = u$lb)
pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)

#### KRR
gamma <- 10^(-3:3)
lb <- 10^(-6:-1) 
pars <- mpars(gamma = gamma)
pars
length(pars[[1]])
res <- gridcvlb(Xtrain, ytrain, segm = segm, 
    score = rmsep, fun = krr,
    lb = lb, pars = pars, verb = TRUE)$val
u <- res[res$y1 == min(res$y1), ][1, , drop = FALSE]
u
fm <- krr(Xtrain, ytrain, gamma = u$gamma, lb = u$lb)
pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)

#### LWPLSR
zsegm <- segm[1:3]   # to save time

nlvdis <- c(5, 10, 15)
diss <- "mahal"
h <- c(1, 2, 5)
k <- c(30, 40, 50, 100)
nlv <- 0:20
pars <- mpars(nlvdis = nlvdis, diss = diss,
    h = h, k = k)
pars
length(pars[[1]])
res <- gridcvlv(Xtrain, ytrain, segm = zsegm,
    score = rmsep, fun = lwplsr, 
    nlv = nlv, pars = pars, verb = TRUE)$val
u <- res[res$y1 == min(res$y1), ][1, , drop = FALSE]
u
group <- paste("nlvdis=", res$nlvdis, " h=", res$h, " k=", res$k, sep = "")
plotscore(res$nlv, res$y1, group = group,
    xlab = "Nb. LVs", ylab = "RMSEP_CV", legend = FALSE)
fm <- lwplsr(Xtrain, ytrain, 
    nlvdis = u$nlvdis, diss = u$diss, 
    h = u$h, k = u$k, nlv = u$nlv, verb = TRUE)
pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)

#### LWPLSR_AGG
zsegm <- segm[1:3]   # to save time

nlvdis <- c(5, 10, 15)
diss <- "mahal"
h <- c(1, 2, 5)
k <- c(30, 40, 50, 100)
nlv <- c("0:10", "5:10", "0:15")
pars <- mpars(nlvdis = nlvdis, diss = diss,
              h = h, k = k, nlv = nlv)
pars
length(pars[[1]])
res <- gridcv(Xtrain, ytrain, segm = zsegm,
    score = rmsep, fun = lwplsr_agg, 
    pars = pars, verb = TRUE)$val
u <- res[res$y1 == min(res$y1), ][1, , drop = FALSE]
u
fm <- lwplsr_agg(Xtrain, ytrain, 
    nlvdis = u$nlvdis, diss = u$diss, 
    h = u$h, k = u$k, nlv = u$nlv, verb = TRUE)
pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)

#### KNNR
nlvdis <- c(5, 10, 15)
diss <- "mahal"
h <- c(1, 2, 5)
k <- c(5, 10, 20, 30)
pars <- mpars(nlvdis = nlvdis, diss = diss,
    h = h, k = k)
pars
length(pars[[1]])
res <- gridcv(Xtrain, ytrain, segm = segm,
    score = rmsep, fun = knnr, 
    pars = pars, verb = TRUE)$val
u <- res[res$y1 == min(res$y1), ][1, , drop = FALSE]
u
fm <- knnr(Xtrain, ytrain, 
    nlvdis = u$nlvdis, diss = u$diss, 
    h = u$h, k = u$k)
pred <- predict(fm, Xtest)$pred
rmsep(pred, ytest)



