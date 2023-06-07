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

## Preprocesssing
Xp <- savgol(snv(X), n = 21, p = 3, m = 2) 

## Splitting: Tot = Train + Test
## The PCA is fitted on Train, and Test will be 
## the supplementary observations.
## Here the splitting is provided by the dataset
## (= variable "typ"), but data Tot could be splitted 
## a posteriori (e.g. random sampling, systematic 
## sampling, etc.) 
s <- which(test == 1)
Xtrain <- Xp[-s, ]
Ytrain <- Y[-s, ]
Xtest <- Xp[s, ]
Ytest <- Y[s, ]
headm(Xtrain)
headm(Xtest)
ntrain <- nrow(Xtrain)
ntest <- nrow(Xtest)
ntot <- ntrain + ntest
c(ntot = ntot, ntrain = ntrain, ntest = ntest)

## Model fitting on Train
nlv <- 15
fm <- pcasvd(Xtrain, nlv = nlv) 
res <- summary(fm, Xtrain)$explvar
plotscore(res$pc, res$pvar, lwd = 2,
    xlab = "PC", 
    ylab = "Prop. variance explained")

Ttrain <- fm$T
headm(Ttrain)

## Projection of Test in the Train score space
Ttest <- transform(fm, Xtest)
headm(Ttest)

T <- rbind(Ttrain, Ttest)
group <- c(rep("0-Train", ntrain), 
    rep("1-Test", ntest))
i <- 1
plotxy(T[, i:(i + 1)], group = group,
    xlab = paste("PC", i), ylab = paste("PC", i + 1),
    zeroes = TRUE, pch = 16)

## SD and OD
res <- scordis(fm, Xtest)   # SD
names(res)
headm(res$res.train)
headm(res$res)
sdtrain <- res$res.train$dstand
sdtest <- res$res$dstand

res <- odis(fm, Xtrain, Xtest)
names(res)
headm(res$res.train)
headm(res$res)
odtrain <- res$res.train$dstand
odtest <- res$res$dstand

sd <- c(sdtrain, sdtest)
od <- c(odtrain, odtest)
z <- matrix(c(sd, od), ncol = 2)
plotxy(z, group = test, pch = 16,
    xlab = "Standardized SD", ylab = "Standardized OD")
abline(h = 1, v = 1, col = "grey")

