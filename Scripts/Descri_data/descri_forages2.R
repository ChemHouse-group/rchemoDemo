## NIRS data on dried and grounded mixed forages (n = 485): stems, leaves etc. 
## Origin: mainly tropical African areas. FOSS NiRSystem Instruments 1100-2498 nm 
## (step = 2 nm). Data being private, spectra have been preprocessed with Savitzky-Golay 
## (d = 2). Response variables:
## - DM: dry matter content
## - NDF: fibers content
## - typ: Type of forage
## Source: CIRAD, Selmet research unit (https://umr-selmet.cirad.fr/en)

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
typ <- Y$typ
test <- Y$test
namy <- names(Y)[1:2]
namy
n <- nrow(X)

table(typ, test)

#### Spectra
## X: already pre-processed (snv + savgol d=2)
plotsp(X,
    col = sample(1:n), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Reflectance")

#### PCA
nlv <- 10
fm <- pcasvd(X, nlv = nlv) 
T <- fm$T

barplot(fm$eig[1:nlv], space = 1, names.arg = 1:nlv, 
    xlab = "PCs", ylab = "Eigenvalue")

res <- summary(fm, X)
names(res)
res$explvar

i <- 1
plotxy(T[, i:(i + 1)], group = typ,
    xlab = paste("PC", i), ylab = paste("PC", i + 1),
    zeroes = TRUE, pch = 16)

i <- 1
plotxy(T[, i:(i + 1)], group = test,
    xlab = paste("PC", i), ylab = paste("PC", i + 1),
    zeroes = TRUE, pch = 16)

#### Variables y

summary(Y[, namy])

j <- 1 # y-variable
nam <- namy[j]
hist(Y[, nam], n = 100,
    main = "", xlab = nam)

boxplot(dm ~ typ, data = Y,
    boxwex = .3, notch = TRUE,
    xlab = "Class", ylab = nam)

boxplot(dm ~ test, data = Y,
    boxwex = .3, notch = TRUE,
    xlab = "Type of sample", ylab = nam)

