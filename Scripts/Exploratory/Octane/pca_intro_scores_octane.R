library(rchemo)

path <- "D:/Mes Donnees/Users/Applications/Nirs/Packages/ChemHouse/rchemoDemo/"
db <- paste(path, "Data/octane.rda", sep = "")
db
load(db)
names(octane)

## Six of the samples of the dataset contain 
## added alcohol  (= 25, 26, and 36-39)

X <- octane$X
headm(X)
n <- nrow(X)

plotsp(X,
    col = sample(1:n), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Absorbance")
  
## Model fitting
nlv <- 6
fm <- pcasvd(X, nlv = nlv) 
#fm <- pcasph(X, nlv = nlv)    # Robust PCA 
names(fm)

T <- fm$T
headm(T)

P <- fm$P
headm(P)

fm$eig

## 2-D Score space 
plotxy(T[, 1:2], zeroes = TRUE,
    xlab = "PC1", ylab = "PC2")

i <- 1
plotxy(T[, i:(i + 1)], col = "red3",
    xlab = paste("PC", i), ylab = paste("PC", i + 1),
    zeroes = TRUE, pch = 16, cex = 1.5)

## Loadings
plotsp(t(P),
    xlab = "Wawelength (nm)", ylab = "Absorbance")

plotsp1(t(P),
       xlab = "Wawelength (nm)", ylab = "Absorbance")

