library(rchemo)

data(iris)

X <- iris[, 1:4]
y <- iris[, 5]
table(y)

nlv <- 2
fm <- fda(X, y, nlv = nlv)
#fm <- fdasvd(X, y, nlv = nlv)
names(fm)
headm(fm$T)

## Tcenters = projection of the class centers in the score space
fm$Tcenters

plotxy(fm$T, group = y, ellipse = TRUE, 
    zeroes = TRUE, pch = 16, cex = 1.5, ncol = 2)
points(fm$Tcenters, pch = 8, col = "blue", cex = 1.5)

## X-loadings matrix
## = coefficients of the linear discriminant function
## = "LD" of function lda of package MASS
fm$P

## Projections of samples
transform(fm, X[1:3, ])

## Explained variance by PCA of the class centers 
## in transformed scale
summary(fm)


