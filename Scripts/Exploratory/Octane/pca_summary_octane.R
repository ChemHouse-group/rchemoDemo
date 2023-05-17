library(rchemo)

path <- "D:/Mes Donnees/Users/Applications/Nirs/Packages/ChemHouse/rchemoDemo/"
db <- paste(path, "Data/octane.rda", sep = "")
db
load(db)
names(octane)

X <- octane$X
headm(X)
n <- nrow(X)
p <- ncol(X)

nlv <- 6
fm <- pcasvd(X, nlv = nlv) 
#fm <- pcasph(X, nlv = nlv) 

barplot(fm$eig[1:nlv], space = 1, names.arg = 1:nlv, 
        xlab = "PCs", ylab = "Eigenvalue")

## Summary
res <- summary(fm, X)
names(res)

## Expliande variance
res$explvar

## Correlation circle
plotxy(res$cor.circle, zeroes = TRUE, asp = 1,
    label = FALSE, cex = 2, col = "red3",
    circle = TRUE, xlim = c(-1, 1), ylim = c(-1, 1))

## Individual and variable contributions

headm(res$contr.ind)     # influential: > 1 / n
headm(res$contr.var)     # influential: > 1 / p

colSums(res$contr.ind)
colSums(res$contr.var)

