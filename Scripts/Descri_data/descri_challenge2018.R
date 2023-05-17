## NIRS data (protein content of forages and feed) used in the challenge of the 
## congress Chemometrics2018 (Paris, January 2018, https://chemom2018.sciencesconf.org/). 
## The original data contain errors (duplicates). The data provided have been corrected 
## (duplicates have been removed), and documented with new descriptors 
## (type of vegetal materials).

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

#### Spectra
m <- 50
plotsp(X[sample(1:n, m), ],
    col = sample(1:n, m), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Reflectance")

Xp <- savgol(snv(X), n = 21, p = 3, m = 2) 

m <- 50
plotsp(Xp[sample(1:n, m), ],
    col = sample(1:n, m), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Reflectance")

#### PCA
nlv <- 10
fm <- pcasvd(Xp, nlv = nlv) 
T <- fm$T

barplot(fm$eig[1:nlv], space = 1, names.arg = 1:nlv, 
    xlab = "PCs", ylab = "Eigenvalue")

res <- summary(fm, Xp)
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
summary(y)

hist(y, n = 50,
    main = "", xlab = "Protein")

boxplot(y ~ typ, data = Y,
    boxwex = .3, notch = TRUE,
    xlab = "Class", ylab = "Protein")

boxplot(y ~ test, data = Y,
    boxwex = .3, notch = TRUE,
    xlab = "Type of sample", ylab = "Protein")

