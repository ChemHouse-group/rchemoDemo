library(rchemo)

path <- "D:/Mes Donnees/Users/Applications/Nirs/Packages/ChemHouse/rchemoDemo/"
db <- paste(path, "Data/cassav.rda", sep = "")
db
load(db)
names(dat)
X <- dat$X 
Y <- dat$Y
n <- nrow(X)
headm(X)
headm(Y)
y <- Y$tbc    
year <- Y$year
table(year)

Xp <- detrend(snv(X), degree = 2)
#Xp <- savgol(snv(X), m = 2, n = 21, p = 3)
headm(Xp)

zX <- X
#zX <- Xp

plotsp(zX,
    col = sample(1:n), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Absorbance")

nlv <- 6
fm <- pcasvd(zX, nlv = nlv) 
T <- fm$T

i <- 1
plotxy(T[, i:(i + 1)], col = "red",
    xlab = paste("PC", i), ylab = paste("PC", i + 1),
    zeroes = TRUE, pch = 16)

i <- 1
plotxy(T[, i:(i + 1)], group = year,
    xlab = paste("PC", i), ylab = paste("PC", i + 1),
    zeroes = TRUE, pch = 16)

i <- 1
plotxy(T[, i:(i + 1)], group = year, ellipse = TRUE,
    xlab = paste("PC", i), ylab = paste("PC", i + 1),
    zeroes = TRUE, pch = 16)

