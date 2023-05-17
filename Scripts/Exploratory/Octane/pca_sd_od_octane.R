library(rchemo)

path <- "D:/Mes Donnees/Users/Applications/Nirs/Packages/ChemHouse/rchemoDemo/"
db <- paste(path, "Data/octane.rda", sep = "")
db
load(db)
names(octane)

X <- octane$X
headm(X)
n <- nrow(X)

nlv <- 3
fm <- pcasvd(X, nlv = nlv) 
#fm <- pcasph(X, nlv = nlv) 

## Score (SD) and orthogonal (OD) distances

res <- scordis(fm)       # SD
#res <- scordis(fm, nlv = 2, alpha = .05)       
#res <- odis(fm, X)      # OD
names(res)
dtrain <- res$res.train
headm(dtrain)
res$cutoff

d <- dtrain$dstand
hist(d, n = 30,
    xlab = "Standardized distance", 
    ylab = "Nb. observations", 
    main = "")
abline(v = 1, col = "red")

plot(1:n, d,
    xlab = "Observation index", 
    ylab = "Standardized distance",
    pch = 16,
    col = "grey40",
    xaxt = "n"
    )
s <- seq(1, n, by = 2)
axis(side = 1, at = s, labels = s)
abline(h = 1, lty = 2, col = "red")

## SD-OD
res <- scordis(fm)
d_sd <- res$res.train$dstand
res <- odis(fm, X)
d_od <- res$res.train$dstand
plot(d_sd, d_od,
    xlab = "Standardized SD", 
    ylab = "Standardized OD",
    pch = 16, col = "red")
abline(h = 1, v = 1, col = "grey")

z <- matrix(c(d_sd, d_od), ncol = 2)
plotxy(z,
    xlab = "Standardized SD", 
    ylab = "Standardized OD",
    pch = 16, col = "red",
    labels = TRUE
    )
abline(h = 1, v = 1, col = "grey")



