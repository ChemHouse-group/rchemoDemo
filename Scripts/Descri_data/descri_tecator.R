## NIRS data recorded on a Tecator Infratec Food and Feed Analyzer working in the 
## wavelength range 850 - 1050 nm by the Near Infrared Transmission (NIT) principle. 
## Each sample contains finely chopped pure meat with different moisture, fat and protein 
## contents. For each meat sample the data consists of a 100 channel spectrum of 
## absorbances and the contents of moisture (water), fat and protein. The absorbance is 
## -log10 of the transmittance measured by the spectrometer. The three contents, measured 
## in percent, are determined by analytic chemistry.
## A full description is given at http://lib.stat.cmu.edu/datasets/tecator.
## Warning: The original X-data contains 22 duplicates, and the Y-data 15 replicates. 
## These replicates were removed in the present dataset.

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

#### Spectra
plotsp(X,
    col = sample(1:n), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Absorbance")

Xp <- savgol(snv(X), n = 15, p = 3, m = 2) 

plotsp(Xp,
    col = sample(1:n), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Absorbance")

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

#### Variables y

summary(Y[, namy])

j <- 2 # y-variable
nam <- namy[j]
nam
hist(Y[, nam], n = 30,
    main = "", xlab = nam)

boxplot(fat ~ typ, data = Y,
    boxwex = .3, notch = TRUE,
    xlab = "Type of sample", ylab = nam)

