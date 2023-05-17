## NIRS data on cassava paths (2009-2013; South-America). FOSS NiRSystem Instruments 
## 400-2498 nm (step = 2 nm). This is an extract of the dataset used in Lesnoff et al. 
## 2020. Response variable: TBC concentration (beta-carotene pigment).
## Source: Harvest Plus Challenge Program, ICRAF, Columbia.
## References:
## - Davrieux, F., Dufour, D., Dardenne, P., Belalcazar, J., Pizarro, M., Luna, J., Londoño, 
## L., Jaramillo, A., Sanchez, T., Morante, N., Calle, F., Becerra Lopez-Lavalle, L., 
## Ceballos, H., 2016. LOCAL regression algorithm improves near infrared spectroscopy 
## predictions when the target constituent evolves in breeding populations. 
## Journal of Near Infrared Spectroscopy 24, 109. https://doi.org/10.1255/jnirs.1213
## - Lesnoff, M., Metz, M., Roger, J.-M., 2020. Comparison of locally weighted PLS 
## strategies for regression and discrimination on agronomic NIR data. 
## Journal of Chemometrics n/a, e3209. https://doi.org/10.1002/cem.3209

library(rchemo)

path <- "D:/Mes Donnees/Users/Applications/Nirs/Packages/ChemHouse/rchemoDemo/"
db <- paste(path, "Data/cassav.rda", sep = "")
db
load(db)
names(dat)
X <- dat$X
Y <- dat$Y
headm(X)
headm(Y)
y <- Y$tbc
year <- Y$year
n <- nrow(X)

table(year)

#### Spectra
plotsp(X,
    col = sample(1:n), lwd = 2,
    xlab = "Wawelength (nm)", ylab = "Absorbance")

Xp <- savgol(snv(X), n = 21, p = 3, m = 2) 

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
plotxy(T[, i:(i + 1)], group = year,
    xlab = paste("PC", i), ylab = paste("PC", i + 1),
    zeroes = TRUE, pch = 16)

#### Variables y
summary(Y$tbc)

hist(Y$tbc, n = 30,
    main = "", xlab = "TBC")

boxplot(tbc ~ year, data = Y,
    boxwex = .3, notch = TRUE,
    xlab = "Year", ylab = "TBC")

