library(rchemo)

path <- "D:/Mes Donnees/Users/Applications/Nirs/Packages/ChemHouse/rchemoDemo/"

db <- paste(path, "Data/datspir_X.csv", sep = "")
db
z <- read.table(db, header = TRUE, dec = ",", sep = ";")
headm(z)
z <- z[, -1]
names(z) <- substr(names(z), 2, 10)
X <- z
headm(X)
plotsp(X)

db <- paste(path, "Data/datspir_Y.csv", sep = "")
z <- read.table(db, header = TRUE, dec = ",", sep = ";")
headm(z)
z <- z[, -1]
headm(z)
names(z) <- tolower(names(z))    # to make lowercase for all dataset 
z[, 1:3][z[, 1:3] == 0] <- NA    # "y = 0" = missing data
Y <- z
headm(Y)

db <- paste(path, "Data/datspir_M.csv", sep = "")
z <- read.table(db, header = TRUE, dec = ",", sep = ";")
z[z == ""] <- NA
names(z) <- tolower(names(z))
M <- z
headm(M)

dat <- list(X = X, Y = Y, M = M)

## Export to .rda
db <- paste(path, "Tmp_res/datspir.rda", sep = "")
db
save(dat, file = db, compress = "xz")

## To re-import the .rda (load object dat)
#load(db) 

## To re-export in CSV
#db <- paste(path, "Tmp_res/X_new.csv", sep = "")
#db
#write.table(X, file = db, sep = ";",
#    row.names = FALSE, col.names = TRUE)

############## Some checking

## Occurrences of NA
z <- X
#z <- Y
#z <- M
res <- checkna(z)
res[res$nbna > 0, ]

## Duplicates
checkdupl(M$id)
checkdupl(X[, seq(1, ncol(X), 50)])
checkdupl(Y)

