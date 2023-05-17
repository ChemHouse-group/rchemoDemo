library(rchemo)
library(readxl)

path <- "D:/Mes Donnees/Users/Applications/Nirs/Packages/ChemHouse/rchemoDemo/"

db <- paste(path, "Data/datspir.xlsx", sep = "")
db
z <- read_xlsx(path = db, sheet = "X",
    col_names = TRUE)
headm(z)
z <- data.frame(z)
headm(z)
z <- z[, -1]
names(z) <- substr(names(z), 2, 10)
X <- z
headm(X)
plotsp(X)

z <- read_xlsx(path = db, sheet = "Y",
    col_names = TRUE)
headm(z)
z <- data.frame(z)
z <- z[, -1]
headm(z)
names(z) <- tolower(names(z))    # to make lowercase for all dataset 
z[, 1:3][z[, 1:3] == 0] <- NA    # "y = 0" = missing data
Y <- z
headm(Y)

z <- read_xlsx(path = db, sheet = "M",
               col_names = TRUE, na = "")
headm(z)
z <- data.frame(z)
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

## To export in CSV
#db <- paste(path, "Tmp_res/X_new.csv", sep = "")
#db
#write.table(X, file = db, sep = ";",
#    row.names = FALSE, col.names = TRUE)
