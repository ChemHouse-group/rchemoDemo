library(rchemo)
library(readxl)

path <- "D:/Mes Donnees/Users/Applications/Nirs/Packages/ChemHouse/rchemoDemo/"

db <- paste(path, "Data/challenge2018.xlsx", sep = "")
db
z <- read_xlsx(path = db, sheet = "X",
    col_names = TRUE)
headm(z)
z <- data.frame(z)
headm(z)
names(z) <- substr(names(z), 2, 10)
X <- z
headm(X)
plotsp(X[1:50,])

z <- read_xlsx(path = db, sheet = "Y",
    col_names = TRUE)
headm(z)
z <- data.frame(z)
headm(z)
names(z) <- tolower(names(z))    # to make lowercase for all dataset 
z$conc[z$conc == 0] <- NA        # "y = 0" = missing data
Y <- z
headm(Y)

dat <- list(X = X, Y = Y)

## Export to .rda
db <- paste(path, "Tmp_res/challenge2018.rda", sep = "")
db
save(dat, file = db, compress = "xz")

## To re-import the .rda (load object dat)
#load(db) 

