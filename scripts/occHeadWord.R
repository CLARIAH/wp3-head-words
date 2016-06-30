
# clear workspace
rm(list = ls())


# load libraries (use install.packages('package') first, if not installed yet)
library(dplyr)
library(tidyr)

# setting working directory

setwd("~/git/wp3-head-words/")

# load data
df <- read.csv2("./data/source/Release_HSN_HISCO_2013_01a.csv",
                stringsAsFactors = FALSE,
                fileEncoding = "latin1")
# lower varnames
names(df) <- tolower(names(df))

# abstract relevant columns and in requested order
df2 <- df[, c(3, 2, 4, 5, 10)]


# Very first and simplistic go add finding occupational head words

# rule 1: remove add words

rmbad <- function(x) {
    badvec <- c(" de", " het", " een", " van", " bij", " uit", " voor", "niet ",
                "eerste ", "tweede ", "derde ", "vierde ", "vijfde ", 
                "beroepstitel ", " vermeld", "interpreteerbaar", "overgenomen")
    subvec <- paste0(badvec, collapse = "|")
    gsub(subvec, "", x)
}

df2$good <- sapply(df2$standard, rmbad)
#View(df2)

# rule 2: get the first word

fword <- function(x) {
    gsub(' [A-z ]*', '' , x)
}

df2$head <- sapply(df2$good, fword)

# removing any spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
df2$head <- trim(df2$head)


str(df2$head)

## Writing out to file
df3 <- df2[, c(7,1,3,5)]
# View(df3)

write.csv(x = df3, "./data/derived/hsn_headword.csv", row.names = FALSE)

# EOF

