
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

## Second go at deriving head words

df2$occSplit <- strsplit(df2$standard, " ") # seperate words

# remove add words
df2$occSplit <- lapply(df2$occSplit, function(x) {
    badvec <- c("de", "het", "een", "van", "bij", "uit", "voor", "niet",
                "eerste", "tweede", "derde", "vierde", "vijfde", 
                "beroepstitel", "vermeld", "interpreteerbaar", "overgenomen")
    sapply(x, function(y) {
        ifelse(y %in% badvec, y <- NA, y)
    })
})

# get the first word
df2$head <- sapply(df2$occSplit, "[[", 1)


# removing any spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
df2$head <- trim(df2$head)



## Writing out to file
names(df2)
df3 <- df2[, c(7,1,3,5)]
# View(df3)

write.csv(x = df3, "./data/derived/hsn_headword.csv", row.names = FALSE)

# EOF

