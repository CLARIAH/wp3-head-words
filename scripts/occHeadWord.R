
# clear workspace
rm(list = ls())

# load libraries (use install.packages('package') first, if not installed yet)
library(dplyr)
library(tidyr)
library(stringr)

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
    badvec <- c("de", "het", "een", "van", "bij", "uit", "voor", "niet", "op",
                "eerste", "tweede", "derde", "vierde", "vijfde", 
                "beroepstitel", "vermeld", "interpreteerbaar", "overgenomen",
                "adjunct", "secretarie", "machine", "oost-indisch",
                "beroepen", "gepensioneerd", "onbezoldigd", "oud", "semi",
                "tijdelijk", "zonder", "plaatsvervangend", "gegageerd",
                "los", "leerling", "privé", "privaat", "technisch", 
                "provinciaal", "provincie")
    sapply(x, function(y) {
        ifelse(y %in% badvec, y <- NA, y)
    })
})

View(df2)

### MORE ISSUES TO SOLVE
# - delete if first word is "bedrijfstitel:"
comp <- function(x) {
    first <- sapply(x, "[[", 1)[1]
    sapply(x, function(y) {
        ifelse(first == "bedrijfstitel:",
               y <- NA, y) 
    })
    
}
df2$occSplit2 <- sapply(df2$occSplit, comp)

df2[df2$standard %in% "bedrijfstitel", ]



# - eigen werk doende
own_work <- function(x) {
    first <- sapply(x, "[[", 1)[1]
    second <- sapply(x, "[[", 1)[2]
    third <- sapply(x, "[[", 1)[3]
    ifelse(first == "eigen" & second == "werk" & third == "doende" ,
           x <- NA, x)
}
df2$occSplit <- sapply(df2$occSplit, own_work)


# - sergeant majoor (should be one word)
sgt_mjr <- function(x) {
    first <- sapply(x, "[[", 1)[1]
    second <- sapply(x, "[[", 1)[2]
    ifelse(first == "sergeant" & second == "majoor",
     "sergeant majoor", x)
}
df2$occSplit <- sapply(df2$occSplit, sgt_mjr)

# - if the first word contains a '-', use last word
#dash <- function(x) {
#    first <- sapply(x, "[[", 1)[1]
#    last <- word(x, -1)
#    ifelse(grepl("-", first), last, x)
#}
#df2$occSplit <- sapply(df2$standard, dash) #NB: now using standard, bc


# - if more than 1 word and first word == 'meester' drop first word
#
#master <- function(x) {
#    first <- sapply(x, "[[", 1)[1]
#    count <- sapply(gregexpr("\\W+", x), length) + 1
#    ifelse(count > 1  & first == "meester", 
#           sapply(x, "[[", 1)[1] <- NA, x)
#}
#
#df2$occSplit2 <- sapply(df2$standard, master) #NB: now using standard, bc




# get the head word (first if not missing, else second)
select_head <- function(x) {
        first <- sapply(x, "[[", 1)[1]
        second <- sapply(x, "[[", 1)[2]
        ifelse(is.na(first), second, first)
}
df2$head <- sapply(df2$occSplit, select_head)


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

