
## ---- libraries ----
library("emi")
library("plyr")
library("dplyr")
library("reshape2")
library("ggplot2")
library("Matrix")
data(users)
data(words)
data(test)
data(train)

## ---- filter-data ----
users <- users[1:10000, ]
words <- words[words$User %in% users$RESPID, ]

## ---- separate-types ----
unum <- get_numeric(users)
wnum <- get_numeric(words)

## --- naive-svd
got_rating <- xtabs(~ User + Artist, get_keys(words), sparse = T)
dim(got_rating)
image(got_rating, aspect = 0.1)
hist(colSums(got_rating), main = "for each artist, how many users rated them?", breaks = 10)

# considering just word 1
wnum[is.na(wnum)] <- -10

ua_num <- dcast(cbind(get_keys(words), wnum[, 5, with = F]), User ~ Artist)[, -1]
head(ua_num)


image(as.matrix(dcast(cbind(get_keys(words), wnum[, 6, with = F]), User ~ Artist))[, -1])
