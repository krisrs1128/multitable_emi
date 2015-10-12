
################################################################################
#
################################################################################

# ---- libraries ----
library("stringr")
library("caret")
library("emi")

data(train)
data(test)
data(users)
data(words)

train_orig <- train
test_orig <- test
users_orig <- users
words_orig <- words

# make factor columns
train$Artist <- as.factor(train$Artist)
train$Track <- as.factor(train$Track)
train$User <- as.factor(train$User)
train$Time <- as.factor(train$Time)

test$Artist <- as.factor(test$Artist)
test$Track <- as.factor(test$Track)
test$User <- as.factor(test$User)
test$Time <- as.factor(test$Time)

words$Artist <- as.factor(words$Artist)
words$User <- as.factor(words$User)

users$User <- as.factor(users$User)

# remove-redundant-labels ------------------------------------------------------
list_back <- users$LIST_BACK
list_back[list_back == ""] <- NA
list_back[list_back == "16+ hours"] <- 17
list_back[list_back == "More than 16 hours"] <- 17
list_back[list_back == "Less than an hour"] <- 0.5
list_back <- str_extract(list_back, "[[0-9|\\.]]+")
list_back <- as.numeric(list_back)

list_own <- users$LIST_OWN
list_own[list_own == ""] <- NA
list_own[list_own == "16+ hours"] <- 17
list_own[list_own == "More than 16 hours"] <- 17
list_own[list_own == "Less than an hour"] <- 0.5
list_own <- str_extract(list_own, "[[0-9|\\.]]+")
list_own <- as.numeric(list_own)

# unwrap some factors into numerics --------------------------------------------
# first for words
heard_of <- as.character(words$HEARD_OF)
heard_of[heard_of == ""] <- "NA"
heard_of[heard_of == "Ever heard of"] <- "Heard of"
heard_of[heard_of == "Ever heard music by"] <- "Listened to ever"
heard_of[heard_of == "Heard of and listened to music EVER"] <- "Listened to ever"
heard_of[heard_of == "Heard of and listened to music RECENTLY"] <- "Listened to recently"
words_heard <- class2ind(as.factor(heard_of), drop2nd = TRUE)
colnames(words_heard) <- paste0("HEARD_OF.", make.names(colnames(words_heard)))

own <- make.names(words$OWN_ARTIST_MUSIC)
own[own == "Don.t.know"] <- "don.t.know"
words_own <- class2ind(as.factor(own), drop2nd = TRUE)
colnames(words_own) <- paste0("OWN_ARTIST_MUSIC.", make.names(colnames(words_own)))

words[, HEARD_OF:=NULL]
words[, OWN_ARTIST_MUSIC:=NULL]
words <- cbind(words, words_heard, words_own)

# now for users
gender.FEMALE <- class2ind(as.factor(users$GENDER), drop2nd = TRUE)
region <- class2ind(as.factor(users$REGION), drop2nd = TRUE)
colnames(region) <- paste0("region.", make.names(colnames(region)))
working <- class2ind(as.factor(users$WORKING), drop2nd = TRUE)
colnames(working) <- paste0("working.", make.names(colnames(working)))
music <- class2ind(as.factor(users$MUSIC), drop2nd = TRUE)
colnames(music) <- paste0("music.", make.names(colnames(music)))

users[, GENDER:=NULL]
users[, REGION:=NULL]
users[, WORKING:=NULL]
users[, MUSIC:=NULL]
users[, LIST_OWN:=NULL]
users[, LIST_BACK:=NULL]

users <- cbind(users, gender.FEMALE, region, working, music, list_own, list_back)
head(users)

use_data(train, overwrite = T)
use_data(test, overwrite = T)
use_data(words, overwrite = T)
use_data(users, overwrite = T)
