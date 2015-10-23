
################################################################################
# Experiment using dimension reduction to perform prediction.
################################################################################

# ---- load-libraries ----
library("plyr")
library("dplyr")
library("emi")
library("ggplot2")
library("proxy")

# ---- get-data ----
data(train)
data(test)
data(users)
data(words)
data(words_na_ix)

# only using 1/5 of users in this experiment
subset_users <- as.character(sample(unique(train$User), 1e4))
train <- filter(train, User %in% subset_users)
users <- filter(users, User %in% subset_users)
words <- words[words_na_ix == 8, ]
words <- filter(words, User %in% subset_users)
test_ix <- get_folds(n = nrow(train))

na_ix <- sapply(words, function(x) all(is.na(x)))
words <- words[, !na_ix, with = F]

table(table(words$User)) # most users asked to rate 3 tracks

# look at correlations between words
words_mat <- as.matrix(words[, 3:ncol(words), with = F])
cor_mat <- cor(words_mat)
cor_mat[upper.tri(cor_mat)] <- NA
diag(cor_mat) <- NA
arrange(melt(cor_mat), desc(abs(value)))[1:20, ]

# calculate jaccard distances between words
D_words <- words_mat %>%
  dist(method = "jaccard")
words_mds <- cmdscale(D_words)

words_mds <- cbind(words[, c("User", "Artist"), with = F], words_mds) %>%
  data.frame()
words_mds$ix <- 1:nrow(words_mds)

ggplot(words_mds) +
  geom_point(aes(x = V1, y = V2, col = ix)) +
  facet_wrap(~Artist)

ggplot(words_mds) +
  geom_point(aes(x = V1, y = V2)) +
  facet_wrap(~Artist)
