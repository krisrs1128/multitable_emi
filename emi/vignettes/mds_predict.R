
################################################################################
# Experiment using dimension reduction to perform prediction.
################################################################################

# ---- load-libraries ----
library("plyr")
library("dplyr")
library("emi")
library("reshape2")
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

# calculating distances between artists using words
data(words)
words$User <- as.factor(words$User)
words$Artist <- as.factor(words$Artist)
artist_means <- words %>%
  group_by(Artist) %>%
  summarise_each(funs(mean(., na.rm = T)))

D_artists <- dist(artist_means[, 3:ncol(artist_means), with = F])
artist_mds <- data.frame(Artist = artist_means$Artist, cmdscale(D_artists))

artist_ratings <- train %>%
  group_by(Artist) %>%
  summarise(mean = mean(Rating))

artist_mds <- merge(artist_mds, artist_ratings)
ggplot(artist_mds) +
  geom_point(aes(x = X1, y = X2, col = mean))

# random forest proximity
train <- train[1:10000, ]
X <- merge(users, words, by = "User", all = T)
X <- merge(train, X, by = c("User", "Artist"), all.x = T)

x_num <- as.matrix(X[, 6:ncol(X), with = F])
x_num <- scale(x_num, center = F, scale = apply(x_num, 2, max, na.rm = T))
library("imputation")
x_hat <- SVDImpute(x_num, 10)

x_hat <- x_hat$x
corX <- cor(x_hat)
corX[upper.tri(corX)] <- NA
diag(corX) <- NA
head(arrange(melt(corX), desc(abs(value))))

x_fac <- expand_factors(X[, c("Track", "Time"), with = F])
x <- cbind(x_fac, x_hat)
y <- X$Rating

library("caret")

svdX <- svd(x)
k <- 5
z <- svdX$u[, seq_len(k)] %*% diag(svdX$d[seq_len(k)]) # pc regression

glmnet_model <- train(x = x, y = y, method = "glmnet", trControl = trainControl(verbose = T))
gbm_model <- train(x = z, y = y, method = "gbm", trControl = trainControl(verbose = T))

glmnet_model
gbm_model
plot(y, predict(glmnet_model), asp = 1)
plot(y, predict(glmnet_model), asp = 1)

rpart_model <- train(x = z, y = y, method = "rpart", trControl = trainControl(verbose = T))
plot(rpart_model$finalModel)


