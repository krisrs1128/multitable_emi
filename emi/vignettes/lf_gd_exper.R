
################################################################################
# Experiment using stochastic gradient descent with covariates
################################################################################

## ---- libraries ----
library("abind")
library("data.table")
library("plyr")
library("dplyr")
library("emi")

## ---- evaluate ----
data(train)
data(words)
data(users)

train <- train[1:10000, ]

# convert to matrix with rownames
X <- dcast(train, User ~ Track, value.var = "Rating", fill = NA)
X2 <- as.matrix(X[, -1, with = F])
colnames(X2) <- colnames(X)[-1]
rownames(X2) <- unlist(X[, 1, with = F])
X <- X2
rm(X2)

# construct feature matrix describing the users
Z_users <- array(0, c(nrow(X), ncol(X), ncol(users) - 1))
dimnames(Z_users) <- list(rownames(X), colnames(X), colnames(users)[-1])

for(j in seq_len(ncol(users))[-1]) {
  cat(sprintf("Creating feature %g \n", j))
  match_ix <- match(rownames(Z_users), as.character(users$User))
  Z_users[,, j - 1] <- unlist(users[match_ix, j, with = F])
}

track_artist_map <- train[, c("Artist", "Track"), with = F] %>%
  distinct()

# construct feature matrix describing user x track combinations
mwords <- melt(words)
variables <- unique(mwords$variable)
Z_pairs <- vector(length = length(variables) - 2, mode = "list")
for(j in seq_along(variables)) {
  cur_words <- filter(mwords, variable == variables[j])
  Z_pairs[[j]] <- dcast(cur_words, User ~ Artist)
}

for(j in seq_along(Z_pairs)) {
  match_ix <- match(rownames(Z_users), as.character(unlist(Z_pairs[[j]]$User)))
  Z_pairs[[j]] <- Z_pairs[[j]][match_ix, ]
  Z_pairs[[j]] <- as.matrix(Z_pairs[[j]][, -1, with = F])
}

# now abind them together
abind_xy <- function(x, y) abind(x, y, along = 3)
Z_pairs <- Reduce(abind_xy, Z_pairs)
rownames(Z_pairs) <- rownames(Z_users)

Z <- abind(Z_pairs, Z_arr, along = 3)
