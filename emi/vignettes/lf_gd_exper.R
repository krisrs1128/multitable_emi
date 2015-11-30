
################################################################################
# Experiment using stochastic gradient descent with covariates
################################################################################

## ---- libraries ----
library("abind")
library("reshape2")
library("data.table")
library("plyr")
library("dplyr")
library("emi")

## ---- evaluate ----
data(train)
data(words)
data(users)

train <- train[1:1000, ]

# convert to matrix with rownames
X <- dcast(train, User ~ Track, value.var = "Rating", fill = NA)
X2 <- as.matrix(X[, -1, with = F])
colnames(X2) <- colnames(X)[-1]
rownames(X2) <- unlist(X[, 1, with = F])
X <- X2
rm(X2)

# construct feature matrix describing user x track combinations
mwords <- words %>%
  filter(User %in% train$User) %>%
  melt(id.vars = c("Artist", "User")) %>%
  arrange(User, variable)
Z_pairs <- acast(mwords, User ~ Artist ~ variable)

# construct feature matrix describing the users
Z_users <- users %>%
  filter(User %in% train$User) %>%
  melt(id.vars = "User") %>%
  acast(User ~ variable)

# will repeat this matrix for every artist
Z_users_array <- array(rep(Z_users, length = dim(Z_pairs)[2]), dim = c(923, 50, 50))
dimnames(Z_users_array) <- list(rownames(Z_users), unique(words$Artist),
                                colnames(Z_users))

ix_user <- match(rownames(X), rownames(Z_users_array))
ix_pairs <- match(rownames(X), rownames(Z_pairs))
Z <- abind(Z_users_array[ix_user,, ], Z_pairs[ix_pairs,, ], along = 3)

# now we need to associate each of these artists with one of the tracks in the
# original ratings matrix
artist_tracks <- train[, c("Artist", "Track"), with = F] %>%
  distinct() %>%
  arrange(Track)
ix_track <- match(colnames(X), artist_tracks$Track)
matched_artists <- artist_tracks[ix_track, "Artist", with = F] %>% unlist(use.names = F)
Z <- Z[, matched_artists, ]

# lazy person's imputation
Z[is.na(Z)] <- 0

grad_results <- lf_gd_cov(X, Z, k_factors = 3, lambas = c(1, 100, 1000),
                          n_iter = 2, gamma = 0.001)
grad_results$beta
plot(grad_results$P[, 1:2])
plot(grad_results$Q[, 1:2])
