
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
train <- train[1:10000, ]

# convert to matrix with rownames
colnames(train)
X3 <- cast_ratings(train[, c("User", "Track", "Rating"), with = F])

# construct feature matrix describing user x track combinations
mwords <- words %>%
  filter(User %in% train$User) %>%
  melt(id.vars = c("Artist", "User")) %>%
  arrange(User, variable)
Z_pairs <- acast(mwords, User ~ Artist ~ variable)

head(words)

# construct feature matrix describing the users
Z_users <- users %>%
  filter(User %in% train$User) %>%
  melt(id.vars = "User") %>%
  acast(User ~ variable)

# will repeat this matrix for every artist
Z_users_array <- array(rep(Z_users, length = dim(Z_pairs)[2]), dim = c(dim(Z_users)[1], dim(Z_pairs)[2], dim(Z_users)[2]))
dimnames(Z_users_array) <- list(rownames(Z_users), colnames(Z_pairs), colnames(Z_users))

ix_user <- match(rownames(X), rownames(Z_users_array))
ix_pairs <- match(rownames(X), rownames(Z_pairs))
Z <- abind(Z_users_array[ix_user,, ], Z_pairs[ix_pairs,, ], along = 3)

# now we need to associate each of these artists with one of the tracks in the
# original ratings matrix
artist_tracks <- train[, c("Artist", "Track"), with = F] %>%
  distinct() %>%
  arrange(Track)
ix_track <- match(colnames(X), artist_tracks$Track)
matched_artists <- artist_tracks[ix_track, "Artist", with = F] %>%
  unlist(use.names = F) %>%
  as.character()
Z <- Z[, matched_artists, ]

# lazy person's imputation
Z[is.na(Z)] <- 0

#grad_results <- lf_gd_cov(X, Z, k_factors = 2, lambdas = c(10, 10, 100),
#                          n_iter = 100, batch_p = 1, gamma_pq = 1e-4,
#                          gamma_beta = 1e-8)

grad_results <- lf_gd_cov(X, Z, k_factors = 2, lambdas = c(10, 10, 100),
                          n_iter = 10, batch_factors = 1, batch_samples = .01,
                          gamma_pq = 1e-4, gamma_beta = 1e-8)


plot(log(grad_results$objs))
plot(grad_results$P)
plot(grad_results$Q)
plot(grad_results$beta)

