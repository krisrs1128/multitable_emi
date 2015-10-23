
################################################################################
# Latent factor approach, fitted with SGD, described in online writeup,
# https://speakerd.s3.amazonaws.com/presentations/501d641214507500020491a3/MusicHackathonShanda.pdf
################################################################################

library("emi")

# build the ratings matrix
data(train)
R <- dcast(train, User ~ Track, value.var = "Rating")[1:1000, ]
R <- as.matrix(R[, -1, with = F])

# make predictions
mu <- mean(R, na.rm = T)
obs_ix <- which(!is.na(R), arr.ind = T)
train_ix <- sample(.8 * nrow(obs_ix))
sgd_res <- lf_sgd(R - mu, obs_ix[train_ix, ], rep(5, 4), iter_max = 1e6, eta = 0.005)
plot(mu + sgd_res$bu, apply(R, 1, mean, na.rm = T))
abline(0, 1)
R_hat <- mu + sgd_res$bu %*% matrix(1, 1, ncol(R)) +
  matrix(1, nrow(R), 1) %*% t(sgd_res$bi) +
  sgd_res$P %*% t(sgd_res$Q)

# compare predictions to truth
plot(R[obs_ix[-train_ix]], R_hat[obs_ix[-train_ix]])
abline(0, 1)
