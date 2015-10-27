
################################################################################
# Script latent factor approaches using stochastic gradient descent
################################################################################

## ---- libraries ----
library("emi")

## ---- evaluate ----
data(train)
data(words)
data(users)

data_list <- list(train = train, words = words, users = users)
train_opts <- list(iter_max = 1e5, eta = 0.5, lambads = rep(0.01, 4))
lf_eval <- evaluate(data_list, lf_sgd_train, train_opts, lf_sgd_predict)

lapply(lf_eval$pred_pairs, function(x) {
  plot(x[[1]], x[[2]], asp = 1)
  Sys.sleep(1)
})
lf_eval$errs






