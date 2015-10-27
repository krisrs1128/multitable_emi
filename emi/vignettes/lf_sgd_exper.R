
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
train_opts <- list(iter_max = 1e7, eta = 0.001, lambdas = rep(100, 4))
lf_eval <- evaluate(data_list, lf_sgd_train, train_opts, lf_sgd_predict, K = 2)
lf_eval$errs

lapply(lf_eval$pred_pairs, function(x) {
  plot(x[[1]], x[[2]], asp = 1)
  Sys.sleep(1)
})



