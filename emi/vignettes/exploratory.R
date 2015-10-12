
## ---- libraries ----
library("emi")
library("plyr")
library("dplyr")
library("reshape2")
library("ggplot2")
data(users)
data(words)

## ---- filter-data ----
users <- users[1:1000, ]
words <- words[words$V2 %in% users$RESPID, ]

## --- users-plots ----
for(cur_col in colnames(users)) {
  print(ggplot(users) +
    geom_histogram(aes_string(x = cur_col)) +
    theme(axis.text.x = element_text(angle = -90)) +
    ggtitle(cur_col))
}

## --- users-pairwise ---
for(i in seq_along(colnames(users))) {
  for(j in seq_len(i - 1)) {
    print(ggplot(users) +
      geom_jitter(aes_string(x = colnames(users)[i], y = colnames(users)[j])) +
      theme(axis.text.x = element_text(angle = -90)) +
      ggtitle(paste(colnames(users)[c(i, j)], sep = "-")))
  }
}

head(words)
artists <- unique(words$V1)
mwords <- melt(words, id.vars = c("V1", "V2"))

for(cur_var in unique(mwords$variable)) {
  cur_words <- filter(mwords, variable == cur_var, !is.na(value))
  dev.new()
  ggplot(cur_words) +
    geom_bar(aes(x = V2, col = value)) +
    theme(legend.position = "none") +
    facet_wrap(~ V1, scale = "free_x")
}

dwords <- dist(words[, 4:ncol(words)])
plot(hclust(dwords))
