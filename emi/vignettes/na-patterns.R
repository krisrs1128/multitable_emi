
################################################################################
# Study the missingness patterns in the words component of the survey
################################################################################

# load libraries
library("emi")
library("data.table")

# extract data excluding keys
data(words)
words_na <- !is.na(words[, 3:ncol(words), with = F])

# get unique NA patterns, for each row
y <- do.call(paste, data.table(words_na))
unique_patterns <- names(table(y))
string_to_vec <- function(x) as.logical(unlist(strsplit(x, " ")))
na_patterns <- data.frame(do.call(rbind, lapply(unique_patterns, string_to_vec)))

# out of 94 possible words, most people were asked about 60
apply(na_patterns, 1, sum)

# there are only 16 unique patterns of survey responses, out of
# 118,000 total responses
na_counts <- as.numeric(table(y))
na_counts
na_patterns
image(as.matrix(na_patterns))
