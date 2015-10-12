
################################################################################
# Experiment using the means for artists / users / tracks times in the baseline
# predictor.
################################################################################

# ---- libraries ----
library("plyr")
library("dplyr")
library("emi")
library("ggplot")

# ---- get-data ----
data(train)
data(test)

# To get a sense of the variation in ratings across users, we can look at
# histograms for each user
train <- colwise(as.factor)(train)
train$Rating <- as.numeric(train$Rating)
summary(train)

# way to many to plot each user separately, let's do overall and choose
# some random users
ggplot(train) +
  geom_histogram(aes(x = Rating), binwidth = 1)

# some randomly chosen users
unique_users <- unique(train$User)
few_users <- filter(train, User %in% sample(unique_users, 20))
ggplot(few_users) +
  geom_histogram(aes(x = Rating)) +
  facet_wrap(~User, scale = "free_y") +
  ggtitle("Ratings for 20 randomly chosen users")

few_users <- filter(train, User %in% sample(unique_users, 200))
ggplot(few_users) +
  geom_boxplot(aes(x = reorder(User, Rating), y = Rating)) +
  theme(axis.text.x = element_text(angle = -90))

# they don't tend to vary too much

# sorting the artists
artist_summaries <- train %>%
  group_by(Artist) %>%
  summarise(mean = mean(Rating), number = length(Rating))
artist_summaries <- arrange(artist_summaries, mean, number)
train$Artist <- factor(train$Artist, artist_summaries$Artist)

# what about artists
ggplot(filter(train, Artist %in% artists_ranked[1:25])) +
  geom_histogram(aes(x = Rating)) +
  facet_grid(Artist ~ ., scale = "free_y") +
  ggtitle("Ratings for worst 25 artists")
ggplot(filter(train, Artist %in% unique_artists[26:50])) +
  geom_histogram(aes(x = Rating)) +
  facet_grid(Artist ~ ., scale = "free_y") +
  ggtitle("Ratings for best 25 artists")

# Did the times they were interviewed make much of a difference?
# some randomly chosen users
ggplot(train) +
  geom_histogram(aes(x = Rating)) +
  facet_grid(Time ~ ., scale = "free_y") +
  ggtitle("Ratings across times")

# there is obvious variation in depth of sampling, and maybe even in
# the distributions (some have many more above 90 e.g.). This may be
# captured by demographic information...
