# Relevance Classification (third set of code)
# This code uses the Naives Bayes method to classify the relevance of articles to the SRC/URC

# load libraries
library(quanteda)
library(quanteda.textmodels)
library(caret)
library(tidyverse)

# Syrian Refugee Crisis
# load data
load("src_articles.RData")
src_coded <- read.csv("src_training_articles_coded.csv")

# split training and heldout data
src_newspaper <- src_coded$Newspaper
set.seed(12345)  # Setting seed for reproducibility
train_index <- createDataPartition(src_newspaper, p = 0.5, list = FALSE) # stratified sampling
src_coded$train <- 0
src_coded$train[train_index] <- 1

# label in mass dataset training and test data
relevance_subset <- src_coded[, c("ID", "relevance", "train")]
src_df <- merge(src_df, relevance_subset, by = "ID", all.x = TRUE)
src_df$train <- ifelse(is.na(src_df$train), 0, src_df$train)

# create a corpus
src_corpus <- corpus(src_df, text_field = "Article")

# create a document-feature matrix with preprocessing
src_dfm <- src_corpus %>%
  tokens() %>%
  tokens_tolower() %>%
  tokens(remove_punct = TRUE) %>%
  tokens(remove_numbers = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_wordstem() %>%
  dfm() %>%
  dfm_tfidf()

# subset training and test set observations
src_dfm_train <- dfm_subset(src_dfm, train)
src_dfm_test <- dfm_subset(src_dfm, !train)

# naive Bayes model
src_nb_train <- textmodel_nb(x = src_dfm_train,
                         y = src_dfm_train$relevance,
                         prior = "docfreq")

# predict for the test data
src_dfm_test$predicted_relevance <- predict(src_nb_train, newdata = src_dfm_test, type = "class")

src_test_results <- data.frame(
  ID = src_dfm_test$ID,
  predicted_relevance = src_dfm_test$predicted_relevance)

src_df <- merge(src_df, src_test_results, by = "ID", all.x = TRUE)

# extract heldout set
heldout_rows <- !is.na(src_df$relevance) & !is.na(src_df$predicted_relevance)
src_heldout <- src_df[heldout_rows, ]

# confusion matrix
src_confusion_nb <- table(predicted_classification = src_heldout$predicted_relevance,
                      true_classification = src_heldout$relevance)

src_confusion_nb_statistics <- confusionMatrix(src_confusion_nb, positive = "1")
src_confusion_nb_statistics

# label relevance for all articles
src_df$predicted_relevance <- as.numeric(src_df$predicted_relevance)
src_df$predicted_relevance <- ifelse(src_df$predicted_relevance == 2, 1, ifelse(src_df$predicted_relevance == 1, 0, src_df$predicted_relevance))
src_df$combined_relevance <- ifelse(!is.na(src_df$relevance), src_df$relevance, src_df$predicted_relevance)
src_official <- subset(src_df, combined_relevance == 1)

# save relevant articles in file
src_official <- src_official[, -((ncol(src_official)-3):ncol(src_official))]
write.csv(src_official, "src_official_articles.csv", row.names = FALSE)

# Ukrainian Refugee Crisis
# load data
load("urc_articles.RData")
urc_coded <- read.csv("urc_training_articles_coded.csv")

# split training and heldout data
newspaper <- urc_coded$Newspaper
set.seed(12345)  # Setting seed for reproducibility
train_index <- createDataPartition(newspaper, p = 0.5, list = FALSE) # stratified sampling
urc_coded$train <- 0
urc_coded$train[train_index] <- 1

# label in mass dataset training and test data
relevance_subset <- urc_coded[, c("ID", "relevance", "train")]
urc_df <- merge(urc_df, relevance_subset, by = "ID", all.x = TRUE)
urc_df$train <- ifelse(is.na(urc_df$train), 0, urc_df$train)

# create a corpus
urc_corpus <- corpus(urc_df, text_field = "Article")

# create a document-feature matrix with preprocessing
urc_dfm <- urc_corpus %>%
  tokens() %>%
  tokens_tolower() %>%
  tokens(remove_punct = TRUE) %>%
  tokens(remove_numbers = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_wordstem() %>%
  dfm() %>%
  dfm_tfidf()

# subset training and test set observations
urc_dfm_train <- dfm_subset(urc_dfm, train)
urc_dfm_test <- dfm_subset(urc_dfm, !train)

# naive Bayes model
nb_train <- textmodel_nb(x = urc_dfm_train,
                         y = urc_dfm_train$relevance,
                         prior = "docfreq")

# predict for the test data
urc_dfm_test$predicted_relevance <- predict(nb_train, newdata = urc_dfm_test, type = "class")

urc_test_results <- data.frame(
  ID = urc_dfm_test$ID,
  predicted_relevance = urc_dfm_test$predicted_relevance)

urc_df <- merge(urc_df, urc_test_results, by = "ID", all.x = TRUE)

# extract heldout set
heldout_rows <- !is.na(urc_df$relevance) & !is.na(urc_df$predicted_relevance)
urc_heldout <- urc_df[heldout_rows, ]

# confusion matrix
confusion_nb <- table(predicted_classification = urc_heldout$predicted_relevance,
                      true_classification = urc_heldout$relevance)

confusion_nb_statistics <- confusionMatrix(confusion_nb, positive = "1")
confusion_nb_statistics

# label relevance for all articles
urc_df$predicted_relevance <- as.integer(urc_df$predicted_relevance)
urc_df$predicted_relevance <- ifelse(urc_df$predicted_relevance == 2, 1, ifelse(urc_df$predicted_relevance == 1, 0, urc_df$predicted_relevance))
urc_df$combined_relevance <- ifelse(!is.na(urc_df$relevance), urc_df$relevance, urc_df$predicted_relevance)
urc_official <- subset(urc_df, combined_relevance == 1)

# save relevant articles in file
urc_official <- urc_official[, -((ncol(urc_official)-3):ncol(urc_official))]
write.csv(urc_official, "urc_official_articles.csv", row.names = FALSE)
