# Prep for LLM Analysis on SRC/URC data

# load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(quanteda)
library(stm)
library(caret)

# load data ---------------------------------------------------------------
src_df <- read.csv("src_official_articles.csv")
urc_df <- read.csv("urc_official_articles.csv")

# add meta data on newspaper outlet political orientation
src_df$Rightwing <- ifelse(src_df$Newspaper %in% c("The Guardian", "The Independent", "Daily Mirror"), 0, 1)
urc_df$Rightwing <- ifelse(urc_df$Newspaper %in% c("The Guardian", "The Independent", "Daily Mirror"), 0, 1)

# convert date in datasets to year month
src_df$year_month <- format(ymd(src_df$Date), "%Y-%m") 
urc_df$year_month <- format(ymd(urc_df$Date), "%Y-%m") 

# potential article topics ------------------------------------------------

# create corpus and dfm for src data
src_corpus <- src_df %>%
  corpus(text_field = "Article")

src_dfm <- src_corpus %>%
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(c("syria", "syrian", "syrians", "refugee", "refugees", "crisis")) %>% # remove common subject words
  tokens_remove(c("said", "says", "like", "one", "just", "can", "people", "mr", "think", "year", "years", 
                  "get", "got", "now", "last", "also", "many", "make", "want", "even", "since", "day")) %>% # remove other stopwords
  tokens_remove(c("block-time", "published-time", "time", "GMT", "people", "bst", "bbc", "load-date", "related", "graphic")) %>% # remove common words in articles
  dfm()

src_dfm <- src_dfm %>%
  dfm_trim(min_docfreq = .1,
           max_docfreq = .9,
           docfreq_type = "prop")

# run stm model
stm_src <- stm(documents = src_dfm,
               K = 10,
               seed = 12345)

# create corpus and dfm for urc data
urc_corpus <- urc_df %>%
  corpus(text_field = "Article")

urc_dfm <- urc_corpus %>%
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE) %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(c("ukraine", "ukrainian", "ukrainians", "refugee", "refugees", "crisis", "russia", "kyiv")) %>% # remove common subject words
  tokens_remove(c("said", "says", "like", "one", "just", "can", "people", "mr", "think", "year", "years", 
                  "get", "got", "now", "last", "also", "many", "make", "want", "even", "since", "day")) %>% # remove other stopwords
  tokens_remove(c("block-time", "published-time", "time", "GMT", "people", "bst", "bbc", "load-date", "related", "graphic")) %>% # remove common words in articles
  dfm()

urc_dfm <- urc_dfm %>%
  dfm_trim(min_docfreq = .1,
           max_docfreq = .9,
           docfreq_type = "prop")

# run stm model
stm_urc <- stm(documents = urc_dfm,
               K = 10,
               seed = 12345)

# split training and heldout data -----------------------------------------

# extract 10% of articles for manual coding
set.seed(12345)
sample_per_newspaper <- function(data) {
  num_rows_to_sample <- ceiling(0.1 * nrow(data))
  sample_data <- data[sample(nrow(data), num_rows_to_sample, replace = FALSE), ]
  return(sample_data)
}

# create training data for SRC
src_newspapers_training <- lapply(split(src_df, src_df$Newspaper), sample_per_newspaper)
src_training_articles <- do.call(rbind, src_newspapers_training)
rownames(src_training_articles) <- NULL # reset row names
write.csv(src_training_articles, "src_official_training.csv", row.names = FALSE) # export for manual coding

# create training data for URC
urc_newspapers_training <- lapply(split(urc_df, urc_df$Newspaper), sample_per_newspaper)
urc_training_articles <- do.call(rbind, urc_newspapers_training)
rownames(urc_training_articles) <- NULL # reset row names
write.csv(urc_training_articles, "urc_official_training.csv", row.names = FALSE) # export for manual coding


# prepare src data for llm ------------------------------------------------
# import manually coded data
src_coded_articles <- read.csv("src_official_training_coded.csv")

# create test data
src_df$Solidarity <- NA
src_df$Category <- NA
src_test_articles <- src_df %>% anti_join(src_coded_articles, by = "ID")
write.csv(src_test_articles, "src_official_test.csv", row.names = FALSE)

# split manually coded data into training and heldout sets
src_coded_articles <- src_coded_articles[!is.na(src_coded_articles$Category) & !is.na(src_coded_articles$solidarity), ]
set.seed(12345)
train_indices <- createDataPartition(src_coded_articles$Newspaper, p = 0.60, list = FALSE) # create stratified indices for the training set (60%)
src_llm_train_set <- src_coded_articles[train_indices, ]
src_llm_validation_set <- src_coded_articles[-train_indices, ]
write.csv(src_llm_train_set, "src_llm_train.csv", row.names = FALSE)
write.csv(src_llm_validation_set, "src_llm_validation.csv", row.names = FALSE)


# prepare urc data for llm ------------------------------------------------
# import manually coded data
urc_coded_articles <- read.csv("urc_official_training_coded.csv")

# create test data
urc_df$Category <- NA
urc_test_articles <- urc_df %>% anti_join(urc_coded_articles, by = "ID")
write.csv(urc_test_articles, "urc_official_test.csv", row.names = FALSE)

# split manually coded data into training and heldout sets
urc_coded_articles <- urc_coded_articles[!is.na(urc_coded_articles$Category), ]
set.seed(12345)
train_indices <- createDataPartition(urc_coded_articles$Newspaper, p = 0.60, list = FALSE) # create stratified indices for the training set (60%)
urc_llm_train_set <- urc_coded_articles[train_indices, ]
urc_llm_validation_set <- urc_coded_articles[-train_indices, ]
write.csv(urc_llm_train_set, "urc_llm_train.csv", row.names = FALSE)
write.csv(urc_llm_validation_set, "urc_llm_validation.csv", row.names = FALSE)


# Training Data Statistics ------------------------------------------------
# src training data
prop.table(table(src_llm_train_set$Category))
prop.table(table(src_llm_validation_set$Category))
prop.table(table(src_llm_train_set$Solidarity))
prop.table(table(src_llm_validation_set$Solidarity))

# src training data
prop.table(table(src_llm_train_set$Category))
prop.table(table(src_llm_validation_set$Category))
prop.table(table(src_llm_train_set$Solidarity))
prop.table(table(src_llm_validation_set$Solidarity))

# SRC LLM topic classification ------------------------------------------------
# NOTE: large language model run in 05_src_topic_classification.ipynb

src_tc_pred <- read.csv("src_llm_tc_predictions.csv")
src_tc_pred <- src_tc_pred[src_tc_pred$category_predictions != "", ]
src_tc_pred <- src_tc_pred[!is.na(as.numeric(src_tc_pred$category_predictions)), ]
src_tc_pred <- src_tc_pred %>%
  mutate(Category = recode(category_predictions,
                           `0` = "Aid",
                           `1` = "Conditions",
                           `3` = "Migration",
                           `4` = "Policy"))
write.csv(src_tc_pred, "src_official_test2.csv", row.names = FALSE)

# SRC LLM sentiment classification ------------------------------------------------------
# NOTE: large language model run in 06_src_sentiment.ipynb

src_sa_pred <- read.csv("src_llm_solidarity_predictions.csv")
src_df <- merge(src_df, src_sa_pred[, c("ID", "solidarity_predictions")], by = "ID", all.x = TRUE)
colnames(src_df)[colnames(src_df) == "solidarity_predictions"] <- "Solidarity"

src_df <- merge(src_df, src_sa_pred[, c("ID", "Category")], by = "ID", all.x = TRUE)
src_df <- merge(src_df, src_llm_train_set[c("ID", "Solidarity", "Category")], by = "ID", all.x = TRUE)
src_df$Category <- ifelse(is.na(src_df$Category.x), src_df$Category.y, src_df$Category.x)
src_df <- src_df[, !(names(src_df) %in% c("Category.x", "Category.y"))]
src_df$Solidarity <- ifelse(is.na(src_df$Solidarity.x), src_df$Solidarity.y, src_df$Solidarity.x)
src_df <- src_df[, !(names(src_df) %in% c("Solidarity.x", "Solidarity.y"))]

src_df <- merge(src_df, src_llm_validation_set[c("ID", "Solidarity", "Category")], by = "ID", all.x = TRUE)
src_df$Category <- ifelse(is.na(src_df$Category.x), src_df$Category.y, src_df$Category.x)
src_df <- src_df[, !(names(src_df) %in% c("Category.x", "Category.y"))]
src_df$Solidarity <- ifelse(is.na(src_df$Solidarity.x), src_df$Solidarity.y, src_df$Solidarity.x)
src_df <- src_df[, !(names(src_df) %in% c("Solidarity.x", "Solidarity.y"))]

src_analysis_df <- src_df[!is.na(src_df$Solidarity), ]
write.csv(src_analysis_df, "src_analysis_df.csv", row.names = FALSE)

# URC LLM topic classification --------------------------------------------
# NOTE: large language model run in 07_urc_topic_classification.ipynb

urc_tc_pred <- read.csv("urc_llm_tc_predictions.csv")
urc_tc_pred <- urc_tc_pred %>%
  mutate(Category = recode(category_predictions,
                           `0` = "Aid",
                           `1` = "Conditions",
                           `2` = "Migration",
                           `3` = "Policy"))
write.csv(urc_tc_pred, "urc_official_test2.csv", row.names = FALSE)


# URC LLM sentiment classification ----------------------------------------
# NOTE: large language model run in 08_urc_sentiment.ipynb

urc_sa_pred <- read.csv("urc_llm_solidarity_predictions.csv")
urc_df <- merge(urc_df, urc_sa_pred[, c("ID", "solidarity_predictions", "Category")], by = "ID", all.x = TRUE)
colnames(urc_df)[colnames(urc_df) == "solidarity_predictions"] <- "Solidarity"

urc_df <- merge(urc_df, urc_llm_train_set[c("ID", "Solidarity", "Category")], by = "ID", all.x = TRUE)
urc_df$Category <- ifelse(is.na(urc_df$Category.x), urc_df$Category.y, urc_df$Category.x)
urc_df <- urc_df[, !(names(urc_df) %in% c("Category.x", "Category.y"))]
urc_df$Solidarity <- ifelse(is.na(urc_df$Solidarity.x), urc_df$Solidarity.y, urc_df$Solidarity.x)
urc_df <- urc_df[, !(names(urc_df) %in% c("Solidarity.x", "Solidarity.y"))]

urc_df <- merge(urc_df, urc_llm_validation_set[c("ID", "Solidarity", "Category")], by = "ID", all.x = TRUE)
urc_df$Category <- ifelse(is.na(urc_df$Category.x), urc_df$Category.y, urc_df$Category.x)
urc_df <- urc_df[, !(names(urc_df) %in% c("Category.x", "Category.y"))]
urc_df$Solidarity <- ifelse(is.na(urc_df$Solidarity.x), urc_df$Solidarity.y, urc_df$Solidarity.x)
urc_df <- urc_df[, !(names(urc_df) %in% c("Solidarity.x", "Solidarity.y"))]

urc_analysis_df <- urc_df[!is.na(urc_df$Solidarity), ]
write.csv(urc_analysis_df, "urc_analysis_df.csv", row.names = FALSE)