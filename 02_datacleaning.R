# Data cleaning (second set of code)
# This code creates data files for training and test data in preparation for manual coding outside of R

# load libraries
library(dplyr)
library(tidyverse)

# load data
load("src_articles.RData")
load("urc_articles.RData")

# sample newspapers for training and test data
set.seed(12345)

# extract 10% of articles for training and heldout set
sample_per_newspaper <- function(data) {
  num_rows_to_sample <- ceiling(0.1 * nrow(data))
  sample_data <- data[sample(nrow(data), num_rows_to_sample, replace = FALSE), ]
  return(sample_data)
}

# create training data for SRC
src_newspapers_training <- lapply(split(src_df, src_df$Newspaper), sample_per_newspaper)
src_training_articles <- do.call(rbind, src_newspapers_training)
rownames(src_training_articles) <- NULL # reset row names
write.csv(src_training_articles, "src_training_articles.csv", row.names = FALSE)

# clean training data in csv for manual coding and reload
src_training_articles_clean <- read.csv("src_training_articles_clean.csv")
src_training_articles_clean$Date <- as.Date(src_training_articles_clean$Date, format = "%d/%m/%Y")

# create SRC test data
src_newspapers_test <- src_df %>% anti_join(src_training_articles_clean)
write.csv(src_newspapers_test, "src_test_articles.csv", row.names = FALSE)

# create training data for URC
urc_newspapers_training <- lapply(split(urc_df, urc_df$Newspaper), sample_per_newspaper)
urc_training_articles <- do.call(rbind, urc_newspapers_training)
rownames(urc_training_articles) <- NULL # reset row names
write.csv(urc_training_articles, "urc_training_articles.csv", row.names = FALSE)

# clean training data in csv for manual coding and reload
urc_training_articles_clean <- read.csv("urc_training_articles_clean.csv")
urc_training_articles_clean$Date <- as.Date(urc_training_articles_clean$Date, format = "%d/%m/%Y")

# create URC test data
urc_newspapers_test <- urc_df %>% anti_join(urc_training_articles_clean)
write.csv(urc_newspapers_test, "urc_test_articles.csv", row.names = FALSE)
