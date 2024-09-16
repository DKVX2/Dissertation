# Dataframe Creation (first set of code)
# This code takes raw word document input from LexisNexis and converts it into R data frames for data processing

# Load Libraries
library(LexisNexisTools)
library(dplyr)
library(tidyverse)

## Syrian Refugee Crisis Dataframe

# Newspaper 1: Daily Express
src_dailyexpress <- lnt_read(x = "src_dailyexpress_raw.docx")
meta_df <- src_dailyexpress@meta
articles_df <- src_dailyexpress@articles
src_dailyexpress_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
src_dailyexpress_df <- distinct(src_dailyexpress_df, Headline, .keep_all = TRUE)
src_dailyexpress_df <- distinct(src_dailyexpress_df, Article, .keep_all = TRUE)
src_dailyexpress_df$Newspaper <- "Daily Express"
src_dailyexpress_df$Newspapertype <- "Tabloid"

# Newspaper 2: Daily Mail
src_dailymail <- lnt_read(x = "src_dailymail_raw.docx")
meta_df <- src_dailymail@meta
articles_df <- src_dailymail@articles
src_dailymail_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
src_dailymail_df <- distinct(src_dailymail_df, Headline, .keep_all = TRUE)
src_dailymail_df <- distinct(src_dailymail_df, Article, .keep_all = TRUE)
src_dailymail_df$Newspaper <- "Daily Mail"
src_dailymail_df$Newspapertype <- "Tabloid"

# Newspaper 3: Daily Mirror
src_dailymirror <- lnt_read(x = "src_dailymirror_raw.docx")
meta_df <- src_dailymirror@meta
articles_df <- src_dailymirror@articles
src_dailymirror_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
src_dailymirror_df <- distinct(src_dailymirror_df, Headline, .keep_all = TRUE)
src_dailymirror_df <- distinct(src_dailymirror_df, Article, .keep_all = TRUE)
src_dailymirror_df$Newspaper <- "Daily Mirror"
src_dailymirror_df$Newspapertype <- "Tabloid"

# Newspaper 4: Daily Telegraph
src_dailytelegraph <- lnt_read(x = "src_dailytelegraph_raw.docx")
meta_df <- src_dailytelegraph@meta
articles_df <- src_dailytelegraph@articles
src_dailytelegraph_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
src_dailytelegraph_df <- distinct(src_dailytelegraph_df, Headline, .keep_all = TRUE)
src_dailytelegraph_df <- distinct(src_dailytelegraph_df, Article, .keep_all = TRUE)
src_dailytelegraph_df$Newspaper <- "Daily Telegraph"
src_dailytelegraph_df$Newspapertype <- "Broadsheet"

# Newspaper 5: The Guardian
src_theguardian_df <- data.frame()
for (i in 1:5) {
  file_name <- paste0("src_theguardian_raw", i, ".docx")
  src_doc <- lnt_read(x = file_name)
  meta_df <- src_doc@meta
  articles_df <- src_doc@articles
  doc_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
  src_theguardian_df <- rbind(src_theguardian_df, doc_df)
}
src_theguardian_df <- distinct(src_theguardian_df, Headline, .keep_all = TRUE)
src_theguardian_df <- distinct(src_theguardian_df, Article, .keep_all = TRUE)
src_theguardian_df$Newspaper <- "The Guardian"
src_theguardian_df$Newspapertype <- "Broadsheet"

# Newspaper 6: The Independent
src_theindependent_df <- data.frame()
for (i in 1:6) {
  file_name <- paste0("src_theindependent_raw", i, ".docx")
  src_doc <- lnt_read(x = file_name)
  meta_df <- src_doc@meta
  articles_df <- src_doc@articles
  doc_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
  src_theindependent_df <- rbind(src_theindependent_df, doc_df)
}
src_theindependent_df <- distinct(src_theindependent_df, Headline, .keep_all = TRUE)
src_theindependent_df <- distinct(src_theindependent_df, Article, .keep_all = TRUE)
src_theindependent_df$Newspaper <- "The Independent"
src_theindependent_df$Newspapertype <- "Broadsheet"

# Newspaper 7: The Sun
src_thesun <- lnt_read(x = "src_thesun_raw.docx")
meta_df <- src_thesun@meta
articles_df <- src_thesun@articles
src_thesun_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
src_thesun_df <- distinct(src_thesun_df, Headline, .keep_all = TRUE)
src_thesun_df <- distinct(src_thesun_df, Article, .keep_all = TRUE)
src_thesun_df$Newspaper <- "The Sun"
src_thesun_df$Newspapertype <- "Tabloid"

# Newspaper 8: The Times
src_thetimes_df <- data.frame()
for (i in 1:2) {
  file_name <- paste0("src_thetimes_raw", i, ".docx")
  src_doc <- lnt_read(x = file_name)
  meta_df <- src_doc@meta
  articles_df <- src_doc@articles
  doc_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
  src_thetimes_df <- rbind(src_thetimes_df, doc_df)
}
src_thetimes_df <- distinct(src_thetimes_df, Headline, .keep_all = TRUE)
src_thetimes_df <- distinct(src_thetimes_df, Article, .keep_all = TRUE)
src_thetimes_df$Newspaper <- "The Times"
src_thetimes_df$Newspapertype <- "Broadsheet"

# Combine all 8 newspaper outlets
src_df <- rbind(src_dailymail_df, src_dailyexpress_df, src_dailymirror_df, src_thesun_df, src_theguardian_df, 
                src_theindependent_df, src_dailytelegraph_df, src_thetimes_df)
src_df <- select(src_df, -Section, -Edition, -Graphic)
src_df$ID <- 1:nrow(src_df)
src_df <- src_df[complete.cases(src_df$Date), ]

# Save the dataframe
save(src_df, file = "src_articles.RData")


## Ukrainian Refugee Crisis Dataframe

# Newspaper 1: Daily Express
urc_dailyexpress <- lnt_read(x = "urc_dailyexpress_raw.docx")
meta_df <- urc_dailyexpress@meta
articles_df <- urc_dailyexpress@articles
urc_dailyexpress_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
urc_dailyexpress_df <- distinct(urc_dailyexpress_df, Headline, .keep_all = TRUE)
urc_dailyexpress_df <- distinct(urc_dailyexpress_df, Article, .keep_all = TRUE)
urc_dailyexpress_df$Newspaper <- "Daily Express"
urc_dailyexpress_df$Newspapertype <- "Tabloid"

# Newspaper 2: Daily Mail
urc_dailymail <- lnt_read(x = "urc_dailymail_raw.docx")
meta_df <- urc_dailymail@meta
articles_df <- urc_dailymail@articles
urc_dailymail_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
urc_dailymail_df <- distinct(urc_dailymail_df, Headline, .keep_all = TRUE)
urc_dailymail_df <- distinct(urc_dailymail_df, Article, .keep_all = TRUE)
urc_dailymail_df$Newspaper <- "Daily Mail"
urc_dailymail_df$Newspapertype <- "Tabloid"

# Newspaper 3: Daily Mirror
urc_dailymirror_df <- data.frame()
for (i in 1:2) {
  file_name <- paste0("urc_dailymirror_raw", i, ".docx")
  urc_doc <- lnt_read(x = file_name)
  meta_df <- urc_doc@meta
  articles_df <- urc_doc@articles
  doc_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
  urc_dailymirror_df <- rbind(urc_dailymirror_df, doc_df)
}
urc_dailymirror_df <- distinct(urc_dailymirror_df, Headline, .keep_all = TRUE)
urc_dailymirror_df <- distinct(urc_dailymirror_df, Article, .keep_all = TRUE)
urc_dailymirror_df$Newspaper <- "Daily Mirror"
urc_dailymirror_df$Newspapertype <- "Tabloid"

# Newspaper 4: Daily Telegraph
urc_dailytelegraph <- lnt_read(x = "urc_dailytelegraph_raw.docx")
meta_df <- urc_dailytelegraph@meta
articles_df <- urc_dailytelegraph@articles
urc_dailytelegraph_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
urc_dailytelegraph_df <- distinct(urc_dailytelegraph_df, Headline, .keep_all = TRUE)
urc_dailytelegraph_df <- distinct(urc_dailytelegraph_df, Article, .keep_all = TRUE)
urc_dailytelegraph_df$Newspaper <- "Daily Telegraph"
urc_dailytelegraph_df$Newspapertype <- "Broadsheet"

# Newspaper 5: The Guardian
urc_theguardian_df <- data.frame()
for (i in 1:2) {
  file_name <- paste0("urc_theguardian_raw", i, ".docx")
  urc_doc <- lnt_read(x = file_name)
  meta_df <- urc_doc@meta
  articles_df <- urc_doc@articles
  doc_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
  urc_theguardian_df <- rbind(urc_theguardian_df, doc_df)
}
urc_theguardian_df <- distinct(urc_theguardian_df, Headline, .keep_all = TRUE)
urc_theguardian_df <- distinct(urc_theguardian_df, Article, .keep_all = TRUE)
urc_theguardian_df$Newspaper <- "The Guardian"
urc_theguardian_df$Newspapertype <- "Broadsheet"

# Newspaper 6: The Independent
urc_theindependent_df <- data.frame()
for (i in 1:4) {
  file_name <- paste0("urc_theindependent_raw", i, ".docx")
  urc_doc <- lnt_read(x = file_name)
  meta_df <- urc_doc@meta
  articles_df <- urc_doc@articles
  doc_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
  urc_theindependent_df <- rbind(urc_theindependent_df, doc_df)
}
urc_theindependent_df <- distinct(urc_theindependent_df, Headline, .keep_all = TRUE)
urc_theindependent_df <- distinct(urc_theindependent_df, Article, .keep_all = TRUE)
urc_theindependent_df$Newspaper <- "The Independent"
urc_theindependent_df$Newspapertype <- "Broadsheet"

# Newspaper 7: The Sun
urc_thesun <- lnt_read(x = "urc_thesun_raw.docx")
meta_df <- urc_thesun@meta
articles_df <- urc_thesun@articles
urc_thesun_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
urc_thesun_df <- distinct(urc_thesun_df, Headline, .keep_all = TRUE)
urc_thesun_df <- distinct(urc_thesun_df, Article, .keep_all = TRUE)
urc_thesun_df$Newspaper <- "The Sun"
urc_thesun_df$Newspapertype <- "Tabloid"

# Newspaper 8: The Times
urc_thetimes_df <- data.frame()
for (i in 1:2) {
  file_name <- paste0("urc_thetimes_raw", i, ".docx")
  urc_doc <- lnt_read(x = file_name)
  meta_df <- urc_doc@meta
  articles_df <- urc_doc@articles
  doc_df <- merge(meta_df, articles_df, by = "ID", all = TRUE)
  urc_thetimes_df <- rbind(urc_thetimes_df, doc_df)
}
urc_thetimes_df <- distinct(urc_thetimes_df, Headline, .keep_all = TRUE)
urc_thetimes_df <- distinct(urc_thetimes_df, Article, .keep_all = TRUE)
urc_thetimes_df$Newspaper <- "The Times"
urc_thetimes_df$Newspapertype <- "Broadsheet"

# Combine all 8 newspaper outlets
urc_df <- rbind(urc_dailymail_df, urc_dailyexpress_df, urc_dailymirror_df, urc_thesun_df, urc_theguardian_df, 
                urc_theindependent_df, urc_dailytelegraph_df, urc_thetimes_df)
urc_df <- select(urc_df, -Section, -Edition, -Graphic)
urc_df$ID <- 1:nrow(urc_df)
urc_df <- urc_df[complete.cases(urc_df$Date), ]

# Save the dataframe
save(urc_df, file = "urc_articles.RData")
