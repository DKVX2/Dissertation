# Main Statistical Analysis for SRC/URC Data

# load libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(lmtest)
library(nnet)
library(stargazer)

# load data ---------------------------------------------------------------
src_df <- read.csv("src_analysis_df.csv")
urc_df <- read.csv("urc_analysis_df.csv")

# descriptive statistics of articles --------------------------------------

# plot 1: number of articles per newspaper
# src plot
article_counts_src <- src_df %>%
  group_by(Newspaper) %>%
  summarise(count = n()) # article count by newspaper 

figure1<- ggplot(article_counts_src, aes(x = Newspaper, y = count)) +
  geom_bar(stat = "identity", fill = "darkturquoise") +
  geom_text(aes(label = count), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Figure 1: Distribution of Newspaper Articles on the SRC",
       x = "Newspaper Outlet",
       y = "Number of Articles") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
ggsave("figure1.png", figure1)

# urc plot
article_counts_urc <- urc_df %>%
  group_by(Newspaper) %>%
  summarise(count = n()) # article count by newspaper 

figure2 <- ggplot(article_counts_urc, aes(x = Newspaper, y = count)) +
  geom_bar(stat = "identity", fill = "darkturquoise") +
  geom_text(aes(label = count), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Figure 2: Distribution of Newspaper Articles on the URC",
       x = "Newspaper Outlet",
       y = "Number of Articles") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
ggsave("figure2.png", figure2, dpi = 1000)


# plot 2: number of articles per newspaper type
articletype_counts_src <- src_df %>%
  group_by(Newspapertype) %>%
  summarise(count = n()) # article count by newspaper type for SRC

articletype_counts_urc <- urc_df %>%
  group_by(Newspapertype) %>%
  summarise(count = n()) # article count by newspaper type for URC

articletype_counts_src$Source <- "SRC"
articletype_counts_urc$Source <- "URC"
articletype_counts <- rbind(articletype_counts_src, articletype_counts_urc)

figure3 <- ggplot(articletype_counts, aes(x = Newspapertype, y = count, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = count), vjust = -0.5, color = "black", size = 3, position = position_dodge(width = 0.9)) +
  labs(title = "Figure 3: Distribution of Newspaper Articles per Type on the SRC and URC",
       x = "Newspaper Type",
       y = "Number of Articles") +
  facet_wrap(~ Source) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("figure3.png", figure3, dpi = 1000)


# plot 3: number of articles per political orientation of newspaper
articlepo_counts_src <- src_df %>%
  group_by(Rightwing) %>%
  summarise(count = n()) # article count by political orientation for SRC

articlepo_counts_urc <- urc_df %>%
  group_by(Rightwing) %>%
  summarise(count = n()) # article count by political orientation for URC

articlepo_counts_src$Source <- "SRC"
articlepo_counts_urc$Source <- "URC"
articlepo_counts <- rbind(articlepo_counts_src, articlepo_counts_urc)
articlepo_counts$Polori <- ifelse(articlepo_counts$Rightwing == 1, "Right Wing", "Left Wing")

figure4 <- ggplot(articlepo_counts, aes(x = Polori, y = count, fill = Source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = count), vjust = -0.5, color = "black", size = 3, position = position_dodge(width = 0.9)) +
  labs(title = "Figure 4: Distribution of Articles by Political Orientation of Newspaper Outlets",
       x = "Political Orientation",
       y = "Number of Articles") +
  facet_wrap(~ Source) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("figure4.png", figure4, dpi = 1000)


# plot 4: number of articles of SRC/URC over time
# src plot
articledate_counts <- src_df %>%
  group_by(year_month) %>%
  summarise(count = n()) # article count by date
articledate_counts$year_month <- as.Date(paste(articledate_counts$year_month, "-01", sep = ""), format = "%Y-%m-%d")

figure5 <- ggplot(articledate_counts, aes(x = year_month, y = count)) +
  geom_line() +
  labs(title = "Figure 5: Number of Articles on the SRC Over Time",
       x = "Year-Month",
       y = "Number of Articles") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  geom_vline(xintercept = as.Date("2015-09-01"), linetype = "dashed", color = "red") +
  geom_text(data = filter(articledate_counts, year_month == as.Date("2015-09-01")),
            aes(x = year_month, y = count, label = paste("Sept 2015:", count)),
            vjust = 1, hjust = -0.2, color = "red") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
ggsave("figure5.png", figure5, dpi = 1000)

# urc plot
articledate_count_urc <- urc_df %>%
  group_by(year_month) %>%
  summarise(count = n()) # article count by date
articledate_count_urc$year_month <- as.Date(paste(articledate_count_urc$year_month, "-01", sep = ""), format = "%Y-%m-%d")

figure6 <- ggplot(articledate_count_urc, aes(x = year_month, y = count)) +
  geom_line() +
  labs(title = "Figure 6: Number of Articles on the URC Over Time",
       x = "Year-Month",
       y = "Number of Articles") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  geom_vline(xintercept = as.Date("2022-03-01"), linetype = "dashed", color = "red") +
  geom_text(data = filter(articledate_count_urc, year_month == as.Date("2022-03-01")),
            aes(x = year_month, y = count, label = paste("March 2022:", count)),
            vjust = 1, hjust = -0.2, color = "red") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))
ggsave("figure6.png", figure6, dpi = 1000)


# descriptive statistics of classifications -------------------------------

# plot on distribution of categories 
category_p1 <- ggplot(src_df %>%
               count(Category) %>%
               mutate(percentage = n / sum(n) * 100),
             aes(x = "", y = n, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Category, "\n", round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "SRC Articles") +
  theme_void() +
  theme(legend.position = "none")

category_p2 <- ggplot(urc_df %>%
                        count(Category) %>%
                        mutate(percentage = n / sum(n) * 100),
                      aes(x = "", y = n, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Category, "\n", round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "URC Articles") +
  theme_void() +
  theme(legend.position = "none")

figure7 <- category_p1 + category_p2 + plot_layout(ncol = 2) + 
  plot_annotation(title = "Figure 7: Article Category Distributions") &
  theme(plot.title = element_text(hjust = 0.5))
ggsave("figure7.png", figure7)

# number of solidarity articles in the datasets
solidarity_counts_src <- src_df %>%
  group_by(Solidarity) %>%
  summarise(count = n()) %>%
  mutate(Source = "SRC")

solidarity_counts_urc <- urc_df %>%
  group_by(Solidarity) %>%
  summarise(count = n()) %>%
  mutate(Source = "URC")

solidarity_counts <- rbind(solidarity_counts_src, solidarity_counts_urc)

figure8 <- ggplot(solidarity_counts, aes(x = factor(Solidarity), y = count, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            color = "black") +
  labs(title = "Figure 8: Number of Articles Showing Solidarity",
       x = "Solidarity Measure",
       y = "Number of Articles",
       fill = "Source") +
  scale_x_discrete(labels = c("0" = "Shows No Solidarity", "1" = "Shows Solidarity")) +
  facet_wrap(~ Source) +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("figure8.png", figure8)

# solidarity/no solidarity articles in each category
src_percentage_data <- src_df %>%
  group_by(Category) %>%
  summarise(
    solidarity_count = sum(Solidarity == 1),
    non_solidarity_count = sum(Solidarity == 0),
    total_count = n()
  ) %>%
  mutate(
    solidarity_percentage = (solidarity_count / total_count) * 100,
    non_solidarity_percentage = (non_solidarity_count / total_count) * 100
  ) %>%
  select(Category, solidarity_percentage, non_solidarity_percentage) %>%
  pivot_longer(cols = c("solidarity_percentage", "non_solidarity_percentage"),
               names_to = "Solidarity",
               values_to = "Percentage")

src_percentage_data$Solidarity <- recode(src_percentage_data$Solidarity,
                                     "solidarity_percentage" = "Shows Solidarity",
                                     "non_solidarity_percentage" = "Shows No Solidarity")

solcat_p1 <- ggplot(src_percentage_data, aes(x = Category, y = Percentage, fill = Solidarity)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_stack(vjust = 0.5), 
            color = "black") +
  labs(title = "SRC Articles",
       x = "Category",
       y = "Percentage of Articles",
       fill = "Article Type") +
  theme_minimal() +
  theme(legend.position = "none")

urc_percentage_data <- urc_df %>%
  group_by(Category) %>%
  summarise(
    solidarity_count = sum(Solidarity == 1),
    non_solidarity_count = sum(Solidarity == 0),
    total_count = n()
  ) %>%
  mutate(
    solidarity_percentage = (solidarity_count / total_count) * 100,
    non_solidarity_percentage = (non_solidarity_count / total_count) * 100
  ) %>%
  select(Category, solidarity_percentage, non_solidarity_percentage) %>%
  pivot_longer(cols = c("solidarity_percentage", "non_solidarity_percentage"),
               names_to = "Solidarity",
               values_to = "Percentage")

urc_percentage_data$Solidarity <- recode(urc_percentage_data$Solidarity,
                                         "solidarity_percentage" = "Shows Solidarity",
                                         "non_solidarity_percentage" = "Shows No Solidarity")

solcat_p2 <- ggplot(urc_percentage_data, aes(x = Category, y = Percentage, fill = Solidarity)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_stack(vjust = 0.5), 
            color = "black") +
  labs(title = "URC Articles",
       x = "Category",
       y = "Percentage of Articles",
       fill = "Article Type") +
  theme_minimal()

figure9 <- solcat_p1 + solcat_p2 + plot_layout(ncol = 2) + 
  plot_annotation(title = "Figure 9: Proportion of Articles Showing Solidarity by Category") &
  theme(plot.title = element_text(hjust = 0.5))
ggsave("figure9.png", figure9)


# Statistical Regression Analysis -----------------------------------------

# src regression analysis
src_df$Solidarity <- as.factor(src_df$Solidarity)
src_df$Rightwing <- as.factor(src_df$Rightwing)
src_df$Tabloid <- ifelse(src_df$Newspapertype == "Tabloid", 1, 0)
src_df$Tabloid <- as.factor(src_df$Tabloid)

src_tabloid_model <- glm(Solidarity ~ Tabloid + Category, data = src_df, family = binomial)
src_po_model <- glm(Solidarity ~ Rightwing + Category, data = src_df, family = binomial)

# urc regression analysis
urc_df$Solidarity <- as.factor(urc_df$Solidarity)
urc_df$Rightwing <- as.factor(urc_df$Rightwing)
urc_df$Tabloid <- ifelse(urc_df$Newspapertype == "Tabloid", 1, 0)
urc_df$Tabloid <- as.factor(urc_df$Tabloid)

urc_tabloid_model <- glm(Solidarity ~ Tabloid + Category, data = urc_df, family = binomial)
urc_po_model <- glm(Solidarity ~ Rightwing + Category, data = urc_df, family = binomial)

models <- list(src_tabloid_model, urc_tabloid_model, src_po_model, urc_po_model)

stargazer(models, title = "Table 7: SRC and URC Logit Model Results", align = TRUE, 
          column.labels = c("SRC Type Model ", "URC Type Model ", "SRC PO Model ", "URC PO Model "),
          covariate.labels = c("Tabloid", "Right-wing", "Category: Conditions", "Category: Migration", "Category: Policy"),
          dep.var.labels = " ", 
          dep.var.caption = "Dependent variable is presence of solidarity",
          out.header = TRUE, type = "html", out = "logitresults.html")
