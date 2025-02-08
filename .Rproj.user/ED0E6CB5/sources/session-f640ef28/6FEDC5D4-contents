install.packages("rvest")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("xml2")
install.packages("stringr")
install.packages("tm")
install.packages("textclean")
install.packages("tokenizers")
install.packages("textstem")

library(rvest)
library(tidyverse)
library(dplyr)
library(xml2)
library(stringr)
library(tm)
library(textclean)
library(tokenizers)
library(textstem)


website1 <- "https://www.90min.com/posts/transfers-worst-signings-football-history-hazard-coutinho-dembele"
read_website1 <- read_html(website1)
names1 <- html_nodes(read_website1, ".tagStyle_1igopqi-o_O-style_48hmcm-o_O-titleStyle_k8egye") %>% html_text(trim = TRUE)
description1 <- html_nodes(read_website1, ".tagStyle_1igopqi-o_O-style_48hmcm-o_O-titleStyle_k8egye~ .tagStyle_z4kqwb-o_O-style_1tcxgp3-o_O-style_1pinbx1-o_O-style_48hmcm") %>% html_text(trim = TRUE)

website2 <- "https://kingsoccertips.com/most-expensive-football-player-transfers/"
read_website2 <- read_html(website2)
names2 <- html_nodes(read_website2, ".main-content-area h3") %>% html_text(trim = TRUE)
description2 <- html_nodes(read_website2, "h3+ p") %>% html_text(trim = TRUE)

website3 <- "https://www.sportco.io/article/worst-transfers-football-548605"
read_website3 <- read_html(website3)
names3 <- html_nodes(read_website3, "p:nth-child(54) strong , p:nth-child(50) strong , p:nth-child(44) strong , p:nth-child(38) strong , p:nth-child(32) strong , p:nth-child(28) strong , p:nth-child(24) strong , p:nth-child(17) strong , p:nth-child(12) strong , p:nth-child(6) strong") %>% html_text(trim = TRUE)
description3 <- html_nodes(read_website3, ".image+ p") %>% html_text(trim = TRUE)


player_data <- data.frame(
  Names = c(names1, names2, names3),
  Description = c(description1, description2, description3)
)


write.csv(player_data, "player_data_raw.csv", row.names = FALSE)

View(player_data)


cleaned_description <- sapply(player_data$Description, function(x) {
  str_replace_all(x, "[^[:alnum:][:space:]]", "") %>% str_squish()
})

tokenized_data <- lapply(cleaned_description, function(x) {
  unlist(tokenize_words(x))
})


tokenized_data_collapsed <- sapply(tokenized_data, function(x) paste(x, collapse = " "))

normalized_data <- lapply(tokenized_data, function(x) {
  tolower(x)
})


normalized_data_collapsed <- sapply(normalized_data, function(x) paste(x, collapse = " "))

no_stopwords <- lapply(normalized_data, function(x) {
  setdiff(x, stopwords("en"))
})


no_stopwords_collapsed <- sapply(no_stopwords, function(x) paste(x, collapse = " "))

stemmed_data <- lapply(no_stopwords, function(x) {
  stem_words(x)
})


stemmed_collapsed <- sapply(stemmed_data, function(x) paste(x, collapse = " "))

lemmatized_data <- lapply(no_stopwords, function(x) {
  lemmatize_words(x)
})


lemmatized_collapsed <- sapply(lemmatized_data, function(x) paste(x, collapse = " "))

expanded_data <- lapply(cleaned_description, function(x) {
  replace_contraction(x)
})


expanded_collapsed <- sapply(expanded_data, function(x) paste(x, collapse = " "))

emoji_handled_data <- lapply(expanded_data, function(x) {
  replace_emoji(x)
})


emoji_handled_collapsed <- sapply(emoji_handled_data, function(x) paste(x, collapse = " "))


data_preprocessing_summary <- data.frame(
  Original = player_data$Description,
  Cleaned = cleaned_description,
  Tokenized = tokenized_data_collapsed,
  Normalized = normalized_data_collapsed,
  NoStopwords = no_stopwords_collapsed,
  Stemmed = stemmed_collapsed,
  Lemmatized = lemmatized_collapsed,
  Expanded = expanded_collapsed,
  EmojiHandled = emoji_handled_collapsed,
  stringsAsFactors = FALSE
)


View(data_preprocessing_summary)


write.csv(data_preprocessing_summary, "data_preprocessing_summary.csv", row.names = FALSE)

