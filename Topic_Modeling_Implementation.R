install.packages("rvest")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("xml2")
install.packages("stringr")
install.packages("tm")
install.packages("textclean")
install.packages("tokenizers")
install.packages("textstem")
install.packages("topicmodels")


library(rvest)
library(tidyverse)
library(dplyr)
library(xml2)
library(stringr)
library(tm)
library(textclean)
library(tokenizers)
library(textstem)  
library(topicmodels)




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


View(player_data)


cleaned_player_data <- player_data %>%
  mutate(
    Cleaned_Description = sapply(Description, function(x) {
      x <- str_replace_all(x, "[^[:alnum:][:space:]]", "") %>% str_squish()  
      x <- str_replace_all(x, "\\d+", "") 
      x <- tolower(x)
      x <- removeWords(x, stopwords("en"))
      x <- lemmatize_strings(x)  
      x <- replace_contraction(x, method = "text")
      x <- replace_emoji(x, method = "text")
      return(x)
    }),
    Cleaned_Name = sapply(Names, function(x) {
      x <- str_replace_all(x, "[^[:alnum:][:space:]]", "") %>% str_squish()  
      x <- str_replace_all(x, "\\d+", "")  
      x <- tolower(x)
      x <- removeWords(x, stopwords("en"))
      x <- lemmatize_strings(x)  
      x <- replace_contraction(x, method = "text")
      x <- replace_emoji(x, method = "text")
      return(x)
    })
  )


View(cleaned_player_data)


write.csv(cleaned_player_data, "1player_data_cleaned.csv", row.names = FALSE)



corpus_description <- Corpus(VectorSource(cleaned_player_data$Cleaned_Description))


corpus_description <- tm_map(corpus_description, content_transformer(tolower))  
corpus_description <- tm_map(corpus_description, removePunctuation) 
corpus_description <- tm_map(corpus_description, removeNumbers) 
corpus_description <- tm_map(corpus_description, removeWords, stopwords("en"))  
corpus_description <- tm_map(corpus_description, stripWhitespace)  
corpus_description <- tm_map(corpus_description, content_transformer(replace_contraction))  


corpus_name <- Corpus(VectorSource(cleaned_player_data$Cleaned_Name))

corpus_name <- tm_map(corpus_name, content_transformer(tolower))  
corpus_name <- tm_map(corpus_name, removePunctuation)  
corpus_name <- tm_map(corpus_name, removeNumbers)  
corpus_name <- tm_map(corpus_name, removeWords, stopwords("en"))  
corpus_name <- tm_map(corpus_name, stripWhitespace)  
corpus_name <- tm_map(corpus_name, content_transformer(replace_contraction))  


updated_description <- sapply(corpus_description, as.character)
updated_name <- sapply(corpus_name, as.character)


updated_data <- data.frame(
  updated_name = updated_name,
  updated_description = updated_description
)


write.csv(updated_data, "1player_data_updated.csv", row.names = FALSE)



dtm_description <- DocumentTermMatrix(corpus_description)
dtm_description
inspect(dtm_description)

dtm_name <- DocumentTermMatrix(corpus_name)
dtm_name
inspect(dtm_name)

dtm_description <- removeSparseTerms(dtm_description, 0.99)  
dtm_name <- removeSparseTerms(dtm_name, 0.99)  



dtm_description_matrix <- as.matrix(dtm_description)
dtm_description_matrix
View(dtm_description_matrix)

dtm_name_matrix <- as.matrix(dtm_name)
dtm_name_matrix
View(dtm_name_matrix)


tfidf_description <- weightTfIdf(dtm_description)
tfidf_description
tfidf_description_matrix_view <- as.matrix(tfidf_description)
tfidf_description_matrix_view
View(tfidf_description_matrix_view)

tfidf_name <- weightTfIdf(dtm_name)
tfidf_name
tfidf_name_matrix_view <- as.matrix(tfidf_name)
tfidf_name_matrix_view
View(tfidf_name_matrix_view)


num_topics_dsc <- 5  
lda_model_description <- LDA(dtm_description, k = num_topics_dsc, control = list(seed = 1234))
lda_model_description

num_topics_names <- 6
lda_model_name <- LDA(dtm_name, k = num_topics_names, control = list(seed = 1234))
lda_model_name


topics_description <- terms(lda_model_description, 10)  
print("Top terms per topic (Description):")
print(topics_description)
topics_description_matrix_view <- as.matrix(topics_description)
View(topics_description_matrix_view)


topics_name <- terms(lda_model_name, 10)  
print("Top terms per topic (Name):")
print(topics_name)
topic_name_matrix_view <- as.matrix(topics_name)
View(topic_name_matrix_view)


doc_topics_description <- posterior(lda_model_description)$topics
print("Document topic proportions (Description):")
print(doc_topics_description)
doc_topics_description_matrix_view <- as.matrix(doc_topics_description)
View(doc_topics_description_matrix_view)


doc_topics_name <- posterior(lda_model_name)$topics
print("Document topic proportions (Name):")
print(doc_topics_name)
doc_topics_name_matrix_view <- as.matrix(doc_topics_name)
View(doc_topics_name_matrix_view)



write.csv(topics_description, "1lda_topics_description.csv", row.names = TRUE)
write.csv(topics_name, "1lda_topics_name.csv", row.names = TRUE)
write.csv(doc_topics_description, "1doc_topic_proportions_description.csv", row.names = TRUE)
write.csv(doc_topics_name, "1doc_topic_proportions_name.csv", row.names = TRUE)


