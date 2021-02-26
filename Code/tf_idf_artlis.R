library(tidyverse)
library(tidytext)
library(ggplot2)
library(tm)
library(stm)
library(quanteda)
library("topicmodels")
library("tm")

#setwd("C:/Users/ajalvero/Downloads/")
setwd("C:/Users/ajalvero/Desktop/Quentin/")
#dat <- read.csv("med_income-Copy1.csv")
#dat <- read.csv("telos_artlis_cleaned.csv")
#dat <- read.csv("subset_kmeans.csv")
dat <- read.csv("ARTLIS_Final.csv")


dat$RANDOM <-sample(c(0,1), replace=TRUE, size=nrow(dat))
dat$Gender <- as.factor(dat$Gender)
dat$kmeans <- as.factor(dat$kmeans)

dat$Gender <- sub("^$", "Prefer not to say/Other", dat$Gender)
dat$Gender <- sub("Other (you may elaborate if you wish):", 
                  "Prefer not to say/Other", dat$Gender)

dat <- dat %>% 
  filter(Gender == "Male" || Gender == "Female")

#Replace periods with period + whitespace. This catches sentences that are not separated by
#whitespace and period.
dat$Challenges_Reading <- as.character(dat$Challenges_Reading)
dat$Challenges_Reading <- gsub(".",". ", dat$Challenges_Reading, fixed = TRUE)
dat$Challenges_Reading <- gsub(",",", ", dat$Challenges_Reading, fixed = TRUE)
dat$Challenges_Reading <- gsub("[[:punct:]]", "", dat$Challenges_Reading)
dat$Challenges_Reading <- tolower(dat$Challenges_Reading)
dat$Challenges_Reading <- removeWords(dat$Challenges_Reading, 
                                      c(stopwords("english"), 
                                        "the", "students", "i", "reading",
                                        "read", "sife", "also"))

dat$Challenges_Writing <- as.character(dat$Challenges_Writing)
dat$Challenges_Writing <- gsub(".",". ", dat$Challenges_Writing, fixed = TRUE)
dat$Challenges_Writing <- gsub(",",", ", dat$Challenges_Writing, fixed = TRUE)
dat$Challenges_Writing <- gsub("[[:punct:]]", "", dat$Challenges_Writing)
dat$Challenges_Writing <- tolower(dat$Challenges_Writing)
dat$Challenges_Writing <- removeWords(dat$Challenges_Writing, 
                                      c(stopwords("english"), 
                                        "the", "students", "i"))

dat$Responses <- as.character(dat$Text)
dat$Responses <- gsub(".",". ", dat$Responses, fixed = TRUE)
dat$Responses <- gsub(",",", ", dat$Responses, fixed = TRUE)
dat$Responses <- gsub("[[:punct:]]", "", dat$Responses)
dat$Responses <- gsub('[[:digit:]]+', '', dat$Responses)
dat$Responses <- tolower(dat$Responses)
dat$Responses <- removeWords(dat$Responses, 
                                      c(stopwords("english"), 
                                        "the", "students", "i"))

#grep("I_WAS_NOT_ASCII", iconv(dat$Challenges_Writing, "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))

write.csv(dat, "telos_artlis_cleaned.csv")

#NOTES:
#Text: "Challenges_Reading"
#Gender: "Gender"
#Race: "Race"
#Ed. level: "{"ImportId":"QID44"}"
#File: telos_quentin.csv

#Unnest tokens (aka split documents into tokens. Automatically lowercases)
##Retrieve word/token counts by group using verbs
by_bucket <- dat %>%
  group_by() %>%
  unnest_tokens(word, Challenges_Reading)%>%
  count(Gender, word, sort = TRUE)

by_bucket <- by_bucket %>% 
  group_by(word)

#Sums number of words by income bucket
total_words_by_group <- by_bucket %>% 
  group_by(Gender) %>% 
  summarize(total = sum(n))

by_bucket <- left_join(by_bucket, total_words_by_group)
by_bucket

#Generate tf-idf by group
by_bucket <- by_bucket %>%
  bind_tf_idf(word, Gender, n)

male <- by_bucket %>%
  filter(Gender == "Male") %>%
  arrange(desc(tf_idf))

#Generate plot
# Is not ordering bars
by_bucket %>%
  arrange(desc(tf_idf)) %>%
  ungroup(word) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(Gender) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = Gender)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~Gender, ncol = 2, scales = "free") +
  coord_flip()


#Unnest tokens (aka split documents into tokens. Automatically lowercases)
##Retrieve word/token counts by group
by_bucket <- dat %>%
  group_by(fifth) %>%
  unnest_tokens(word, Prompt.4.Essay) %>%
  count(fifth, word, sort = TRUE)

#Sums number of words by income bucket
total_words_by_group <- by_bucket %>% 
  group_by(fifth) %>% 
  summarize(total = sum(n))

by_bucket <- left_join(by_bucket, total_words_by_group)
by_bucket

#Generate tf-idf by group
by_bucket <- by_bucket %>%
  bind_tf_idf(word, fifth, n)
by_bucket

#Generate plot
# Is not ordering bars
by_bucket %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(fifth) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = fifth)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~fifth, ncol = 2, scales = "free") +
  coord_flip()



################################################################################
# STM
################################################################################

#Processes data
artlis_dfm <- dfm(as.character(dat$Responses), 
                    remove =  stopwords("english"),remove_punct = TRUE,
                    stem = TRUE, verbose = FALSE, tolower = TRUE)

artlis_processed <- convert(artlis_dfm, to = "stm", 
                                         docvars = data.frame(dat))

#Lines below prepare corpus for processing and analysis
artlis_out <- prepDocuments(artlis_processed$documents,
                             artlis_processed$vocab, 
                             artlis_processed$meta)
artlis_docs <- artlis_out$documents
artlis_vocab <- artlis_out$vocab
artlis_meta <- artlis_out$meta

#Actual STM models in each line below. K was derived by first chunk of code.
artlis_Fit <- stm(artlis_docs, artlis_vocab, K = 10, 
                   data = artlis_meta, max.em.its = 9000, 
                   init.type = "LDA", verbose = TRUE)

#Line below should spit out topics with words
labelTopics(artlis_Fit)

plot(topicCorr(artlis_Fit))

gamma_artlis <- tidy(artlis_Fit, matrix = "gamma")
write.csv(gamma_artlis, "gamma_artlis.csv")

topic_words <- labelTopics(artlis_Fit)
