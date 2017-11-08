#Load in Libraries#
install.packages("tidytext")
install.packages("monkeylearn")
library(tidytext)
library(gutenbergr)
library(monkeylearn)
library(tidyverse)
#Playing around with a book ID 14169
x <- gutenberg_download(c(14169), meta_fields = "title")
y <- x %>%
  unnest_tokens(paragraph, text, token = "paragraphs") %>%
  summarize(whole_text = paste(paragraph, collapse = " "))
chapters <- strsplit(y$whole_text, "[Cc]hapter")[[1]]
ychap <- tibble::tibble(
  chapter = 1:length(chapters),
  text = chapters
)
#Monkey Learn can only handle chapters that are less then 50000 bytes
all(nchar(ychap$text, type = "bytes") < 50000)
#Gets all Entities from Text
entities <- monkeylearn_extract(request = ychap$text,
                                key = "214be92d9ff78e77110c9e68d86898a34e713714",
                                extractor_id = "ex_isnnZRbS",
                                verbose = TRUE)
entit <- entities %>%
  group_by(entity, tag) %>%
  summarize(n_occurences = n()) %>%
  arrange(desc(n_occurences)) %>%
  knitr::kable()
entit
#Finds Key Words in Text
keywords <- monkeylearn_extract(request = ychap$text,
                                key = "214be92d9ff78e77110c9e68d86898a34e713714",
                                extractor_id = "ex_y7BPYzNG",
                                params = list(max_keywords = 5))
words <- keywords %>%
  group_by(keyword) %>%
  summarize(n_occurences = sum(count)) %>%
  arrange(desc(n_occurences)) %>%
  filter(n_occurences > 5) %>%
  knitr::kable()
words
#Finds Key Topics
topics <- monkeylearn_classify(ychap$text,
                               key = "214be92d9ff78e77110c9e68d86898a34e713714",
                               classifier_id = "cl_5icAVzKR")
top <- topics %>%
  group_by(label) %>%
  summarize(n_occurences = n()) %>%
  filter(n_occurences > 1) %>%
  arrange(desc(n_occurences)) %>%
  knitr::kable()
top