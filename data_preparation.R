# install.packages(c("tidyverse", "janitor"))

## Loading the libraries and the dataset

library(tidyverse)
library(janitor)

raw_df <- read_csv("Medical Tourism DataSet.csv")

## Inspecting the data

head(raw_df)
glimpse(raw_df)

## Cleaning the data

cleaned_data <- raw_df %>%
  clean_names() %>%
  mutate(cited_by = replace_na(cited_by, 0)) %>%
  select(
    authors,
    title,
    year,
    source_title,
    document_type,
    cited_by,
    affiliations,
    author_keywords, # We choose this over 'index_keywords'
    doi,
    link
  )

glimpse(cleaned_data)

## Preparing the data for analysis
## Creating a dataset for the authors and their publication count and then keywords

author_counts <- cleaned_data %>%
  filter(!is.na(authors)) %>%
  mutate(authors = str_replace_all(authors, ",", ";")) %>%
  separate_rows(authors, sep = ";") %>%
  mutate(authors = str_trim(authors)) %>%
  filter(authors != "[No author name available]") %>%
  count(authors, name = "publication_count", sort = TRUE)

# Viewing the top 10 authors
head(author_counts, 10)

keyword_counts <- cleaned_data %>%
  filter(!is.na(author_keywords)) %>%
  separate_rows(author_keywords, sep = ";") %>%
  mutate(author_keywords = str_to_lower(author_keywords)) %>%
  mutate(author_keywords = str_replace_all(author_keywords, "health care", "healthcare")) %>%
  mutate(author_keywords = str_trim(author_keywords)) %>%
  count(author_keywords, name = "frequency", sort = TRUE)

# Viewing the top 20 keywords
head(keyword_counts, 20)

## Saving a copy of the data
write_csv(cleaned_data, "cleaned_medical_data.csv")
write_csv(author_counts, "author_counts.csv")
write_csv(keyword_counts, "keyword_counts.csv")

## Analysing journals

journal_counts <- cleaned_data %>%
  filter(!is.na(source_title)) %>%
  count(source_title, name = "publication_count", sort = TRUE)

# Viewing the top 10 journals
head(journal_counts, 10)

write_csv(journal_counts, "journal_counts.csv")
