## Case4 Voice of Customers
## Author: Shin Adachi

library(tidyverse)
library(jsonlite)
library(quanteda)
library(ggplot2)

setwd("/Users/shinsukeadachi/Desktop/QTM385-3/case4")
fname <- "Toys_and_Games_5.json"

con <- file(fname, "r")
mydata <- list()
i <- 1
while (T) {
  line <- readLines(con, 1)
  line <- jsonlite::fromJSON(line)
  mydata[[i]] <- data.frame(
    overall = line$overall,
    review_time = line$reviewTime,
    text = line$reviewText,
    sku = line$asin,
    reviewer_id = line$reviewerID
  )
  i <- i + 1
  if (i > 2000) {
    break
  }
}


line

df <- do.call(rbind, mydata) %>%
  as_tibble()

df <- df %>%
  mutate(
    date = lubridate::mdy(review_time)
  ) %>%
  select(-review_time)


df

df %>%
  group_by(overall) %>%
  summarize(n=n())

df

en_stops <- stopwords()

my_tokens <-
  tokens(
    df$text,
    remove_punct = T,
    remove_numbers = T,
    remove_symbols = T,
    remove_url = T,
    remove_separators = T
  )

my_dfm <- dfm(my_tokens, tolower = T, remove = en_stops) %>%
  dfm_wordstem()

my_dfm

topfeatures(my_dfm)

gdf <- data.frame(
  date = df$date,
  love = as.vector(my_dfm[,"love"]),
  total = ntoken(my_tokens)
) %>%
  group_by(date) %>%
  summarize(
    love = sum(love),
    total = sum(total)
  ) %>%
  mutate(
    pct = love / total
  )

gdf %>%
  ggplot(aes(x=date, y=pct)) +
  geom_line() +
  geom_smooth(span=0.05)
  
library(quanteda.textmodels)
library(quanteda.textplots)

df

tm1 <- textmodel_nb(x=my_dfm, y=as.integer(df$overall >= 4))
df$sentiment <- predict(tm1, my_dfm)

df %>%
  group_by(sku) %>%
  summarize(
    pos = sum(overall >= 4),
    tot = n()
  ) %>%
  mutate(
    pct = pos / tot
  ) %>%
  arrange(pct) 

(df %>%
  filter(sku == "0857260979"))$text %>%
  tokens() %>%
  dfm(remove=en_stops) %>%
  topfeatures()

####

tsk <- quanteda.textstats::textstat_keyness(my_dfm, df$overall == 1)
tsp <- textplot_keyness(tsk)
tsp

df %>%
  group_by(sku) %>%
  summarize(mean=mean(overall)) 

df %>%
  length(sku)

