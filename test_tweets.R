source('myfunctions.R')

create_token(
  app = "HydrogenMobility",
  consumer_key = "S2ZekeyafDX1UiPVASkbcydD9",
  consumer_secret = "Rppk0P13iVyw58e7PvLb2Eh8mb2RwLlnDW2lsXV14EvUTvy8AY",
  access_token = "957197191394856961-CYyVWpBD9AxbKiNEI8dKxMFxkTyF65F",
  access_secret = "bLvvlJHiUMB2hOATPSGJGcHJ3gG1jLqtWEqa17uuGQMIv")

hydrogen_tweets <- search_tweets(q = "greenhydrogen OR hydrogencar 
                    OR fuelcell OR hydrogenenergy 
                    OR hydrogenplant OR hydrogenfuel",
                                 include_rts = FALSE, retryonratelimit = TRUE)

head(hydrogen_tweets)
nrow(hydrogen_tweets)

## export the results to avoid scraping again
rtweet::write_as_csv(hydrogen_tweets, "hydrogen_tweets.csv", fileEncoding = "UTF-8")

## wordclouds of #words
wordcloud(tagcounts_sorted[0:200,1], 
          tagcounts_sorted[0:200,2], 
          max.words=100,
          random.order = FALSE, 
          random.color = FALSE, 
          colors = "cornflowerblue")

## frequency of counted #words
barplot(tagcounts_sorted[1:20,]$Freq, las = 2, 
        names.arg = tagcounts_sorted[1:20,]$htags_list, 
        cex.names = 0.8,
        col ="deepskyblue3", border="white",
        main ="Most frequent #words",
        ylab = "Frequency",
        ylim = range(pretty(c(0, tagcounts_sorted[1:20,]$Freq))))


## copy the data for sentiment analysis
hydrogen_sent = hydrogen_tweets %>% dplyr::select(screen_name, text)
head(hydrogen_sent) 

hydrogen_sent$stripped_text <- gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", hydrogen_sent$text)
## hydrogen_sent$stripped_text <- gsub("@\\S+", "", hydrogen_sent$text)
head(hydrogen_sent)

## use unnest_tokens() to split column "stripped_text" into tokens, 
## flattening the table into one-token-per-row.
hydrogen_sent_stem <- hydrogen_sent %>% 
  dplyr::select(stripped_text) %>% 
  tidytext::unnest_tokens(word, stripped_text)
head(hydrogen_sent_stem)
nrow(hydrogen_sent_stem)


cleaned_hydrogen_sent <- hydrogen_sent_stem %>% anti_join(stop_words)
head(cleaned_hydrogen_sent)
# head(hydrogen_tweets$text)


## Top n words in all the tweets
cleaned_hydrogen_sent %>%
  count(word, sort=TRUE) %>%
  top_n(50) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) + geom_col() + xlab(NULL) +
  coord_flip() + theme_classic() +
  labs(x = "Count",
       y = "Unique words",
       title = "Unique word counts found in #greenhydrogen tweets")

cleaned_hydrogen_sent %>%
  count(word, sort=TRUE) %>%
  top_n(30) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(x=word, y=n)) + geom_col() + xlab(NULL) +
  coord_flip() + theme_classic() +
  labs(x = "Count",
       y = "Unique words",
       title = "Unique word counts found in texts of #greenhydrogen tweets")


## bing sentiment analysis
bing_hydrogen <- cleaned_hydrogen_sent %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()

bing_hydrogen

## plotting the results
bing_hydrogen %>% group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales="free_y") +
  labs(title="Tweets about green hydrogen",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()


## creating a column "score"--> 
## 1 for positive words and -1 for negative words
hydrogen_sent_score = bing_hydrogen %>%
  mutate(
    score = case_when(
      sentiment == 'negative' ~ n*(-1),
      sentiment == 'positive' ~ n*1
      )
    )

## calculating the total score
sent_total_score = case_when(
  nrow(hydrogen_sent_score) == 0 ~ 0, ## if there's no word -> score is 0
  nrow(hydrogen_sent_score) >0 ~ sum(hydrogen_sent_score$score) ## otherwise sum up scores
)

## also keeping track of tweets that contain no word from the bing list
zero_type = case_when(
  nrow(hydrogen_sent_score)==0 ~ "Type 1", ## "Type 1" -> no word
  nrow(hydrogen_sent_score)>0 ~ "Type 2" ## "Type 2" -> with words
)
list(score = sent_total_score, type = zero_type, hydrogen_sent_score)

## plotting the result
ggplot(hydrogen_sent_score, aes(x=score, fill=..count..)) +
  geom_histogram(bins=20, alpha=.6) +
  scale_fill_gradient("Count",low="lightseagreen",high="steelblue") +
  labs(title = "Total score of sentiment analysis")
