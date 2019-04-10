#' ---
#' title: "Local Brewery Twitter Analysis"
#' author: "Tim Gordon"
#' date: "April 10th, 2019"
#' ---

#'##Introduction
#'The goal of this project is to see how two of the most popular breweries in the 
#'Cincinnati area distinguish themselves from one another by looking at how they 
#'market themselves and interact with customers on Twitter. From the Twitter data script,
#'I load the Tweets from the two CSV files I generated for both breweries.
#'
#'First I load the necessary packages to conduct analyses on the data
library(tidytext)
library(ggplot2)
library(dplyr)
library(rtweet)
library(cowplot)

options(warn = -1)
#'
#'##Analysis:
#'
#'I load in tweets from the separate package and assign to respective variable name
Madtree_tweets <- read.csv("MadTreetweetsUpdate.csv", stringsAsFactors = FALSE)
Rhinegeist_tweets <- read.csv("RhinegeistTweetsUpdate.csv", stringsAsFactors = FALSE)

#'How many tweets have we collected for each unique official twitter account
table(Madtree_tweets$screen_name)
table(Rhinegeist_tweets$screen_name)

#'Create time-series plot for each brewery. Filter by all tweets since 2016 for each account. 
#'Group by the account name of brewery. This is only relevant for Madtree's twitter activity as they have two separate official accounts. 
#'Each line is representative of a week so we can get a better overview of twitter activity over a 3 year period.
#'The twitter activity across the two breweries seem to be fairly consistent with about 1 tweet every other day. 
#'We can also observe peaks in the Winter months for Madtree and near early summer for Rhinegeist as the respective breweries increase twitter activity to advertise their respective anniversaries. 
#'MadTree's anniversary is Feb. 17,and Rhinegeist's is June 29. 
#'From both charts, we can see that MadTree has slightly more Twitter activity especially with their tweets divided between 2 accounts after the summer of 2017.
m_ts <- Madtree_tweets %>%
  filter(created_at > "2016-1-01") %>%
  group_by(screen_name) %>%
  ts_plot(,"weeks", trim = 1L) +
  labs(title = "MadTree Twitter",
       subtitle = "Weekly Timeline") +
  theme_light()

r_ts <- Rhinegeist_tweets %>%
  filter(created_at > "2016-1-01") %>%
  group_by(screen_name) %>%
  ts_plot(,"weeks", trim = 1L) +
  labs(title = "Rhinegeist Twitter",
       subtitle = "Weekly Timeline") +
  theme_light()

#'Text analysis Data Prep for Madtree. Remove any http from twitter text 
Madtree_tweets$stripped_text <- gsub("http.*","",  Madtree_tweets$text)
Madtree_tweets$stripped_text <- gsub("https.*","", Madtree_tweets$stripped_text)
head(Madtree_tweets$stripped_text)

#'Get stop works to remove redudant words. Stop words include words such as the,
#'an, a, and those.They are typically general words which lead to more specific words.
#'We want ot find the specific words to see how MadTree makes their customer 
#'interactions and marketing strategies unique on Twitter. After we unnest tokens
#'to identify words used across their tweets, we remove any stop words. 
data("stop_words")

Madtree_tidytext <- Madtree_tweets %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text) %>%
  anti_join(stop_words)

nrow(Madtree_tidytext)

#'Graph Madtree unique word counts. From this graph, we can see that MadTree typically 
#'uses words associated with a brewery such as drink, cans, and beers. They also use
#'a few words that are unique to its brand in Cincinnati such as their taproom, and drinktrees. 
#'The most unique wowrd is the emoji of beer glasses clanking each other. I thnik this
#'Twitter activity is what you would expect from a brewery but there is not enought unique words
#'used to sell people on their unique aspects. Many people from the Cincinnati area will tell you 
#'they go to Madtree for the community aspect of it especially for their garden 
#'which is open to all ages and allows people to bring their pets. These qualities 
#'are unique to Madtree between these two breweries, and I do not believe they are utilizing 
#'it well enough from looking at their Twitter feed. 
munique <- Madtree_tidytext %>%
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "MadTree Tweets",
       subtitle = "Unique Words Only")
munique

#'Text analysis Data Prep for Rhingeist. Remove any http from twitter text 
Rhinegeist_tweets$stripped_text <- gsub("http.*","",  Rhinegeist_tweets$text)
Rhinegeist_tweets$stripped_text <- gsub("https.*","", Rhinegeist_tweets$stripped_text)
head(Rhinegeist_tweets$stripped_text)

#'Get stop works to remove redudant words. Stop words include words such as the,
#'an, a, and those. They are typically general words which lead to more specific words.
#'We want ot find the specific words to see how MadTree makes their customer 
#'interactions and marketing strategies unique on Twitter. After we unnest tokens
#'to identify words used across their tweets, we remove any stop words. s
Rhinegeist_tidytext <- Rhinegeist_tweets %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text) %>%
  anti_join(stop_words)

nrow(Rhinegeist_tidytext)

#'Graph of Rhinegeist unique words used on Twitter. Rhinegeist seems to be more event
#'focused as they seem to have a more unified social media presence. With key words such as 
#'fb "Facebook", event, and specific time references. Rhingeist seems to better distinguish it's 
#'brand on social media as it shows it shows they are more event focused. 

runique <- Rhinegeist_tidytext %>%
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Rhinegeist Tweets",
       subtitle = "Unique Words Only")
runique
#'Word sort by count to identify which words used most commonly indicate which
#'emotion MadTree uses in tweets. 
Madtree_counts <- Madtree_tidytext %>% 
  count(word, sort = TRUE)
Madtree_counts

#'Sentiment analysis preparation. Bing assigns a value that usually indicates 
#'what emotion a certain word conveys. This value is being assigned to 
#'words MadTree used most frequently in tweets
bing <- get_sentiments("bing")

Madtree_counts <- Madtree_counts %>%
  inner_join(bing)
nrow(Madtree_counts)

#'Plot the sentiment value of most common words for Madtree twitter data. 
#'THis graph is slightly wrong as the context for some of the negative words are 
#'positive in this situation such as limited time beer, ice cold beer, and hang with 
#'friends. All the positive sentiment words are great, but it's very similar to Rhingeist
#'and shows that they are struggling to set their online presence apart from Rhinegeist.
m_sent <- Madtree_counts %>%
  filter(n > 15) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment",
       title = "Madtree Tweets Sentiment")
m_sent

#'Word sort by count to identify which words used most commonly indicate which
#'emotion Rhingeist uses in tweets. 
Rhinegeist_counts <- Rhinegeist_tidytext %>% 
  count(word, sort = TRUE)
Rhinegeist_counts

#'Sentiment analysis preparation. Bing assigns a value that usually indicates 
#'what emotion a certain word conveys. This value is being assigned to 
#'words MadTree used most frequently in tweets
Rhinegeist_counts <- Rhinegeist_counts %>%
  inner_join(bing)
nrow(Rhinegeist_counts)

#'Plot the sentiment for Rhinegeist.Rhingeist sentiment analysis is very similar to MadTree's. 
#'Each revolves around creating a theme for their brewery with words like awesome, happy, and free. 
#'They also have words that are not really negative in the context used. For example, the majority of the negative 
#'sentiment words such as wild, hazy, sour dark, and pale all describe their brand and the beer. 
#'From Rhingeist's sentiment analysis, it seems like they describe their brews more compared to 
#'MadTree. 
r_sent <- Rhinegeist_counts %>%
  filter(n > 15) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment",
       title = "Rhinegeist Tweets Sentiment")
r_sent

#'##Conclusion:
#'       
#'    
#'If we look at these plots side-by-side it is much more clear to draw conclusions.    
plot_grid(m_ts, r_ts, labels = "AUTO", nrow = 2)
#'MadTree clearly is more actively tweeting with its two official accounts especially in the
#'winter. Rhinegeist however takes advantage of the summer months and really ups their twitter 
#'activity in preparation for their anniversary. MadTree based on their Twitter activity seem to
#'be much more active when it comes to communicating with customers. 
plot_grid(munique, runique, labels = "AUTO", ncol = 2)
#'I think the unique word plot is where the differences really show. I think MadTree needs to
#'better leverage it's community aspect. That is a key portion of their success is the garden 
#'where you can drink beers with friends and family, but it is hard to read that from this graph. 
#'Like I previously mentioned, they market more to families encouraging them to bring their 
#'kids or their dogs. The fun for the whole family is really the core of their appeal, but they 
#'are struggling to promote that on Twitter. Rhingeist has a much more concrete and unified approach to 
#'using Twitter. They seem to be much more event focused and coordinated between social media platforms 
#'as they invite followers on Twitter to checkout their facebook events. 
plot_grid(m_sent, r_sent, labels = "AUTO", ncol = 2)
#'The sentiment analysis does not prove much in how each brewery markets themself on Twitter.
#'They use very similar words to invite people to come to events and enjoy the weekend at their 
#'brewery frequently and the negative words are actually positive in this context. The key 
#'distinction in the sentiment analyiss is that Rhinegeist is more descriptive of the brand itself 
#'using key words to illustrate it's gothic aesthetic. 
#'
#'##Final Thoughts:
#'
#'I think MadTree does not do enough to distinguish itself from another popular brewery 
#'Rhinegeist. I think it has a very family friendly vibe which is one of the key 
#'strengths that they are not leveraging with their online presence. Rhinegeist does a better job
#'of differentiating itself from MadTree as it adopts a event-oriented approach to Twitter. They 
#'are also more descriptive of the brand and product itself. They utilize the unique gothic tone of the 
#'brand to better market themselves on Twitter compared to MadTree. 