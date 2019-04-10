#' ---
#' title: "Local Brewery Twitter Analysis"
#' author: "Tim Gordon"
#' date: "April 10th, 2019"
#' ---
#' #load rtweet and httpuv packages to access twitter content
library(rtweet)
library(httpuv)

#Load developer app key
appname <- "user"
key <- "key"
secret <- "secret"

#Use the previously assigned objects into the twitter_token
twitter_token <- create_token(
  app=appname,
  consumer_key = key,
  consumer_secret = secret,
  set_renv = FALSE)

#Get timelines of both official madtree accounts: madtreebrewing, and madtreetaproom.
#I am using timeline because I am trying to see if there is any distinction 
#between the 2 breweries twitter activity. This is why I set it to 3000 
#so I could get tweets from at least 3 years ago. This is especially important 
#for Madtree because they opened the new taproom on their 4th anniversary 
#in February 2017. They did not start the new twitter account until the 
#following summer. This will be visisble in the other script. 
rtweet_Madtree <- get_timeline(c("MadTreeBrewing","MadTreeTaproom"), 
                               token = twitter_token, n=3000, 
                               lang = 'en', include_rts = FALSE)
table(rtweet_Madtree$screen_name)

#Get timeline of Rhinegeist twitter.
#Rhinegeist has only one official account for one place to get all our 
#data from. I wanted to keep the number of tweets I was searaching for 
#with each respective brewery consistent at 3000 to get tweets from 
#at least 3 years ago. 
rtweet_Rhinegeist <- get_timeline(c("Rhinegeist"),
                                  token = twitter_token, n=3000,
                                  lang = 'en', include_rts = FALSE)
table(rtweet_Rhinegeist$screen_name)

#Check the data and save to csv for further analysis. 
rtweet_Madtree
write_as_csv(rtweet_Madtree, "MadTreetweetsUpdate.csv")

rtweet_Rhinegeist
write_as_csv(rtweet_Rhinegeist, "RhinegeistTweetsUpdate.csv")




