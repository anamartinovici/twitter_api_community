rm(list=ls())

library("httr")
library("tidyverse")

use_header <- list("header" = c(Authorization = paste0("Bearer ", 
													   Sys.getenv("BEARER_TOKEN"))))

endpoint_user_lookup   <- "https://api.twitter.com/2/users/by?usernames="
endpoint_recent_search <- "https://api.twitter.com/2/tweets/search/recent?query="
endpoint_tweet_lookup  <- "https://api.twitter.com/2/tweets?ids="

test_hashtag1 <- "test1ngADF"
test_hashtag2 <- "test1ng"

################################################
# Step 1: test connection to API
################################################

response <-	httr::GET(url = paste0(endpoint_user_lookup, "TwitterDev"),
					  config = httr::add_headers(.headers = use_header[["header"]]))

# for a complete list of HTTP status codes, 
#		check: https://developer.twitter.com/ja/docs/basics/response-codes
if(httr::status_code(response) == 200) {
	cat(paste("The HTTP status code is: ", status_code(response), sep = ""))
	cat("\n")
	cat("This means Success!")
} else {
	cat("Oh, no! Something went wrong.\n")
	cat(paste("The HTTP status code is: ", status_code(response), "\n", sep = ""))
	cat("Check the list of HTTP status codes to understand what went wrong.\n")
}
remove(response)

################################################
# Step 2: recent search endpoint
################################################

params <- list(tweet.fields = 'referenced_tweets')
response <-	httr::GET(url = paste0(endpoint_recent_search, 
								   URLencode(paste0(test_hashtag1, " OR ", test_hashtag2),
								   		  reserved = TRUE)),
					  config = httr::add_headers(.headers = use_header[["header"]]),
					  query = params)
httr::status_code(response)
# convert the output, this should create a list
obj_recentsearch <- httr::content(response)
# check the list
names(obj_recentsearch)
sapply(obj_recentsearch[["data"]], names)

# some tweets are retweets or quoted, others are neither (ref_tweet is null)
f_get_tweet_type <- function(input_list) {
	if(is.null(input_list[["referenced_tweets"]])) {
		# you can change the label to use for a tweet that is neither a quote or a retweet
		return("original_tweet")
	} else {
		return(input_list[["referenced_tweets"]][[1]][["type"]])	
	}
}

f_get_ref_tweet_id <- function(input_list) {
	if(is.null(input_list[["referenced_tweets"]])) {
		# you can change the label to use for a tweet that is neither a quote or a retweet
		return("original_tweet")
	} else {
		return(input_list[["referenced_tweets"]][[1]][["id"]])	
	}
}

text_recentsearch <- obj_recentsearch[["data"]] %>% {
	tibble(id = map_chr(., "id"),
		   text = map_chr(., "text"),
		   type = map_chr(., f_get_tweet_type),
		   ref_tweet_id = map_chr(., f_get_ref_tweet_id))
}

text_recentsearch <- text_recentsearch %>% 
	mutate(text_length = str_length(text),
		   includes_hashtag_1 = str_detect(text, regex(test_hashtag1, ignore_case = TRUE)),
		   includes_hashtag_2 = str_detect(text, regex(test_hashtag2, ignore_case = TRUE)))
# the request sent to the API asked for all tweets that include one of the hashtags
# therefore, the expected number of tweets that contain neither hashtag is 0 (zero)
nrow(text_recentsearch %>% filter(includes_hashtag_1 == FALSE, includes_hashtag_2 == FALSE))
table(text_recentsearch[["type"]],
	  text_recentsearch[["text_length"]])

# get the pairs of original tweet and retweet ids
retweets <- text_recentsearch %>% filter(type == "retweeted")
id_RT_1 <- retweets[["id"]][1]
id_original_tweet_1 <- retweets[["ref_tweet_id"]][1]

id_RT_2 <- retweets[["id"]][2]
id_original_tweet_2 <- retweets[["ref_tweet_id"]][2]

################################################
# Step 3: tweet lookup endpoint
################################################

response <-	httr::GET(url = paste0(endpoint_tweet_lookup, 
								   URLencode(paste0(id_RT_1, ",", 
								   				    id_original_tweet_1, ",",
								   				    id_RT_2, ",",
								   				    id_original_tweet_2),
								   		     reserved = TRUE)),
					  config = httr::add_headers(.headers = use_header[["header"]]))
httr::status_code(response)
# convert the output, this should create a list
obj_tweet_lookup <- httr::content(response)
# check the list
names(obj_tweet_lookup)

df_tweet_lookup <- obj_tweet_lookup[["data"]] %>% {
	tibble(id = map_chr(., "id"),
		   text = map_chr(., "text"))
}

df_tweet_lookup <- df_tweet_lookup %>% 
	mutate(text_length = str_length(text))

# this is the original tweet (280 characters long)
df_tweet_lookup %>% filter(id == id_original_tweet_1)
# the retweet is only 140 characters long (expected 280, based on:
# https://dev.to/twitterdev/understanding-the-new-tweet-payload-in-the-twitter-api-v2-1fg5
# Section name "Tweet Text":
# "The v1.1 payload returned the truncated Tweet text 
# (for Tweets over 140 characters) by default, and also returned 
# a Boolean field to identify whether the Tweet is truncated or not. 
# In order to get the full Tweet text, you needed to use the 
# tweet_mode = extended parameter and then do a conditional check 
# on the truncated field. 
# With the new v2 Tweet payload, you do not need to do this check. 
# The Tweet text for all Tweets (including 140+ characters) 
# is returned in the 'text' field and can be used directly."
df_tweet_lookup %>% filter(id == id_RT_1)

# same for the second fair of original_tweet and retweet
df_tweet_lookup %>% filter(id == id_original_tweet_2)
df_tweet_lookup %>% filter(id == id_RT_2)
