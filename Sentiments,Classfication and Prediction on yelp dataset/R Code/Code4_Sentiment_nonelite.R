library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyverse)
library(textcat)

######################################### Read consolidated data #############################################################

combined_df_nonelite <- read.csv("F:\\Advanced Data Mining\\Project\\Output_Ohio\\combined_df_nonelite.csv", stringsAsFactors = FALSE)

#################################################### Calling sentiment libraries ############################################

#afinn<-get_sentiments("afinn")
#bing <-get_sentiments("bing")
nrc <-get_sentiments("nrc")

##################################################### Taking only relevant columns ##########################################

bus_rev_nonelite_df <- combined_df_nonelite[,c(2,7,6)]

##################################################### Putting each word in review in a separate row ###########################

tidy_nonelite_sent_df <- bus_rev_nonelite_df %>% unnest_tokens(word, text)

################################################ Final sentiment output for elite users #############################

nonelite_nrcSent <- tidy_nonelite_sent_df %>%
  inner_join(nrc) %>%
  count(index = business_id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

nonelite_nrcSent <- merge(x = nonelite_nrcSent, y = combined_df_nonelite, by.y = "business_id", by.x = "index")

nonelite_nrcSent <- nonelite_nrcSent[,c(18,20,1:12)]

nonelite_nrcSent <- distinct(nonelite_nrcSent)

#nonelite_nrcSent_1 <- nonelite_nrcSent %>% inner_join(elite_bus_id, by = "index")

remove("combined_df_nonelite", "bus_rev_nonelite_df", "nrc", "tidy_nonelite_sent_df")