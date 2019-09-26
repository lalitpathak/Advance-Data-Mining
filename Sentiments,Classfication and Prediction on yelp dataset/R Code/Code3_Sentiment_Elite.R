library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyverse)
library(textcat)

######################################### Read consolidated data #############################################################

combined_df_elite <- read.csv("F:\\Advanced Data Mining\\Project\\Output_Ohio\\combined_df_elite.csv", stringsAsFactors = FALSE)

#################################################### Calling sentiment libraries ############################################

#afinn<-get_sentiments("afinn")
#bing <-get_sentiments("bing")
nrc <-get_sentiments("nrc")

##################################################### Taking only relevant columns ##########################################

bus_rev_elite_df <- combined_df_elite[,c(2,7,6)]

##################################################### Putting each word in review in a separate row ###########################

tidy_elite_sent_df <- bus_rev_elite_df %>% unnest_tokens(word, text)

################################################ Final sentiment output for elite users #############################

elite_nrcSent <- tidy_elite_sent_df %>%
  inner_join(nrc) %>%
  count(index = business_id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

elite_nrcSent <- merge(x = elite_nrcSent, y = combined_df_elite, by.y = "business_id", by.x = "index")

elite_nrcSent <- elite_nrcSent[,c(18,20,1:12)]

elite_nrcSent <- distinct(elite_nrcSent)

#elite_bus_id <- as.data.frame(elite_nrcSent$index)

#colnames(elite_bus_id) <- "index"

remove("combined_df_elite", "bus_rev_elite_df", "nrc", "tidy_elite_sent_df")