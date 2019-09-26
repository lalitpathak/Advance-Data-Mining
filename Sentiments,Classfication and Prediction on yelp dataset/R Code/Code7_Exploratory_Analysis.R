library(tidyverse) #  data manipulation and graphs
library(stringr) #  string manipulation
library(lubridate) #  date manipulation
library(wordcloud) #  wordcloud
library(tidytext) # tidy implementation of NLP methods
library(DT)       # table format display of data
library(leaflet) # maps
library(igraph) #  graphs
library(ggraph) #  graphs
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(textcat)
library(ggplot2)
library(MAP)
library(maps)
library("ggmap")
library(maptools)

# 1) Missing values in our dataset:

setwd("C:\\ADM\\ADM Datasets");

combined_df_exp <- read.csv("rev_user_bus.csv", stringsAsFactors = FALSE)
missing_data <- plot_missing(combined_df_exp)

Count_of_orignal_data <- count(combined_df_exp)   #add image for both unique count and actual count
colnames(Count_of_orignal_data) <- c("Actual_Count")

# 2) Number of unique values from the dataset
unique_df <- count(unique(combined_df_exp)) #### identify the unique value in the dataset
colnames(unique_df) <- c("Unique_Count")

# 3) how many business are open and closed in the bar graph 

# write.csv(combined_df_unique_busniess,"combined_df_unique_busniess.csv",row.names = FALSE)


combined_df_unique_busniess <- unique(combined_df_exp[,c('business_id','is_open')])
cbbPalette <- c("#E69F00", "#56B4E9")

open_close <- combined_df_unique_busniess %>%
group_by(is_open) %>%
summarise(Count = n()) %>%
arrange(desc(Count))

open_close$is_open[open_close$is_open == "1"] <- "Open"
open_close$is_open[open_close$is_open == "0"] <- "Close"


ggplot(data=open_close, aes(x=is_open, y=Count)) +
geom_bar(stat="identity",fill=cbbPalette)+
geom_text(aes(label=Count),hjust=0,vjust=1.6, color="Black", size=4) +
labs(x = 'Status', y = 'Count',title = 'Total Number Of Open And Close Restaurant In The State Of Ohio') +
theme_bw()



# 3) Top 10 business_name in the State of Ohio
# 
# 
# 
# Unique_business <- combined_df_exp %>%
# group_by(business_name) %>%
# summarise(Count = n()) %>%
# arrange(desc(Count)) %>%
# ungroup() %>% 
# # mutate(City = reorder(city,Count)) %>%
# head(10)


# 4) Top 10 cities with most businesses in Ohio ###########

combined_df_cities_max_business <- unique(combined_df_exp[,c('business_id','city')])
combined_df_cities_max_business %>%
group_by(city) %>%
summarise(Count = n()) %>%
arrange(desc(Count)) %>%
ungroup() %>%
# mutate(City = reorder(city,Count)) %>%
head(10) -> Most_Business
  
ggplot(data=Most_Business, aes(x=city, y=Count, color=city,fill= city)) +
geom_bar(stat="identity")+
geom_text(aes(label=Count), vjust=1.6, color="black", size=3.5)+
labs(x = 'City', y = 'Count of Business',title = 'Top 10 Cities With  Most Number of Businesses in Ohio')+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 5) Count of  Reviews by elite and non elite user in the state of Ohio

rev_bus_user_bar <- read.csv("rev_user_bus.csv", stringsAsFactors = FALSE)

barchart_data <- rev_bus_user_bar %>%
group_by(elite_on_review_date) %>%
summarise(Count = n()) %>%
arrange(desc(Count))

colnames(barchart_data) <- c("User_Type", "Count")

barchart_data$User_Type[is.na(barchart_data$User_Type)] <- "Non Elite"
barchart_data$User_Type[barchart_data$User_Type == "Yes"] <- "Elite"

ggplot(data=barchart_data, aes(x=User_Type, y=Count,fill=User_Type)) +
geom_bar(stat="identity")+
geom_text(aes(label=Count),hjust=0,vjust=1.6, color="black", size=4)+
labs(x = 'Users', y = 'Count',title = 'Total Number Of Reviews by Elite and Non Elite Users in Ohio') +
theme_bw()


# 6) visualize the frequency of the ratings in the states of Ohio

# 1) Busniess_Rating    

unique(combined_df_exp[,c(2,9)]) %>%
group_by(business_stars) %>%
summarise(Count = n()) %>%
arrange(desc(business_stars)) %>%
ungroup() %>%
mutate(business_stars = reorder(business_stars,Count)) %>%
  
ggplot(aes(x = business_stars,y = Count,fill=business_stars)) +
geom_bar(stat='identity',) +
geom_text(aes(x = business_stars, y = 1, label = (round(Count))),hjust=0, vjust=.3, size = 4, colour = 'White',fontface = 'bold') +
labs(x = 'Business_star', y = 'Count',title = 'Business Rating Distribution by No. of Users in Ohio') +
coord_flip() +
theme_bw()


# 2) Review_stars
  
combined_df_exp %>% group_by(review_stars) %>% summarize(count=n());
combined_df_exp %>%
group_by(review_stars) %>%
summarise(Count = n()) %>%
arrange(desc(review_stars)) %>%
ungroup() %>%
mutate(review_stars = reorder(review_stars,Count)) %>%
    
ggplot(aes(x = review_stars,y = Count,fill=review_stars)) +
geom_bar(stat='identity',colour="white") +
geom_text(aes(x = review_stars, y = 1, label =round(Count)),hjust=0, vjust=.5, size = 4, colour = 'black',fontface = 'bold') +
labs(x = 'Star', y = 'Count',title = 'Review Rating Distribution by No. of Users in Ohio') +
coord_flip() + 
theme_bw()


#############################################################################################################################
######################################## 7) Worcloud and Bar Chart ##########################################################

rev_bus_user <- read.csv("rev_user_bus.csv", stringsAsFactors = FALSE)

rev_bus_user <- rev_bus_user[,c(3,6)]

# Cleaning

rev_bus_user$text <- gsub(pattern="\\W", replace=" ",rev_bus_user$text)

rev_bus_user$text <- gsub(pattern="\\d",replace=" ",rev_bus_user$text)

rev_bus_user$text <- tolower(rev_bus_user$text)

rev_bus_user$text <- removeWords(rev_bus_user$text,stopwords())

rev_bus_user$text <- gsub(pattern = "\\b[a-z]\\b{1}",replace=" ",rev_bus_user$text)

rev_bus_user$text <- stripWhitespace(rev_bus_user$text)

# Unnest

unnest_text <- rev_bus_user %>% unnest_tokens(word, text)

unnest_text <- as.data.frame(unnest_text[,-1])

colnames(unnest_text) <- "Word"

count_by_word <- unnest_text %>%
group_by(Word) %>%
summarise(Count = n()) %>%
arrange(desc(Count)) %>%
ungroup()

# Selecting first 100 rows
count_by_word <- count_by_word[1:100,]


# Wordcloud

wordcloud(count_by_word$Word, # words
count_by_word$Count,
colors = brewer.pal(8, "Dark2"),
rot.per = 0.35, random.order=FALSE)




