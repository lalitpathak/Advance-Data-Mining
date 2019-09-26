#################################################### Row binding review and business csv files ###################################

df1 <- read.csv("F:\\Advanced Data Mining\\Project\\Output_Ohio\\bus_rev_join_1.csv")

df2 <- read.csv("F:\\Advanced Data Mining\\Project\\Output_Ohio\\bus_rev_join_2.csv")

df3 <- read.csv("F:\\Advanced Data Mining\\Project\\Output_Ohio\\bus_rev_join_3.csv")

df4 <- read.csv("F:\\Advanced Data Mining\\Project\\Output_Ohio\\bus_rev_join_4.csv")

final_df <- rbind(df1, df2, df3, df4)

final_df <- final_df[,-1]

####################################################### Row binding user csv files ##########################################

u_df1 <- read.csv("F:\\Advanced Data Mining\\Project\\Output_Ohio\\USER_1.csv")

u_df2 <- read.csv("F:\\Advanced Data Mining\\Project\\Output_Ohio\\USER_2.csv")

u_df3 <- read.csv("F:\\Advanced Data Mining\\Project\\Output_Ohio\\USER_3.csv")

u_df4 <- read.csv("F:\\Advanced Data Mining\\Project\\Output_Ohio\\USER_4.csv")

u_final_df <- rbind(u_df1, u_df2, u_df3, u_df4)

###################################################### Joining review, business & user #######################################

combined_df <- merge(x=final_df, y=u_final_df, by = "user_id")

##################################################### renaming few columns ##################################################

names(combined_df)[names(combined_df) == "stars.x"] <- "review_stars"

names(combined_df)[names(combined_df) == "stars.y"] <- "business_stars"

names(combined_df)[names(combined_df) == "name.x"] <- "business_name"

names(combined_df)[names(combined_df) == "name.y"] <- "user_name"

############################################## Adding a flag to identify elite users ########################################

library(lubridate)

combined_df$elite_on_review_date[str_detect(combined_df$elite, as.character(year(combined_df$date)))] <- "Yes"

############################################### Cleaning ##########################################################

combined_df$text <- gsub(pattern="\\W", replace=" ",combined_df$text)

combined_df$text <- gsub(pattern="\\d",replace=" ",combined_df$text)

combined_df$text <- tolower(combined_df$text)

combined_df$text <- removeWords(combined_df$text,stopwords())

combined_df$text <- gsub(pattern = "\\b[a-z]\\b{1}",replace=" ",combined_df$text)

combined_df$text <- stripWhitespace(combined_df$text)

############################################## Elite users data frame with their reviews ########################################

combined_df_elite <- combined_df %>% filter(elite_on_review_date == "Yes")

write.csv(combined_df_elite, "combined_df_elite.csv", row.names = FALSE)

############################################# Users other than elite with their reviews #########################################

combined_df_nonelite <- combined_df %>% filter(is.na(elite_on_review_date))

write.csv(combined_df_nonelite, "combined_df_nonelite.csv", row.names = FALSE)

remove("combined_df", "df1", "df2", "df3", "df4", "final_df", "u_df1", "u_df2", "u_df3", "u_df4", "u_final_df", "combined_df_elite", "combined_df_nonelite")