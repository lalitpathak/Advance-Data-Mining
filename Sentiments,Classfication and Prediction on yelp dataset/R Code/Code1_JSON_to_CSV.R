library(stringr) 
library(jsonlite) 
library(dplyr) 
library(tidyr)
 
 
########### Reviews File

infile = "F:\\Data Mining JSON Files\\review.json" 
 
#review_lines <- readr::read_lines(infile, n_max = 1000000) 

#review_lines <- readr::read_lines(infile, skip = 1000000, n_max = 1000000) 

review_lines <- readr::read_lines(infile, skip = 2000000, n_max = 1000000)

#review_lines <- readr::read_lines(infile, skip = 3000000) 
 
reviews_combined <- str_c("[", str_c(review_lines, collapse = ", "), "]") 
 
reviews <- fromJSON(reviews_combined) 

reviews_1 <- flatten(reviews)  

reviews_2 <- tbl_df(reviews_1) 
 
review_words <- reviews_2 %>% select(review_id, business_id, user_id, stars,date, text) 
 
######## Business File 

infile_bus = "F:\\Data Mining JSON Files\\business.json" 

bus_lines <- readr::read_lines(infile_bus)

buss_combined <- str_c("[", str_c(bus_lines, collapse = ", "), "]")

buss <- fromJSON(buss_combined)

buss_1 <- flatten(buss) 

buss_2 <- tbl_df(buss_1) 
 
bus_words <- buss_2 %>% select(business_id, name, city, stars, is_open, state) %>% filter(state=="OH")

############################### Join reviews and business ##################################################### 

df <- merge(x=review_words, y=bus_words, by="business_id") 

write.csv(df, "bus_rev_join_3.csv")

######################################################### User ########################################################
library(stringr) 
library(jsonlite) 
library(dplyr) 

infile = "F:\\Advanced Data Mining\\Project\\Data Mining JSON Files\\user.json" 

#review_lines<-readr::read_lines(infile, n_max = 1000000) 

#review_lines<-readr::read_lines(infile, skip = 1000000, n_max = 1000000) 

users_lines <- readr::read_lines(infile, skip = 1500000, n_max = 500000)

users_combined <- str_c("[", str_c(users_lines, collapse = ", "), "]") 

users <- fromJSON(users_combined)

users_1 <- flatten(users) 

users_2 <- tbl_df(users_1) 

users_words <- users_2 %>% select(user_id, name, elite, average_stars, fans)

write.csv(users_words, "USER_4.csv", row.names = FALSE)
