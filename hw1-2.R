library(tidyverse)
setwd("/Users/john/R working_dir")
watch_table <- read.csv("watch_table.csv")
user_table <- read.csv("user_table.csv")
drama_table <- read.csv("drama_table.csv")

#2-1
full_table <- watch_table %>% left_join(drama_table) %>% left_join(user_table)

#2-2
gender_view_df <- full_table %>% group_by(drama_id,gender) %>% 
  summarise(number = length(gender)) %>%
  as.data.frame()

#2-3
iOS_df <- filter(full_table,device == "iOS") 
mean_age_of_iOS_user <- group_by(iOS_df,gender) %>% summarize(mean(age)) %>% as.data.frame()

#2-4
taipei_male_df <- filter(full_table, gender == "male" & location == "Taipei")
mean_age_of_taipei_male_user <- mean(taipei_male_df$age)
taipei_male_device_df <- group_by(taipei_male_df,device) %>% summarise(number = length(device)) %>% as.data.frame() 