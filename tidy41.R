# Import packages
library(readr)  # for read_csv
library(knitr)  # for kable
library(dplyr)
library(ggplot2)
library(tidyverse)

#Download data from github
myfile <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv"

iplifts <- read_csv(myfile)
View(iplifts) # check out the dataset

#Coerce columns into factors
cols <- c("sex","event","equipment","age_class",       
          "division","weight_class_kg",
          "place","meet_name" )
iplifts[cols] <- lapply(iplifts[cols], factor)  

df_clean <- iplifts %>% 
  janitor::clean_names()

df_clean %>% 
  group_by(federation) %>% 
  count(sort = TRUE)

size_df <- df_clean %>% 
  select(name:weight_class_kg, starts_with("best"), 
         place, date, federation, meet_name)  %>% 
  filter(!is.na(date)) %>% 
  filter(federation == "IPF") %>% 
  object.size()

ipf_data <- df_clean %>% 
  select(name:weight_class_kg, starts_with("best"), 
         place, date, federation, meet_name)  %>% 
  filter(!is.na(date)) %>% 
  filter(federation == "IPF")


#Body weight versus maximum bench lift, wrap by gender
ggplot(data = ipf_data,
       aes(x = bodyweight_kg, y = best3bench_kg,
           color = equipment)) +
  geom_jitter(stat="identity")+
    facet_wrap(~sex, scales = "free") +
 labs(x = "The recorded bodyweight of the lifter (kg)",
      y = "Maximum successful attempt for the benchlift (kg)",
      caption = "Source:International Powerlifting Federation",
      color = "Equipment category")+
 theme_bw()+ 
 theme(
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10),
    legend.position="right",
    axis.text = element_text(
      size = 12))

#Animate maximum and minimum weights by year
#load gganimate package
library(gganimate)
theme_set(theme_bw())#set theme

#Create column with year
ipf_data$date <- as.Date(ipf_data$date,"%Y-%m-%d")
ipf_data = ipf_data %>% mutate(year = year(date))

theme_set(theme_bw())
ipf_data1 = ipf_data %>% 
  filter (best3deadlift_kg>0) #omit negative values

#Create a static plot
p <- ggplot(
  ipf_data1, 
  aes(x = bodyweight_kg , y = best3deadlift_kg , 
      colour = sex)) +
  geom_point(alpha = 0.7) +
  scale_color_discrete(labels = c("Female","Male")) +
  labs(x = "The recorded bodyweight of the lifter (kg)",
       y = "Maximum successful attempt for the deadlift (kg)",
       caption = "Source: International Powerlifting Federation")
 
p
#transitions through distinct states in time
p + transition_states(year,transition_length = 1,
                      state_length = 1) +
  labs(title = "Year: {closest_state}")

#create facets by equipment
p + facet_wrap(~equipment) +
  transition_states(year, transition_length = 1,
                    state_length = 1) +
  labs(title = "Year: {closest_state}")+
  view_follow(fixed_y = TRUE)#Follow the data in each frame





