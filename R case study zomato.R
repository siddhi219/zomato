library(readr)
library(dplyr)
library(ggplot2)
library(lazyeval)
library(mosaic)
library(statisticalModeling)
library(stringr)
library(tidyverse)
library(leaflet)
install.packages("mosaic")
install.packages("lazyeval")
install.packages("tidyverse")

#=============================================================================
#import data
zomato<- read.csv("zomato.csv")

#============================================================================
#============================================================================
#DATA CLEANING

zomato1<-zomato %>% select(-opentable_support,-zipcode,-rating_text)
zomato1$delivery[zomato1$delivery==-1]<-"NO"
zomato1$delivery[zomato1$delivery==1]<-"YES"
zomato1$takeaway[zomato1$takeaway==-1]<-"NO"
zomato1$delivery[zomato1$delivery=='0']<-NA
zomato1<-na.omit(zomato1)

zomato1$rating_text <- ifelse(zomato1$aggregate_rating<2.5,"Poor",ifelse(zomato1$aggregate_rating<3.5,"Average",ifelse(zomato1$aggregate_rating<4,"Good",ifelse(zomato1$aggregate_rating<4.5,"Very Good",ifelse(zomato1$aggregate_rating<5,"Excellent","...")))))

attach(zomato)

#=============================================================================
#=============================================================================
##removing  outlier
zomato2<-zomato1 %>% filter(average_cost_for_two>45  & average_cost_for_two<=15000)
summary(zomato2$average_cost_for_two)
zomato3<-zomato2 %>% filter(votes>=1)



data<-zomato3

#========================================================================================================================================
#========================================================================================================================================
#1.What are the most popular restaurants type around bangalore?
pop_rest<-data %>% filter(city=='Bangalore') %>% group_by(establishment) %>% summarise(rest_type=n()) %>% arrange(desc(rest_type)) %>% head(8)
ggplot(data=pop_rest, aes(x=establishment , y=rest_type))+geom_bar(stat = 'identity', color ="black" , fill ="coral1")





#2.Which city has the most expensive restaurants?
exp<-data %>% filter(average_cost_for_two>10000) %>% group_by(city,average_cost_for_two) %>%  summarise(exp_res=n()) %>% arrange(desc(average_cost_for_two))
ggplot(data=exp , aes(x=city , y=average_cost_for_two))+geom_bar(stat = 'identity' , color ="black" , fill ="cyan1")




#3.Which restaurants Provides Zomato with the Highest Number of Customers
largest_votes<-data %>% filter(votes>11000) %>% group_by(name,votes) %>% summarise(highest_voting=n()) %>% arrange(desc(votes)) %>% head(6)
ggplot(data = largest_votes , aes(x=name , y= votes))+geom_bar(stat = 'identity' , color ="black" , fill ="darkviolet")




#4.which restaurant have received highest rating in India?
rating<-data %>%filter(aggregate_rating>4.8) %>% group_by(city,name, aggregate_rating) %>% summarise(highest_rating=n()) %>% arrange(desc(highest_rating)) %>% head(6)
ggplot(data = rating , aes(x=name , y=highest_rating))+geom_bar(stat = 'identity'   , fill ="lightpink")




#5.Which Restaurants serves the highest variety of Rajasthani dishes ?
rajasthani_dish<-data %>% filter(cuisines== "Rajasthani") %>% group_by(name , cuisines ) %>% summarise(dish=n()) %>% arrange(desc(dish)) %>% head(5)
ggplot(data = rajasthani_dish , aes(x=name , y=dish))+geom_bar(stat = "identity" , color ="black" , fill =" darkkhaki")




#6. Which of the Restaurants in Mumbai come under Excellent Category ?
excellent_category<-data %>% filter(city=='Mumbai'& rating_text=='Excellent') %>% group_by(city,rating_text , name) %>% summarise(rate=n()) %>% arrange(desc(rate)) %>% head(6)
ggplot(data = excellent_category , aes(x=name , y=rate))+geom_bar(stat = "identity" , color="black" , fill=" Lightblue")



#7.Which city has highest number of restaurants registered on zomato  ?
res_reg<-data %>% select(city) %>% count(city) %>% arrange(desc(n)) %>% head(15)
ggplot(data = res_reg , aes(x=city , y=n))+geom_bar(stat = "identity" , color ="black" , fill =" lightgreen")




#8.Which of the restaurants in Mumbai are Purely Vegetarian ?
pure_veg <- data %>%
  filter(str_detect(highlights,"Pure Veg"))%>%
  select(name,city,highlights)



#9. Which restaurants serves Alcohol?

serves_alcohol <- data %>%
  filter(str_detect(highlights,"Serves Alcohol"))%>%
  select(name,city,highlights)




#10.Which restaurant serve  best continental dishes based on the ratings provided by the customers ?
continental<- data %>%filter(cuisines=="Continental" & aggregate_rating>=4.3) %>% group_by(name,cuisines,aggregate_rating)%>% summarise(best=n()) %>% arrange(desc(aggregate_rating))



#11.What are the most popular cuisines in the city of Mumbai?
most_popular<-data %>% filter(city=="Mumbai") %>% group_by(name,cuisines) %>% summarise(ordered=n()) %>% arrange(desc(ordered))  %>%  head(10)


#12. . Which restaurants are open till midnight?
working_at_midnight <- data %>%
  filter(str_detect(timings,"Midnight")|(str_detect(timings,"midnight")))%>%
  select(name,city,timings)


#13 which restaurants in Mumbai are Indoor Seating Arrangement?
Indoor_seating <- data %>%
  filter(str_detect(highlights,"Indoor Seating"))%>% filter(city=='Mumbai')%>%
  select(name,city,highlights)


#14.which city has the highest number of hotels with ratings as 'Excellent' ?
#used to check which city has the higgest count of restautants registered

max_number <- data %>% filter(rating_text =='Excellent')%>%
  group_by(city,rating_text)%>%
  summarise(count = n())%>%top_n(count,n=1)

#15.Which are the Restaurant's with Ratings as Excellent in Chennai ?
#Chennai has the maximum count of restaurants registered on zomato with excellent rating

restaurants_chennai <-data %>% filter(rating_text=='Excellent'& city == 'Chennai')

leaflet()%>%addTiles()%>%
  setView(lng = 80.20, lat = 13.04, zoom = 12)%>% #set a particular view on map for chennai
  addCircleMarkers(lat=restaurants_chennai$latitude,
                   lng = restaurants_chennai$longitude,
                   radius = 0.0005)



#16. Which Hotels lie in Mumbai the Category below 'Good'?
belowgood<-data %>% filter(rating_text=='Poor'   |  rating_text=='Average') %>%
  group_by(rating_text , name) %>% summarise(rate=n()) %>% arrange(desc(rate)) %>% head(6)

ggplot(data = belowgood , aes(x=name , y=rate))+geom_bar(stat = "identity") +
  geom_col(fill = ifelse(belowgood$rating_text=='Poor',"green",
                         ifelse(belowgood$rating_text=='Average',"red","orange")))
