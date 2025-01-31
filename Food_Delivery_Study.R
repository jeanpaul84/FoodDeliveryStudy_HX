library(dplyr)
library(ggplot2)

data <- read.csv("data/Food_Time_Data_Set.csv")

str(data)

head(data)

#ID Variable

n_distinct(data$ID)

## Interistingly, there are less distinct ID values than rows.

sum(is.na(data$ID))

## There are no null values

# Delivery Person Id

head(data$Delivery_person_ID, n=8)

n_distinct(data$Delivery_person_ID)

## In this dataset there's only 1286 different delivery people, meaning that the dataset has multiple deliveries from a single person.

sum(is.na(data$Delivery_person_ID))

## There are no null values

# Delivery Person Age

head(data$Delivery_person_Age, n=8)

min(data$Delivery_person_Age)

## Min doesn't work since there's a null value.

sum(is.na(data$Delivery_person_Age))

## There's indeed just 1 null value

data[is.na(data$Delivery_person_Age), ]

## Seems like the entry (row) itself is invalid because every column has an NA value.

## Deleting this row:

str(data[!is.na(data$Delivery_person_Age), ])

data <- data[!is.na(data$Delivery_person_Age), ]

## Calculating min again

min(data$Delivery_person_Age)

data %>% 
  filter(Delivery_person_Age == 15)

max(data$Delivery_person_Age)

## The ages of the delivery people range from 15 to 50.

data %>% 
  ggplot(aes(Delivery_person_Age)) +
  geom_histogram(col = "black", fill = "turquoise")

## Most ages range from 20 to 40, with 15 and the 50 being the outliers.

## However, this graph can be wrong due to the fact that takes multiple deliveries as a different count for the age.

data %>% 
  distinct(Delivery_person_ID, .keep_all = TRUE) %>% # grabbing different people or else multiple deliveries would count as multiple ages
  ggplot(aes(Delivery_person_Age)) +
  geom_histogram(col = "black", fill = "turquoise")

## With this graph, the scale changes and now the 15 and 50 values don't appear. Could this be due to them not having an ID (NA)? Or maybe the same ID has multiple ages assigned to it (in this case this is human error)?


data %>% 
  filter(Delivery_person_Age == 15)

data %>% 
  filter(Delivery_person_ID == "JAPRES15DEL03") %>% 
  select(Delivery_person_Age)

## This is a big issue, there are multiple ages assigned to the same Delivery_person_ID

## Is this a common mistake? Does this happen for each Delivery_person_ID?

data %>% 
  group_by(Delivery_person_ID) %>% 
  distinct(Delivery_person_Age) %>% 
  summarize(distinct_ages = n())

## This is indeed a common mistake, since the docs for this dataset mention: "Delivery_person_ID: A unique identifier assigned to each delivery person for tracking purposes."

## So, the Delivery_person_ID isn't a reliable identifier for a singular person.

## Therefore, an accurate count of each age's different workers (delivery person) cannot be obtained.


# Delivery person Ratings -----------

min(data$Delivery_person_Ratings)

max(data$Delivery_person_Ratings)

## The rating scale is from 1 to 6, inclusive. This rating scale seems to deviate from the norm in the sense that most apps have a 1-5 scale or 1-10 scale, but a 1-6 scale is very peculiar.

data %>% 
  ggplot(aes(Delivery_person_Ratings)) +
  geom_histogram(col = "black", fill = "turquoise")

## Most delivery people have a rating between 3 and 5. With the other ratings having very low counts.

data %>% 
  filter(Delivery_person_Ratings > 5)

## An interesting detail is that the only delivery people who have a rating bigger than 5 (6) are 50 year olds. However, due to recent findings stating that the Delivery_person_ID variable cannot identify singular people, this is most likely a single person that is 50 years old that somehow has this 6 rating.

## So again, an accurate count of the number of distinct people (in order to count the amount of people per rating) cannot be obtained.


# Restaurant latitude and longitude 

## Latitude and longitude values can be very raw, they just specify coordinates on a map. However, if a latitude and longitude pairing appears several times in a repeated manner, this means that there are several orders for the same restaurant (since the latitude and longitude pairing uniquely identifies position of a restaurant). This technically assumes that no restaurant can be on top of another (this is very common in malls), so this is to get an idea of the amount of deliveries that possibly come from the same restaurant.

## This can even be studied to see if most people order from the same restaurant and therefore that restaurant needs more time than others in order to prepare the food.

options(pillar.sigfig = 6)

data %>% 
  group_by(Restaurant_latitude, Restaurant_longitude) %>% 
  summarize(order_count = n()) %>% 
  arrange(desc(order_count))

## There seems to be a count of 801 deliveries who don't have the restaurant's coordinates (assuming 0, 0 isn't an intentional entry, since it is in the middle of the ocean).

## Apart from that, 54 orders seem to be from the same restaurant, 48 from another and so on.

## It is important to compare the average delivery time of this popular restaurant with one that has a low delivery count.

data <- data %>% 
  mutate(Restaurant_latitude = round(Restaurant_latitude, 4),
         Restaurant_longitude = round(Restaurant_longitude, 4))

data %>% 
  group_by(Restaurant_latitude, Restaurant_longitude) %>% 
  summarize(order_count = n()) %>% 
  arrange(desc(order_count))
