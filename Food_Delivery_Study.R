library(dplyr)
library(ggplot2)

data <- read.csv("data/Food_Time_Data_Set.csv")

str(data)

head(data)

# Categorical Columns

## 'Type of order' seems to be a categorical columns, but by default it has a 'chr' type, so it will be converted into a factor column. The same happens with type_of_vehicle, weather_description and traffic_level

data <- data %>% 
  mutate(Type_of_order = as.factor(Type_of_order),
         Type_of_vehicle = as.factor(Type_of_vehicle),
         weather_description = as.factor(weather_description),
         Traffic_Level = as.factor(Traffic_Level))

str(data)

# Numerical columns

## Most numerical columns seem to be correct except for the TARGET column and the 'Distance..km' columns



data <- data %>%
  mutate(Distance..km. = as.numeric(Distance..km.),
         TARGET = as.numeric(TARGET))

## The conversion was done, but NAs were introduced by coercion.

sum(is.na(data$Distance..km.))

sum(is.na(data$TARGET))

## Maybe the NA values in Distance happen to also affect into an NA in TARGET

data %>% 
  filter(is.na(Distance..km.) & is.na(TARGET)) %>% 
  head(.)

## All values shown that have NA in distance also have NA in TARGET. To see if the result is a value close to 921 (the amount of total NA values in Distance), the amount of rows that have NA in both distance and target will be calculated.

data %>% 
  filter(is.na(Distance..km.) & is.na(TARGET)) %>% 
  summarize(nrow(.))

## These values are not useful towards the model later on because these are values that cannot be predicted at all. As a result, these values will be removed from the dataset.

data <- data %>%  
  filter( !(is.na(Distance..km.) & is.na(TARGET)) ) 

## Now, re-calculating the count of na values:

sum(is.na(data$Distance..km.))

sum(is.na(data$TARGET))

## Analyzing the remaining Distance values:

data %>% 
  filter(is.na(Distance..km.))

## It seems that all of the other needed values are still there for these rows, so I will use the restaurant coordinates vs. the delivery location coordinates to fill the distance:
## https://community.esri.com/t5/coordinate-reference-systems-blog/distance-on-a-sphere-the-haversine-formula/ba-p/902128
## https://github.com/rspatial/geosphere

if(!require(geosphere)) install.packages("geosphere", repos = "http://cran.us.r-project.org")

library(geosphere)

new_distances <- data %>%
  filter(is.na(Distance..km.)) %>% 
  rowwise() %>%
  mutate(new_distance = distHaversine(c(Restaurant_longitude, Restaurant_latitude),
                                     c(Delivery_location_longitude, Delivery_location_latitude)) / 1000) %>% 
  head(.) %>% 
  select(Distance..km.,
         new_distance,
         Restaurant_latitude, 
         Restaurant_longitude, 
         Delivery_location_latitude, 
         Delivery_location_longitude,
         TARGET) %>% 
  pull(new_distance)

data[is.na(data$Distance..km.), "Distance..km."] <- new_distances

sum(is.na(data$Distance..km.))

rm(new_distances)

## It can be seen that now there's no more NA values in the Distance..km. column.

## Now analyzing the leftover TARGET NA values:

sum(is.na(data$TARGET))

data[is.na(data$TARGET),]

# The amount of rows that will be in the na-cleaned dataset is:
nrow(data) - sum(is.na(data$TARGET))

## There are 45 missing values in the target variable, because the goal of this study is to train the algorithm on the most accurate data available, then the best option for these NA values is to delete them, since filling them in any way can introduce an incorrect bias that could prevent the model from learning correct patterns from the data.

data <- data[!is.na(data$TARGET),]

nrow(data)

# Variable names

## In the provided dataset there's no standard naming convention for the columns, some start with a capital letter while others don't and some are in all-caps (TARGET and ID) while others aren't.

colnames(data) <- c("id", "delivery_person_id", "delivery_person_age", "delivery_person_ratings",
  "restaurant_latitude", "restaurant_longitude", "delivery_loc_latitude",
  "delivery_loc_longitude", "order_type", "vehicle_type", "temperature", "humidity",
  "precipitation", "weather_type", "X", "traffic_level", "distance", "delivery_time_min")

str(data)

## Now there's a more uniform column name standard.

# Per-column Analysis --------------

#ID Variable

n_distinct(data$id)

nrow(data)

## Interistingly, there are less distinct ID values (9037) than rows (9040).

data %>% 
  group_by(id) %>% 
  summarize(appearances = n()) %>% 
  arrange(desc(appearances))

data %>% 
  filter(id == "6.00E+02")

## There doesn't seem to be any anormal values, the only remarkable detail is that the coordinates of both the restaurant and the delivery destination are similar between entries.

data %>% 
  filter(id == "6.00E+03")

## Again the coordinates are similar, but that's about it.

data %>% 
  filter(id == "9.00E+02")

## In here the longitudes between rows vary greatly, but the latitudes do kind of match.

sum(is.na(data$id))

## There are no null values

# Delivery Person Id

head(data$delivery_person_id, n=8)

n_distinct(data$delivery_person_id)

## In this dataset there's only 1134 different delivery people, meaning that the dataset has multiple deliveries from a single person.

sum(is.na(data$delivery_person_id))

## There are no null values

# Delivery Person Age

head(data$delivery_person_age, n=8)

min(data$delivery_person_age)

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

data %>% 
  filter(Restaurant_latitude == 11.0213 & Restaurant_longitude == 76.995) %>% 
  summarize(mean_)
