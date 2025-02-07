if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

if (!require(ggplot2)) install.packages("ggplot2")
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

## In these code blocks the ages of the delivery people differ, so the 'id' itself isn't a unique person identifier.

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

max(data$delivery_person_age)

## The ages of the delivery people range from 15 to 50.

data %>% 
  filter(delivery_person_age == 15)

## There's 3 entries of a 15-year-old doing deliveries.

data %>% 
  filter(delivery_person_age == 50)

## There are also 3 entries of a 50 year old delivering.

data %>% 
  ggplot(aes(delivery_person_age)) +
  geom_histogram(col = "black", fill = "turquoise")

## Most ages range from 20 to 40, with 15 and the 50 being the outliers.

## However, this graph can be wrong due to the fact that takes multiple deliveries as a different count for the age.

data %>% 
  distinct(delivery_person_id, .keep_all = TRUE) %>% # grabbing different people or else multiple deliveries would count as multiple ages
  ggplot(aes(delivery_person_age)) +
  geom_histogram(col = "black", fill = "turquoise")

## With this graph, the scale changes and now the 15 and 50 values don't appear. Could this be due to them not having a 'delivery_person_id' (NA)? (No because there are no NA values in 'delivery_person_id') Or maybe the same ID has multiple ages assigned to it (in this case this would be human error)?


data %>% 
  filter(delivery_person_age == 15)

data %>% 
  filter(delivery_person_id == "JAPRES15DEL03") %>% 
  select(delivery_person_age)

## This is a big issue, there are multiple ages assigned to the same delivery_person_id

## Is this a common mistake? Does this happen for each delivery_person_id?

data %>% 
  group_by(delivery_person_id) %>% 
  distinct(delivery_person_age) %>% 
  summarize(distinct_ages = n())

## This is indeed a common mistake, since the docs for this dataset mention: "Delivery_person_ID: A unique identifier assigned to each delivery person for tracking purposes."

## So, the Delivery_person_ID isn't a reliable identifier for a singular person.

## Therefore, an accurate count of each age's different workers (delivery person) cannot be obtained.

## A possible cause for this is that the program made to assign 'delivery_person_id's to workers aren't meant to generate one per worker, instead it could generate one per delivery in real time, and when one delivery_person_id is no longer in use, then it could re-assign it to another delivery, hence another possible driver.


## Checking to see if the driver's age has to do with the delivery time:

data %>% 
  ggplot(aes(delivery_person_age, delivery_time_min)) +
  geom_point(col = "black")

## There's no clear effect of the age on the delivery time.

# Delivery person Ratings -----------

min(data$delivery_person_ratings)

max(data$delivery_person_ratings)

## The rating scale is from 1 to 6, inclusive. This rating scale seems to deviate from 'normal' scales in the sense that most apps have a 1-5 scale or a 1-10 scale, but a 1-6 scale is very uncommon.

data %>% 
  ggplot(aes(delivery_person_ratings)) +
  geom_histogram(col = "black", fill = "turquoise")

## Most delivery people have a rating between 3 and 5. With the other ratings having very low counts.

data %>% 
  filter(delivery_person_ratings > 5)

## An interesting detail is that the only delivery people who have a rating bigger than 5 (6) are 50 year olds. However, due to recent findings stating that the Delivery_person_ID variable cannot identify singular people, this is most likely a single person that is 50 years old that somehow has this 6 rating.

## So again, an accurate count of the number of distinct people (in order to count the amount of people per rating) cannot be obtained.

## Checking on the possible effect of age on delivery time:

data %>% 
  ggplot(aes(x = delivery_person_ratings, y = delivery_time_min)) +
  geom_point(col = "black")

## The rating itself doesn't seem to have an effect on delivery time since all ratings deliver on very similar timeframes.

# Restaurant latitude and longitude 

## Latitude and longitude values can be very raw, they just specify coordinates on a map. However, if a latitude and longitude pairing appears several times in a repeated manner, this means that there are several orders for the same restaurant (since the latitude and longitude pairing uniquely identifies position of a restaurant). This technically assumes that no restaurant can be on top of another (this is very common in malls), so this is to get an idea of the amount of deliveries that possibly come from the same restaurant.

## This can even be studied to see if most people order from the same restaurant and therefore that restaurant needs more time than others in order to prepare the food.

options(pillar.sigfig = 6)

data %>% 
  group_by(restaurant_latitude, restaurant_longitude) %>% 
  summarize(order_count = n()) %>% 
  arrange(desc(order_count))

## 54 orders seem to be from the same restaurant, 48 from another and so on.

## It is important to compare the average delivery time of this popular restaurant with one that has a low delivery count to see if these coordinate pairings introduce a bias on the target variable.

data <- data %>% 
  mutate(restaurant_latitude = round(restaurant_latitude, 4),
         restaurant_longitude = round(restaurant_longitude, 4))

data %>% 
  group_by(restaurant_latitude, restaurant_longitude) %>% 
  summarize(order_count = n()) %>% 
  arrange(desc(order_count))

data %>% 
  filter(restaurant_latitude == 11.0213 & restaurant_longitude == 76.995) %>% 
  summarize(mean_delivery_time = mean(delivery_time_min))

## The most sought-out restaurant has a mean delivery time of almost 35 minutes.

data %>% 
  group_by(restaurant_latitude, restaurant_longitude) %>% 
  summarize(order_count = n()) %>% 
  arrange(order_count)

data %>% 
  filter(restaurant_latitude == 9.9668 & restaurant_longitude == 76.243) %>% 
  summarize(mean_delivery_time = mean(delivery_time_min))

## One of the least ordered restaurant has almost a 36 minute delivery time.

## The average time of both restaurants is extremely similar, indicating that there isn't a 'delay' effect by ordering from a 'popular' restaurant in this case.

# Delivery location

## Again, these coordinate values are very raw so the amount of orders that came from the same home will be calculated. Again, this assumes that no home can be on top of another, and this is almost the norm in terms of capital cities where there exist multiple apartment buildings.

data <- data %>% 
  mutate(delivery_loc_latitude = round(delivery_loc_latitude, 4),
         delivery_loc_longitude = round(delivery_loc_longitude, 4))

data %>% 
  group_by(delivery_loc_latitude, delivery_loc_longitude) %>% 
  summarize(order_count = n()) %>% 
  arrange(desc(order_count))

## There are several 'homes' that ordered 9 times in this data.

# Order type

head(data$order_type, n=8)

data %>% 
  group_by(order_type) %>% 
  summarize(appearances = n(), avg_delivery_time = mean(delivery_time_min)) %>% 
  arrange(desc(appearances))

## Thanks to the output, it can be seen that the most common order type is the snack, followed by the meal, drinks and buffet being the last one.  

## Also, it can be inferred that ordering drinks usually takes longer than ordering meals, this could be due to the fact that these could be low-income orders, so restaurant managers could give a higher priority to meals since these usually cost more than just drinks.

## So the order type does matter when predicting average delivery time.

# Vehicle type

head(data$vehicle_type, n=8)

data %>% 
  group_by(vehicle_type) %>% 
  summarize(appearances = n(), avg_delivery_time = mean(delivery_time_min)) %>% 
  arrange(desc(appearances))

## Interestingly enough, bicycle deliveries are the ones that have the least amount of average delivery time out of the four. This could be due to bicycle riders not taking up on large rides (in the sense of deliveries that are far from the restaurant) due to its increased physicality needed. This added physicality can also explain the low amount of people that use it for delivery purposes.

## This will be calculated for verification:

data %>% 
  group_by(vehicle_type) %>% 
  summarize(appearances = n(),
            avg_delivery_time = mean(delivery_time_min),
            delivery_distance = mean(distance)) %>% 
  arrange(desc(appearances))

## As it was hinted at previously, the bicycle riders indeed have a lower aaverage delivery distance when compared to motorcycle riders for example.

## An interesting remark are the scooter riders. While using a scooter does include physicality during work (but not as much as bicycles do), it still remains the second most-used vehicle type for food transportation.

## All of this hints at the possibility of the vehicle_type having an impact on delivery time.

# Temperature

head(data$temperature, n=8)

## These temperatures seem to be in Celsius rather than Fahrenheit. This isn't specified in the docs.

data %>% 
  ggplot(aes(temperature)) +
  geom_histogram(fill = "turquoise", col = "black")

## Most temperatures seem to be around de 22.5 mark.

## Heat could introduce laziness and exhaustion for the delivery drivers (https://www.belsky-weinberg-horowitz.com/tips-for-avoiding-heat-sickness-while-driving/), so it wouldn't be abnormal to see that higher temperatures lead to higher delivery times:

data %>% 
  ggplot(aes(x = delivery_time_min, y = temperature)) +
  geom_point(fill = "turquoise", col = "black")

## It is surprising to see that the most-heated deliveries are the ones who usually have lower delivery times. However, an argument can be made that states that this could be because people that live closer to these restaurants have a hotter climate than those who live further away. Things like the heat from gas car engines are prejudicial for the climate (therefore hotter temperatures).

## So the temperature seems to correlate to the delivery time, but it does so in the contrary sense to the one thought.

# Humidity

head(data$humidity)

min(data$humidity)

max(data$humidity)

# The values of the humidity seem to be percentages (this is the usual metric for the general population). This also isn't described in the dataset docs.

data %>% 
  ggplot(aes(humidity)) +
  geom_histogram(fill = "turquoise", col = "black")

## As per the histogram, it can be seen that the humidity values seem to be pretty scattered, there's no bell-like shaped curve present in the graph.

data %>% 
  ggplot(aes(x = delivery_time_min, y = humidity)) +
  geom_point(col = "black")

## No clear pattern can be extracted on the effect of humidity on delivery time.

# Precipitation

head(data$precipitation, n = 8)

n_distinct(data$precipitation)

min(data$precipitation)

max(data$precipitation)

## The precipitation values don't seem to come in a binary or percentage format.

data %>% 
  ggplot(aes(x = precipitation)) +
  geom_histogram(fill = "turquoise", col = "black")

## The most prominent precipitation value is zero (no rain or snow). All of the other values on the scale barely appear, being almost a line at the bottom of the graph.

## Inspecting those values different to zero:

data %>% 
  filter(precipitation != 0)

## There's just 1 entry that has a value bigger than 1, but this entry marks the weather as "moderate rain", so values bigger than 1 mean that there's definite rain (and not mist) and the .46 shows how heavy the rainfall is.

data %>% 
  ggplot(aes(x = delivery_time_min, y = precipitation)) +
  geom_point(col = "black")

## There's no clear pattern between the precipitation and the delivery time.

# Weather type

data %>% 
  ggplot(aes(x = weather_type, y = precipitation)) +
  geom_boxplot(fill = "turquoise", col = "black")

## The only weather types to have non-zero values are mist and moderate rain.


## Plotting the possible effect of the weather type on delivery time:
data %>% 
  ggplot(aes(x = weather_type, y = delivery_time_min)) +
  geom_boxplot(fill = "turquoise", col = "black")

## The weather_type seems to have an extremely little effect on delivery time (see fog, moderate rain and overcast clouds), but the difference is so small that this could introduce almost no effect over the model.

# X column

head(data$X)

n_distinct(data$X)

## There are only NA values in this column:

data %>% 
  filter(!is.na(X))

## The filter returned 0 rows. Therefore, this column serves no purpose and it will be discarded from the dataset:

data <- data %>% 
  select(-X) 

# Traffic level

## Naturally, traffic is a great cause for arriving late to a location, however, because the vehicle types here aren't 4 wheeled transport means, then I expect the traffic level to have little effect over the delivery time.

data %>% 
  ggplot(aes(x = reorder(traffic_level, delivery_time_min, FUN = median), y = delivery_time_min)) +
  geom_boxplot(fill = "turquoise", col = "black") +
  labs(x = "Traffic Level",
       y = "Delivery Time (m)")

## Despite the vehicle types in the dataset being 2-wheeled, the traffic does have quite a clear correlation with the delivery time.

## So the Traffic level does incur in the delivery time.

# Distance

mean(data$distance)

## The average distance traveled by the delivery people is around 14 kilometers.

data %>% 
  ggplot(aes(distance)) + 
  geom_histogram(fill = "turquoise", col = "black")

## Shorter distances are more common than longer distances.

## The distance itself is naturally a big factor in arrival time to a destination, I expect this to have the same behavior here:

data %>% 
  ggplot(aes(x = distance, y = delivery_time_min)) + 
  geom_point(col = "black") 

## There is a clear line pattern between distance and delivery time, as I expected. The bigger the distance, the greater the delivery time.

# Delivery time 

## This is the target variable.

mean(data$delivery_time_min)

## People usually have to wait around 38 minutes for their food delivery.

data %>% 
  ggplot(aes(delivery_time_min)) + 
  geom_histogram(fill = "turquoise", col = "black")

## Usually, extremely quick deliveries are rare, as well as extremely delayed ones, with the middleground being more common. This behavior is seen in the histogram.

# Handling missing data

sum(is.na(data))

## There's no missing data.

# Handling inconsistent values:

## It was pointed out before that the delivery_person_id column is basically made up of inconsistent values (not the expected behavior for this study), since it can't identify a singular person.

data %>% 
  group_by(delivery_person_id) %>% 
  distinct(delivery_person_age) %>% 
  summarize(distinct_ages = n())

## So this column will be deleted.

data <- data %>% 
  select(-delivery_person_id)

## The 'id' column was also shown to be of little value:
data %>%
  group_by(id) %>%
  summarize(appearances = n()) %>%
  arrange(desc(appearances))

data %>%
  filter(id == "6.00E+02")

## Here the same id is assigned to different people and different deliveries.


data <- data %>% 
  select(-id)

## The 'id' column has been removed.

str(data)

sum(duplicated(data))

## There are no duplicate entries in the data

# Dataset split

if (!require(caret)) install.packages("caret")
library(caret)

set.seed(2025)

train_indexes <- createDataPartition(data$delivery_time_min, p = 0.8, list = FALSE)
train_data <- data[train_indexes,]
test_data <- data[-train_indexes,]

# Outlier deletion

## No outliers will be deleted since important features like the distance correlate very well and give good insights for predicting the delivery time.

# Data Scaling

## Since the models that are going to be used are linear regression and decision trees, then no scaling is needed (both models aren't sensible to scale).

# Feature selection

nums <- lapply(data, is.numeric)
nums <- unlist(nums)

data[, nums]


correlation_matrix <- cor(data[,nums])
correlation_matrix

## These values contain a lot of numbers, so rounding will be used again here for ease-of-read purposes.

correlation_matrix <- round(correlation_matrix, 2)
correlation_matrix

str(correlation_matrix)

head(correlation_matrix)

## Converting the data into a long format (it's currently as a wide format):

if (!require(reshape2)) install.packages("reshape2")
library(reshape2)

correlation_matrix <- melt(correlation_matrix)

head(correlation_matrix)

## Now it can be seen that the relationships between variables are described in just 2 columns, rather than a lot more (as it was before).

## Now creating a correlation heatmap

correlation_matrix %>% 
  ggplot(aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90),
        axis.title = element_blank())
