rm(list = ls())

library(tidyverse)
library(lubridate)
library(ggplot2)
install.packages("caTools")
library(caTools)
install.packages("FNN")
library(FNN)


data_path <- "./Data/Train.csv"
train <- read_csv(data_path)
View(train)

sample_train <- sample_n(train, size = 100000)
plot(sample_train$pickup_x, sample_train$pickup_y)

# t <- sample_train[(sample_train$pickup_x <= 175) & 
#                     (sample_train$pickup_x >= 135) &
#                     (sample_train$pickup_y <= 425) &
#                     (sample_train$pickup_y >= 250) &
#                     (sample_train$dropoff_x <= 200) &
#                     (sample_train$dropoff_x >= -100) &
#                     (sample_train$dropoff_y >= 100) &
#                     (sample_train$dropoff_y <= 550)
#                     ,]


t <- sample_train[(sample_train$pickup_x <= 175) & 
                    (sample_train$pickup_x >= 135) &
                    (sample_train$pickup_y <= 425) &
                    (sample_train$pickup_y >= 250) &
                    (sample_train$dropoff_x <= 175) &
                    (sample_train$dropoff_x >= 135) &
                    (sample_train$dropoff_y >= 250) &
                    (sample_train$dropoff_y <= 425)
                  ,]

plot(t$dropoff_x, t$dropoff_y)

# Date-Time Features
t$pickup_datetime <- as.POSIXlt(t$pickup_datetime, format = "%Y-/%m-/%d %H:%M:%S")
t$Month <- t$pickup_datetime$mon + 1
t$Day <- t$pickup_datetime$wday
t$Date <- t$pickup_datetime$mday
t$Hour <- t$pickup_datetime$hour

t$Month <- as.factor(t$Month)
t$Day <- as.factor(t$Day)
t$Date <- as.factor(t$Date)
t$Hour <- as.factor(t$Hour)



dist_finder <- function(df, pickup_x, dropoff_x, pickup_y, dropoff_y){
  return(dist(rbind(df[pickup_x], df[dropoff_x]), method = "manhattan") + dist(rbind(df[pickup_y], df[dropoff_y]), method = "manhattan"))
}

t["man_dist"] <- apply(t,1, dist_finder, pickup_x = "pickup_x", dropoff_x = "dropoff_x", pickup_y = "pickup_y", dropoff_y = "dropoff_y")

speed_finder <- function(df, duration, man_dist){
  speed = as.double(getElement(df, man_dist))/as.double(getElement(df, duration))
  return(speed)
}

t["speed"] <- apply(t, 1, speed_finder, duration = "duration", man_dist = "man_dist")

str(t)

# Data Visualizations
t %>%
  ggplot(aes(man_dist, duration)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Direct distance [m]", y = "Trip duration [s]")

t %>%
  filter(duration < 8000 & duration > 8) %>%
  filter(man_dist > 5 & man_dist < 800) %>%
  ggplot(aes(man_dist, duration)) +
  geom_bin2d(bins = c(500,500)) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Direct distance [m]", y = "Trip duration [s]")

t %>%
  filter(speed < 0.25 ) %>%
  ggplot(aes(speed)) +
  geom_histogram(fill = "red", bins = 100) +
  labs(x = "Average speed")



# clustering based on location, to create regions.
set.seed(20)
locations <- data_frame(c(t$pickup_x,t$dropoff_x), c(t$pickup_y,t$dropoff_y))
location_clusters <- kmeans(locations[, 1:2], 80, nstart = 20)
plot(locations$`c(t$pickup_x, t$dropoff_x)`, locations$`c(t$pickup_y, t$dropoff_y)`, col = location_clusters$cluster)

# Define pickup and drop off regions based on the clusters generated
t["pickup_region"] <- get.knnx(location_clusters$center, t[,3:4], 1)$nn.index[,1]
t["dropoff_region"] <- get.knnx(location_clusters$center, t[,5:6], 1)$nn.index[,1]

# Visualize pickup and drop off regions
plot(t$pickup_x,t$pickup_y, col = t$pickup_region)
plot(t$dropoff_x,t$dropoff_y, col = t$dropoff_region)


# create train, validation and test data in the ratio of 70:15:15
model_data <- t[, c(2, 7, 9:14)]
set.seed(123)
sample = sample.split(model_data, SplitRatio = 0.70)
train1 = subset(x = model_data, sample == TRUE)
temp = subset(x = model_data, sample == FALSE)

sample1 = sample.split(temp, SplitRatio = 0.5)
val1 = subset(x = temp, sample1 == TRUE)
test1 = subset(x = temp, sample1 == FALSE)

write_csv(train1, "./train1.csv")
write_csv(val1, "./val1.csv")
write_csv(test1, "./test1.csv")
