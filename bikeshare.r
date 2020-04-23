
# An Exploration into the Members of Bikeshare Companies in Chicago and New York City
#
# Editor : Bethany Marie Ward
# Version : 1.1
# Date : 23/04/2020
#
# Created By : Bethany Marie Ward
# Date : 15/04/2020
#
# For description, associated files and credits view README.md

ny = read.csv('new_york_city.csv')
chi = read.csv('chicago.csv')

head(ny)
head(chi, 8)

library(ggplot2)
library(grDevices)

# Member birth years of the two cities
ggplot()  + 
    geom_histogram(
        aes(x = Birth.Year, y = ..density.., fill = "New York City"),           
        data = subset(ny, !is.na(Birth.Year)), 
        binwidth = 1
    ) +
    geom_histogram(
        aes(x = Birth.Year, y = ..density.., fill = "Chicago"),           
        data = subset(chi, !is.na(Birth.Year)), 
        binwidth = 1
    ) +
    scale_x_continuous(limits = c(1920, 2020), breaks = seq(1920, 2020, 20)) + # Seeing that there were some anomalies/outliers in early years, I decided to limit the data
    labs(x = 'Birth Year', y = 'Density') +
    ggtitle('Birth Years of Bikeshare Subscribers by City') +
    scale_fill_manual("", values = c(adjustcolor('tomato', alpha = 0.75), adjustcolor('violet', alpha = 0.75)), breaks = c("New York City", "Chicago")) + 
    scale_color_discrete(guide = FALSE)

# Birth year data
writeLines("Birth Years of Bikeshare Subscribers in New York City")
summary(ny$Birth.Year, subset(ny, !is.na(Birth.Year))) 
writeLines("Birth Years of Subscribers in Chicago City")
summary(chi$Birth.Year, subset(chi, !is.na(Birth.Year)))

# Add a column Age to NYC data
ny$Age <- (as.numeric(format(as.Date(ny$Start.Time), '%Y')) - ny$Birth.Year)

# Age:trip_duration
ggplot(aes(x = Age, y = Trip.Duration), data = subset(ny, !is.na(Age))) +
    geom_jitter(alpha = 1/100, lwd = 2) +
    scale_y_continuous(limits = c(0, 5000)) +
    scale_x_continuous(limits = c(16, 100), breaks = seq(10, 100, 10)) +
    labs(x = 'Age', y = 'Trip Duration (Seconds)') +
    ggtitle('Bikeshare Ride Durations by Subscriber Age in New York City') 

# Trip duration data displays the extremity of outliers 
writeLines('Duration of Bikeshare Trips in New York City')
summary(ny$Trip.Duration)

# Age data
writeLines('Age of Bikeshare Subscribers in New York City')
summary(ny$Age)

# Gender:trip_duration (adjusted to ignore extremities)
ggplot(aes(x = Gender, y = Trip.Duration), data = subset(chi, Gender != '')) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0, 3000)) +
    scale_y_continuous(breaks = seq(0, 3000, 500)) +
    labs(x = 'Gender', y = 'Trip Duration (Seconds)') +
    ggtitle('Bikeshare Bike Ride Duration by Gender in Chicago City')

# Trip durations by gender in Chicago
writeLines('Bikeshare Trip Durations by Gender in Chicago City')
by(chi$Trip.Duration, chi$Gender, summary)

system('python -m nbconvert Explore_bikeshare_data.ipynb')
