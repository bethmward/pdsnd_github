
ny = read.csv('new_york_city.csv')

chi = read.csv('chicago.csv')

head(ny)



head(chi, 8)

library(ggplot2)
library(grDevices) # I'm using this because I find it way easier for working with colours


# Base plot overlayed by two histograms to portray the subcriber birth years of the two cities
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

# Function writes a title for any summary in this project
mySummaryTitle <- function(x = "x", bySummary = FALSE, y = "y", city = "Washington DC") {
    if (bySummary == FALSE) {
        writeLines(paste(x, "of", y, "in", city))
   } else {
        writeLines(paste(x, "by", y, "in", city))
    }
}

# Summarise birth year data
mySummaryTitle("Birth Years", FALSE, "Bikeshare Subscribers", "New York City")
summary(ny$Birth.Year, subset(ny, !is.na(Birth.Year))) 
mySummaryTitle('Birth Years', FALSE, "Subscribers", "Chicago City")
summary(chi$Birth.Year, subset(chi, !is.na(Birth.Year)))

# Add a column for age for concise code
ny$Age <- (as.numeric(format(as.Date(ny$Start.Time), '%Y')) - ny$Birth.Year)

# Base plot overlayed by scatterplot displaying age:trip_duration
ggplot(aes(x = Age, y = Trip.Duration), data = subset(ny, !is.na(Age))) +
    geom_jitter(alpha = 1/100, lwd = 2) +
    scale_y_continuous(limits = c(0, 5000)) +
    scale_x_continuous(limits = c(16, 100), breaks = seq(10, 100, 10)) +
    labs(x = 'Age', y = 'Trip Duration (Seconds)') +
    ggtitle('Bikeshare Ride Durations by Subscriber Age in New York City') 

# summarised trip duration data to display the extremity of outliers 
mySummaryTitle('Duration', FALSE, 'Bikeshare Trips', 'New York City')
summary(ny$Trip.Duration)

# summarised age data
mySummaryTitle('Age', FALSE, 'Bikeshare Subscribers', 'New York City')
summary(ny$Age)

# adjusted boxplot to take care of major outliers
ggplot(aes(x = Gender, y = Trip.Duration), data = subset(chi, Gender != '')) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0, 3000)) +
    scale_y_continuous(breaks = seq(0, 3000, 500)) +
    labs(x = 'Gender', y = 'Trip Duration (Seconds)') +
    ggtitle('Bikeshare Bike Ride Duration by Gender in Chicago City')

# summarise trip durations by gender
mySummaryTitle('Bikeshare Trip Durations', TRUE, 'Gender', 'Chicago City')
by(chi$Trip.Duration, chi$Gender, summary)

system('python -m nbconvert Explore_bikeshare_data.ipynb')
