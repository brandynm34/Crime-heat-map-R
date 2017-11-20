library(ggplot2)
library(plyr)
library(dplyr)

crimes <- read.csv("~/Documents/Data_science_Club/R_Tutorials/crime.csv")
crimes <- crimes[, 2:ncol(crimes)]

numberOfRows <- nrow(crimes)
numberOfCols <- ncol(crimes)

# distinct types of crimes
crimeCategories <- unique(crimes$Category)

# Don't touch this
print(paste("There are", numberOfRows, "observations."))
print(paste("There are", numberOfCols, "variables."))
print(paste("List of all types of crimes:", paste(crimeCategories, collapse = ", ")))

# points in the top right are incorrect.
# Find incorrect values (badvalues)

badValues <- crimes[ (crimes$X > -121.5) & (crimes$Y > 60), ]
numberOfBadValues <- nrow(badValues)

# reverse the conditional selection to remove them from the crimes dataframe
crimes <- crimes[(crimes$X < -121.5) & (crimes$Y < 60), ]
newNrow <- nrow(crimes)

print(paste("Number of wrong observations:", numberOfBadValues))
print(paste("Number of observations after removal:", newNrow))

# Use table to get a frequency count of the crimes
crimeTable <- table(crimes$Category)

# transform the table object into a dataframe for you.
crimeFreq <- as.data.frame(crimeTable)
names(crimeFreq) <- c("Category", "Frequency")

# sorted the dataframe by frequency, put most frequent at top
crimeFreq <- arrange(crimeFreq, desc(Frequency))

# top ten crime names
topTen <- crimeFreq[1:10, "Category"]

# use only those top ten crimes (the subset)

lessCrimes <- crimes[crimes$Category %in% topTen, ]

print(paste("Top ten crimes:", paste(topTen, collapse=", ")))

options(repr.plot.width=7, repr.plot.height=5)

# making a ggplot scatterplot with the subsetted data
lessColoredPlot <- ggplot(lessCrimes) +
  geom_point(aes(x=X, y=Y, color=Category)) +
  theme_minimal()

lessColoredPlot

devtools::install_github("dkahle/ggmap")
library(ggmap)
SFmap <- get_map(location = c(lon = mean(lessCrimes$X), lat = mean(lessCrimes$Y)), 
                 zoom = 12, maptype = "roadmap", color = "bw")
ggmap(SFmap)

options(repr.plot.width=7, repr.plot.height=5)

# streetmap the base layer, 
almostDone <- ggmap(SFmap) +
  geom_point(aes(x=X, y=Y, color=Category), data=lessCrimes) +
  scale_x_continuous(limits=c(-122.518307, -122.355789)) +
  scale_y_continuous(limits=c(37.7047, 37.823))

# final map
done <- ggmap(SFmap) +
  geom_point(aes(x=X, y=Y, color=Category), data=lessCrimes, size=0.9, alpha=0.3) +
  scale_x_continuous(limits=c(-122.518307, -122.355789)) +
  scale_y_continuous(limits=c(37.7047, 37.823)) +
  guides(colour = guide_legend(override.aes = list(alpha=1.0, size=6.0), title="Type of Crime")) +
  ggtitle("Heat Map of Crimes")  +
  xlab("Latitude") +
  ylab("Longitude")

done