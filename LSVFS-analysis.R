##### RE-Analysis post-1st review
## Analysis of LSVFS data from calvary baptist church
## monitoring period August 2017-November 2017
## data location in CBC_LSVFS.DEL.csv
## Visualizing data
require("ggplot2")      # Powerful and aesthetic plotting system for R
require("gridExtra")    # Arrange multiple ggplots in same figure (multi-panels)
require("scales")       #
require("RColorBrewer") # creates nice color schemes
require("corrplot")     # A graphical display of a correlation matrix between all combinations of variables
## Statistical analysis
require("stats")        # Lots of stats stuff
## Data management
require("plyr")         # Allows you t split data structure into groups (pollutant type, location, etc.) and apply function on each group
require("dplyr")
require("zoo")          # Helps streamline data if you have irregular time series
require("reshape2")     # Convert data with "wide" columns to "long" columns
require("lubridate")    # Date and time data made easy! See reference PDF in Google Drive
require("data.table")
require("TTR")
#require("xlsx")        # creates errors # Reads and writes to xlsx file
require("purrr")
require("tidyr")
require("fBasics")
require("pls")
## Mapping tools
require("stringi")
require("ggmap")        # Plotting of maps same as you would with ggplot2
require("maptools")     # Read, write, and handle Shapefiles in R
require("mapdata")      # Supplement to maps package

## Read data file
# Data file has previous manipulations
LSVFS <- read.csv("./Working/CBC_LSVFS.DEL.csv")
## View to confirm proper read
#View(LSVFS)

## rename columns
colnames(LSVFS) <- c("date.time", 
                     "rainfall", 
                     "intensity",
                     "Air.temp", 
                     "In.temp", 
                     "In.depth", 
                     "Out.temp", 
                     "Out.depth", 
                     "event")
# Confirm
# View(LSVFS)

## Set date time fomat
LSVFS$date.time <- mdy_hm(LSVFS$date.time, tz = "est")
# Confirm class
#class(LSVFS[,1])

## Need to convert units to metric
LSVFS.m <- mutate(LSVFS, rainfall = (rainfall * 25.4), 
                  intensity = (intensity * 25.4), 
                  Air.temp = (Air.temp - 32)/1.8, 
                  In.temp = (In.temp - 32)/1.8, 
                  In.depth = (In.depth * 30.48), 
                  Out.temp = (Out.temp - 32)/1.8, 
                  Out.depth = (Out.depth * 30.48))
#View(LSVFS.m)

## Antecedant dry period analysis
## Rainfall event delineation
# Exstract from Drizzle0.9.5 + modified
event <- LSVFS.m$event
event[event != 0] <- NA
ADP.index <- cumsum(diff(!is.na(c(NA, (event)))) > 0) + (0*event)
# Add ADP index as new variable
LSVFS.m[, "ADP.index"] <- ADP.index
#Replace index NAs with zero
LSVFS.m$ADP.index[is.na(LSVFS.m$ADP.index)] <- 0 
# Confirm
# View(LSVFS.m)

## Summary of ADP
ADP.sum <- (LSVFS.m) %>%
  group_by(ADP.index) %>%
  summarise(duation = (max(date.time) - min(date.time))) 
#View(ADP.sum)
# Range in days
# 1.02-81.67
# Median in days
# 3.73

