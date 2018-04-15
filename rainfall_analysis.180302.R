####### Analysis in project file
#Visualizing data
require("ggplot2")      # Powerful and aesthetic plotting system for R
require("gridExtra")    # Arrange multiple ggplots in same figure (multi-panels)
require("scales")       #
require("RColorBrewer") # creates nice color schemes
require("corrplot")     # A graphical display of a correlation matrix between all combinations of variables
#Statistical analysis
require("stats")        # Lots of stats stuff
#Data management
require("plyr")         # Allows you t split data structure into groups (pollutant type, location, etc.) and apply function on each group
require("dplyr")
require("zoo")          # Helps streamline data if you have irregular time series
require("reshape2")     # Convert data with "wide" columns to "long" columns
require("lubridate")    # Date and time data made easy! See reference PDF in Google Drive
require("data.table")
require("xlsx")         # Reads and writes to xlsx file
require("purrr")
require("tidyr")
#Mapping tools
require("stringi")
require("ggmap")        # Plotting of maps same as you would with ggplot2
require("maptools")     # Read, write, and handle Shapefiles in R
require("mapdata")      # Supplement to maps package
###### read the file
LSVFS.1 <- read.csv("CBC_LSVFS.DEL.csv")
# reformat dates
LSVFS.1$date.time <- as.POSIXct(CBC_LSVFS.1$date.time, 
                                format = "%m/%d/%y %H:%M", tz = "est")
# Rename columns
# Nameing that will differeintiate in plot legend
colnames(LSVFS.1) <- c("Date", 
                       "Rainfall", 
                       "Intensity", 
                       "Air.temp", 
                       "In1.temp", 
                       "In2.depth", 
                       "Out1.temp", 
                       "Out2.depth", 
                       "event")
## View
#View(LSVFS.1)

###### Exstract Date & Rainfall
RainData <- LSVFS.1 %>%
            select("Date", "Rainfall") 
#View(RainData)

###### Replace NA with 0
RainData[is.na(RainData)] <- 0
### View
#View(RainData)

###### Delineate Events
### Code found online
# column 1 is time stamp in POSIXct format, 
# column 2 is rainfall depth. My data is collected at two minute intervals; 
# Some data hourly. 
Rain_Over_0<- RainData[RainData[,2]!=0,]  
# Create vector increasing by 1 as Diff=>60 (Time specific) 
# change value of Diff here to change the MIT.
# input your value of MIT (in minutes) where the code says 30.
Rainindex<-c(0,cumsum(diff(Rain_Over_0[,1])>60)) 
# Split into list of events
RainEvents<-split(Rain_Over_0, Rainindex) # this returns a list of events. You can then use sapply functions to determine the rain statistics you need. 
#View(RainEvents)

###### Unlist events 
ul.RainEvents <- RainEvents %>%
                 unnest() 
# Error in function_list[[k]](value): Could not find function "unnest"



