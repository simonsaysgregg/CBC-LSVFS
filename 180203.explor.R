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
#Mapping tools
require("stringi")
require("ggmap")        # Plotting of maps same as you would with ggplot2
require("maptools")     # Read, write, and handle Shapefiles in R
require("mapdata")      # Supplement to maps package
####### read the file
CBC_LSVFS.1 <- read.csv("CBC_LSVFS.DEL.csv")

####### reformat dates
CBC_LSVFS.1$date.time <- as.POSIXct(CBC_LSVFS.1$date.time, format = "%m/%d/%y %H:%M", tz = "est")

### Rename columns
colnames(CBC_LSVFS.1) <- c("date.time", "rainfall", "intensity", "Air.temp", "In.temp", "In.depth", "Out.temp", "Out.depth", "event")

## View
#View(CBC_LSVFS.1)


######## melt the data and add new column names
###### Two step melting process
CBC_LSVFS.m <- melt(CBC_LSVFS.1, id = c("date.time", "rainfall", "intensity", "In.depth", "Out.depth", "event"))

## Confirm

#View(CBC_LSVFS.m)

### Rename columns

colnames(CBC_LSVFS.m) <- c("date.time", "rainfall", "intensity", "In.depth", "Out.depth", "event", "loc", "temp")

###### Second Melt

CBC_LSVFS.m <- melt(CBC_LSVFS.m, id = c("date.time", "rainfall", "intensity", "event", "loc", "temp"))

##### Confirm

#View(CBC_LSVFS.m)

##### Rename columns

colnames(CBC_LSVFS.m) <- c("date.time", "rainfall", "intensity", "event", "loc", "temp", "depth.loc", "depth")

### confirm

#View(CBC_LSVFS.m)

##### spliting data organizing, naming and adding back to data frame
##### for two different columns
##### For the temp locations
LSVFSsplit <- unlist(strsplit(as.character(CBC_LSVFS.m$loc), "\\."))
dim(LSVFSsplit) = c(2, nrow(CBC_LSVFS.m))
LSVFSsplit <- data.frame(t(LSVFSsplit))

### confirm

#View(LSVFSsplit)

#### Adding it back

CBC_LSVFS.m$loc <- LSVFSsplit[,1]

###### No need to add the second column of the split becasue only interested in locations
##### also temp column exist already containning only temperatures
###### No need#######CBC_LSVFS.m$temp <- LSVFSsplit[,2]

### For the second column
LSVFSsplit <- unlist(strsplit(as.character(CBC_LSVFS.m$depth.loc), "\\."))
dim(LSVFSsplit) = c(2, nrow(CBC_LSVFS.m))
LSVFSsplit <- data.frame(t(LSVFSsplit))

### confirm

#View(LSVFSsplit)

#### Adding it back

CBC_LSVFS.m$depth.loc <- LSVFSsplit[,1]


##### Rename columns


colnames(CBC_LSVFS.m) <- c("Date", "Rainfall", "Intensity", "Event", "Temperature_Location", "Temperature", "Depth_Location", "Depth")

##### Confirm

#View(CBC_LSVFS.m)


######## Send this to .csv

write.csv(CBC_LSVFS.m, file = "CBC_LSVFS.melted.csv")

###### Convert units to metric 
## rainfall = mm, temp = C, depth = cm

CBC_LSVFS.m.1 <- mutate(CBC_LSVFS.m, Rainfall = (Rainfall * 25.4), Intensity = (Intensity * 25.4), Temperature = (Temperature - 32) / 1.8, Depth = (Depth * 30.48))

### View

View(CBC_LSVFS.m.1)

########## Event Analysis

event.sum.1 <- ddply(CBC_LSVFS.m.1, .(Event, Temperature_Location), summarise, 
                     Start = min(Date), 
                     End = max(Date), 
                     Duration = max(Date) - min(Date), 
                     Rainfall_Accumulation = sum(Rainfall, na.rm = TRUE), 
                     Maximum_Intensity = max(Intensity), 
                     Average_Temperature = mean(Temperature), 
                     Median_Temperature = median(Temperature), 
                     Mininum_Temperature = min(Temperature), 
                     Maximum_Temperature = max(Temperature), 
                     Temperature_Variance = var(Temperature))


#### View
View(event.sum.1)

#### Remove event 0 and Air
#### Preparing to plot median & max temp box plots
event.sum.2 <- subset(event.sum.1, Event != 0 & Temperature_Location != "Air")

#View
View(event.sum.2)

## median & Max temp Scatter plot
ggplot(data = event.sum.2, aes(x = Start))+
  geom_point(aes(y = Median_Temperature, shape = Temperature_Location))+ 
  #geom_point(aes(y = Maximum_Temperature, shape = Temperature_Location))+
  ggtitle("Median Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")
#geom_boxplot(aes(x = Temperature_Location, y = Temperature))

##### scatter Maximum Event Temperature 
ggplot(data = event.sum.2, aes(x =Start))+
  geom_point(aes(y = Maximum_Temperature, shape = Temperature_Location))+
  ggtitle("Maximum Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")
#geom_boxplot(aes(x = Temperature_Location, y = Temperature))


##### Boxplot Median Event Temperature 
ggplot(data = event.sum.2)+
  geom_boxplot(aes(x = Temperature_Location, y = Median_Temperature))+
  ggtitle("Median Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")
#geom_boxplot(aes(x = Temperature_Location, y = Temperature))

##### Boxplot Maximum Event Temperature 
ggplot(data = event.sum.2)+
  geom_boxplot(aes(x = Temperature_Location, y = Maximum_Temperature))+
  ggtitle("Maximum Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")
#geom_boxplot(aes(x = Temperature_Location, y = Temperature))


#### Plot something
##### Average Temperatures colored by location
ggplot(data=event.sum.1,aes(x=Event,y=Average_Temperature, colour = Temperature_Location))+
  geom_point()

###### Histogram of rainfall amounts; adjust bin width to taste

ggplot(data = event.sum.1, aes(event.sum.1$Rainfall_Accumulation))+
  geom_histogram(binwidth = 50)



###### Find max precip event; Event == 34
event.sum.1[which.max(event.sum.1$Rainfall_Accumulation),]

######## Subset max precip event
max.precip.event <- subset(CBC_LSVFS.m.1, Event == 34)

##### Plot Depth
ggplot(data = max.precip.event, aes(x = Date, y = Depth, colour = Depth_Location))+
  geom_line()

##### Plot Temperature
ggplot(data = max.precip.event, aes(x = Date, y = Temperature, colour = Temperature_Location))+
  geom_line()

###### Find Max Maximum Temperature event; Event == 9
event.sum.1[which.max(event.sum.1$Maximum_Temperature),]

########## Subset maximum max tmep event
max.temp.event <- subset(CBC_LSVFS.m.1, Event == 9)

#### Plot temperature

ggplot(data = max.temp.event, aes(x = Date, y = Temperature, colour = Temperature_Location))+
  geom_line()

### Plot Depth

ggplot(data = max.temp.event, aes(x = Date, y = Depth, colour = Depth_Location))+
  geom_line()

##### Find max median event; Event == 14
event.sum.1[which.max((event.sum.1$Median_Temperature)),]

#### subset event
max.med.event <- subset(CBC_LSVFS.m.1, Event == 14)

###### Plot Temperature
ggplot(data = max.med.event, aes(x = Date, y = Temperature, colour = Temperature_Location))+
  geom_line()

###### Plot Depth
ggplot(data = max.med.event, aes(x = Date, y = Depth, colour = Depth_Location))+
  geom_line()

######## Subset/filter all data between and date range; Trying to get period before and after a storm

event.expl.1 <- CBC_LSVFS.m.1[(CBC_LSVFS.m.1$Date > "2017-09-10" & CBC_LSVFS.m.1$Date < "2017-09-13"), ]

###### Plot Temperature
ggplot(data = event.expl.1, aes(x = Date, y = Temperature, colour = Temperature_Location))+
  geom_line()

###### Plot Depth
ggplot(data = event.expl.1, aes(x = Date, y = Depth, colour = Depth_Location))+
  geom_line()

##### Event 1 subset and plotting

event.1 <- subset(CBC_LSVFS.m.1, Event == 1)

##### Plot temperature

ggplot(data = event.1, aes(x = Date, y = Temperature, colour = Temperature_Location))+
  geom_line()

###### Plot Depth
ggplot(data = event.1, aes(x = Date, y = Depth, colour = Depth_Location))+
  geom_line()

##### Event 2 subset and plotting
#### Good Thermograph

event.2 <- subset(CBC_LSVFS.m.1, Event == 2)

##### Plot temperature

ggplot(data = event.2, aes(x = Date, y = Temperature, colour = Temperature_Location))+
  geom_line()

###### Plot Depth
ggplot(data = event.2, aes(x = Date, y = Depth, colour = Depth_Location))+
  geom_line()

##### Event 3 subset and plotting
#### Good Thermograph
event.3 <- subset(CBC_LSVFS.m.1, Event == 3)

##### Plot temperature

ggplot(data = event.3, aes(x = Date, y = Temperature, colour = Temperature_Location))+
  geom_line()

###### Plot Depth
ggplot(data = event.3, aes(x = Date, y = Depth, colour = Depth_Location))+
  geom_line()

##### Event 4 subset and plotting
event.4 <- subset(CBC_LSVFS.m.1, Event == 4)
event.4.1 <- subset(CBC_LSVFS.m.1, Date >= as.POSIXct("2017-08-30") & Date <= as.POSIXct('2017-09-01'))
##### SimplePlot temperature
ggplot(data = event.4, aes(x = Date, y = Temperature, colour = Temperature_Location))+
  geom_line()
###### Plot Depth
ggplot(data = event.4, aes(x = Date, y = Depth, colour = Depth_Location))+
  geom_line()
###### Revalue data frame
View(event.4.1)
#########
revalue(event.4.1$Temperature_Location, c("Air"="Air Temp", "In"="In Temp", "Out"="Out Temp"))
View(event.4.1)
event.4.3 <- revalue(event.4.2$Depth_Location, c("In"="In Depth", "Out"="Out Depth"))
View(event.4.3)

##### Advance plot attempt
p <- ggplot()+
    ggtitle("Event Thermogrpah")
p <- p + geom_line(data = event.4.1, aes(x= Date, y = Temperature, linetype = Temperature_Location))
    
# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(data = event.4.1, aes(x= Date, y = Depth*1.0, linetype = Depth_Location))
  #scale_linetype_manual(values = c("dotted", "dashed", "twodash", "longdash", "dotdash"), labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))
    #discrete_scale(aesthetics = "linetype", scale_name = "", palette = 5, labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))
# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1.0, name = "Depth (cm)"))
# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Temperature (°C)",
              x = "Date")
p <- p + theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
p

##### Combining plots
##### Plot temperature

g.top <- ggplot(data = event.2, aes(x = Date)) +
            theme(plot.margin = unit(c(1,39,-25,1),units="points"),
            axis.title.y = element_text(vjust =0.25)) +
            geom_line(aes(y = Depth, linetype = Depth_Location))+
            geom_linerange(aes(y = Rainfall, ymin = 0, ymax = Rainfall))+
            #geom_line(aes(y = Depth, colour = Depth_Location))+
            labs(y = "Depth (cm)")

g.bottom <- ggplot(data = event.2, aes(x = Date))+
            theme(plot.margin = unit(c(0,5,1,1),units="points")) +
            labs(x = "Date", y = "Temperature, (C)")+
            geom_line(aes(y = Temperature, linetype = Temperature_Location))

grid.arrange(g.top, g.bottom, heights = c(1/5, 4/5))  


##### Another attempt
p <- ggplot(data = event.4.1, aes(x = Date))+
     ggtitle("Event Thermogrpah")
p <- p + geom_line(aes(y = Temperature, linetype = Temperature_Location))

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = Depth*1.0, linetype = Depth_Location))

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*1.0, name = "Depth (cm)"))

# modifying colours and theme options
#p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Temperature (°C)",
              x = "Date")
p <- p + theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
p


####### Verticl Rainfall plot

g.top <- ggplot(event.2, aes(x = Date, y = Rainfall, ymin=0, ymax=Rainfall)) +
  #theme_bw()+
  geom_linerange() +
  scale_y_continuous(limits = c(1.0,0), expand = c(0,0), trans = "reverse") +
  theme_classic()+
  theme(plot.margin = unit(c(5,139,-30,1),units="points"),
        axis.title.y = element_text(vjust =0.3)) +
  labs(y = "Rain (mm/hr)")

g.bottom <- ggplot(data = event.2, aes(x = Date))+
  geom_line(aes(y = Temperature, linetype = Temperature_Location))+
  #geom_line(aes(y = Depth, linetype = Depth_Location))+
  #scale_y_continuous(expand = c(0,0), limits = c(0,35)) +
  #theme_bw()+
  theme_classic()+
  theme(plot.margin = unit(c(0,5,5,8.5),units="points")) +
  labs(x = "Date", y = "Temperature, (C)")
  #geom_line(aes(y = Temperature, linetype = Temperature_Location))

grid.arrange(g.top, g.bottom, heights = c(1/5, 4/5)) 
############# Another attempt************

ggplot(event.2, aes(x = Date)) +
  geom_line(aes(y = Temperature, linetype = Temperature_Location)) +  #plot temp
  geom_line(aes(y = Depth, linetype = Depth_Location)) +  # plot depth
  ## specify our yaxis limits and remove any axis expansion
  scale_y_continuous(sec.axis = sec_axis(~.*.3), expand = c(0,0), limits = c(15,30), name = "Depth (cm)") +  
  labs(x = "Date", y = "Temperature (C)") +
  #theme_classic() +
 # theme(plot.background = element_rect(fill = "transparent"),
  #      plot.margin = unit(c(2,0,1,1),units="lines"))+
  theme(legend.position = "bottom", legend.title = element_blank())

####### advance ploting; two data sets, two axis
##### Some other plotting options This is a dummy graph>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#### Summarize data to begin scaling prep
View(event.2)

summary(event.2$Temperature)

summary(event.2$Depth)

##### Adding dummy column of scaled data

event.2$Depth2 <- event.2$Depth * (25-18)/max(event.2$Depth) + (18-min(event.2$Depth, na.rm = T))

summary(event.2$Depth2)
##### decided not to scale the data

#### setting scale limits
## What we want the axis to show
# What we want the axis to show
ylimits <- seq(0,20, by = 5)

## What we need to specify in our graph
ylimits2 <- ylimits * 25/max(event.2$Depth) + (25-min(event.2$Depth,na.rm=T))


g.bottom <- ggplot(event.2, aes(x = Date)) +
  geom_line(aes(y = Temperature, linetype = Temperature_Location)) +  #plot temp
  geom_line(aes(y = Depth2, linetype = Depth_Location)) +  # plot depth
  ## specify our yaxis limits and remove any axis expansion
  scale_y_continuous(expand = c(0,0), limits = c(15,30)) +  
  labs(x = "Date", y = "Temperature (C)") +
  theme_classic() +
  theme(legend.position = "bottom", legend.title = element_blank())
  theme(plot.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(2,0,1,1),units="lines"))
#g.bottom

g.y <- ggplot(event.2, aes(x = Date, y = Depth)) +
  theme_classic() + 
  geom_line(colour = "transparent") +
  scale_y_continuous(breaks = ylimits2, labels = ylimits, expand = c(0,0),
                     limits = c(10,35)) +
  scale_x_datetime(limits = c(min(event.2$Date),min(event.2$Date))) +
  labs(y = "Depth (cm)") +
  
  ## Adjust the placement of the y axis title
  theme(axis.title.y = element_text(vjust = 4.5, hjust = 0.88,angle = 270),  
        ## Adjust the justification of the y axis labels
        axis.text.y = element_text(hjust=-10),  
        ## Reverse the ticks to go on the other side
        axis.ticks.length = unit(-0.15,"cm"),
        ## Reverse spacing between ticks and text to move text to the right
        #axis.ticks.margin = unit(-0.5, "cm"), 
        axis.title.x = element_blank(), ## Remove all x-axis attributes
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        plot.background = element_rect(fill = "transparent"),
        plot.margin = unit(c(2,0,3.85,-1.5),units="lines"))
#g.y

vp1 <- viewport(width = 0.9, height = 1, x = 0, y = 0.5, just = c(0,0.5))
vp2 <- viewport(width = 0.1, height = 1, x = 0.9, y = 0.5,just = c(0,0.5))

print(g.bottom, vp=vp1)
print(g.y, vp=vp2)
############################ End
