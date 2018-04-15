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
LSVFS.1 <- read.csv("CBC_LSVFS.DEL.csv")
####### reformat dates
LSVFS.1$date.time <- as.POSIXct(CBC_LSVFS.1$date.time, format = "%m/%d/%y %H:%M", tz = "est")
### Rename columns
##### Nameing that will differeintiate in plot legend
colnames(LSVFS.1) <- c("Date", "Rainfall", "Intensity", "Air.temp", "In1.temp", "In2.depth", "Out1.temp", "Out2.depth", "event")
## View
#View(LSVFS.1)

##### Rplace nas with zero
LSVFS.1[is.na(LSVFS.1)] <- 0
### View
#View(LSVFS.1)
###### Convert units to metric 
## rainfall = mm, temp = C, depth = cm
LSVFS.2 <- mutate(LSVFS.1, Rainfall = (Rainfall * 25.4), 
                           Intensity = (Intensity * 25.4),
                           CumRain = cumsum(Rainfall), 
                           Air.temp = (Air.temp - 32) / 1.8, 
                           In1.temp = (In1.temp - 32) / 1.8, 
                           In2.depth = (In2.depth * 30.48), 
                           Out1.temp = (Out1.temp - 32) / 1.8, 
                           Out2.depth = (Out2.depth * 30.48))
### View
#View(LSVFS.2)

######## melt the data and add new column names
###### Two step melting process
LSVFS.m <- melt(LSVFS.2, id = c("Date", 
                                "Rainfall", 
                                "Intensity", 
                                "CumRain", 
                                "In2.depth", 
                                "Out2.depth", 
                                "event"))
## Confirm
#View(LSVFS.m)
### Rename columns
colnames(LSVFS.m) <- c("Date", 
                       "Rainfall", 
                       "Intensity",
                       "CumRain",
                       "In2.depth", 
                       "Out2.depth", 
                       "event", 
                       "Temperature_Location", 
                       "Temperature")

###### Second Melt
LSVFS.m1 <- melt(LSVFS.m, id = c("Date", 
                                 "Rainfall", 
                                 "Intensity", 
                                 "CumRain",
                                 "event", 
                                 "Temperature_Location", 
                                 "Temperature"))
##### Confirm
#View(LSVFS.m1)
##### Rename columns
colnames(LSVFS.m1) <- c("Date", 
                           "Rainfall", 
                           "Intensity",
                           "CumRain",
                           "event", 
                           "Temperature_Location", 
                           "Temperature",
                           "Depth_Location",
                           "Depth")
### confirm
#View(LSVFS.m1)

##### spliting data organizing, naming and adding back to data frame
##### for two different columns
##### For the temp locations
LSVFSsplit.1 <- unlist(strsplit(as.character(LSVFS.m1$Temperature_Location), "\\."))
dim(LSVFSsplit.1) = c(2, nrow(LSVFS.m1))
LSVFSsplit.1 <- data.frame(t(LSVFSsplit.1))
### confirm
#View(LSVFSsplit.1)
#### Adding it back
LSVFS.m1$Temperature_Location <- LSVFSsplit.1[,1]
###### No need to add the second column of the split becasue only interested in locations
##### also temp column exist already containning only temperatures
###### No need#######CBC_LSVFS.m$temp <- LSVFSsplit[,2]

### For the second column
LSVFSsplit.2 <- unlist(strsplit(as.character(LSVFS.m1$Depth_Location), "\\."))
dim(LSVFSsplit.2) = c(2, nrow(LSVFS.m1))
LSVFSsplit.2 <- data.frame(t(LSVFSsplit.2))
### confirm
#View(LSVFSsplit.1)
#### Adding it back
LSVFS.m1$Depth_Location <- LSVFSsplit.2[,1]
##### Rename columns
colnames(LSVFS.m1) <- c("Date", 
                        "Rainfall",
                        "Intensity",
                        "CumRain",
                        "Event", 
                        "Temperature_Location", 
                        "Temperature", 
                        "Depth_Location", 
                        "Depth")
##### Confirm
#View(LSVFS.m1)
######## Send this to .csv
write.csv(LSVFS.m1, file = "LSVFS.melted.csv")

########## Rainfall Histogram
ggplot(summary.2, aes())+
  geom_histogram(aes(x = Rainfall_Accumulation), binwidth = 5 )+
  ggtitle("CBC Rainfall Accumulation Histogram")+
  labs( x = "Rainfall Accumulation (mm)", y = "Event Count")+
  theme(plot.title = element_text(hjust = 0.5))



### Subsetting events
##### Event 1 subset and plotting
evnt.1 <- subset(LSVFS.m1, Event == 1)
evnt.1.1 <- subset(LSVFS.m1, Date >= as.POSIXct("2017-08-30 12:00") & Date <= as.POSIXct("2017-08-31 8:00"))
##### Plot temperature
ggplot(data = evnt.1, aes(x = Date, y = Temperature, colour = Temperature_Location))+
  geom_line()
###### Plot Depth
ggplot(data = evnt.1, aes(x = Date, y = Depth, colour = Depth_Location))+
  geom_line()
##### Advance plot attempt Event1
ggplot()+
  ggtitle("August 30th Event Thermogrpah")+
  geom_line(data = evnt.1.1, aes(x= Date, y = Temperature, linetype = Temperature_Location))+
  geom_line(data = evnt.1.1, aes(x= Date, y = Depth*1.0, linetype = Depth_Location))+
  scale_linetype_manual(values = c("dotted", "dashed", "twodash", "longdash", "dotdash"), labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  #discrete_scale(aesthetics = "linetype", scale_name = "", palette = 5, labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  scale_y_continuous(sec.axis = sec_axis(~.*1.0, name = "Depth (cm)"))+
  #scale_colour_manual(values = c("blue", "red"))
  labs(y = "Temperature (°C)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

##### Event 1+2 subset and plotting
#### Good Thermograph
#evnt.2 <- subset(LSVFS.m1, Event == 2)
evnt.2.1 <- subset(LSVFS.m1, Date >= as.POSIXct("2017-08-30 12:00") & Date <= as.POSIXct("2017-08-31 8:00"))
##### Plot temperature
#ggplot(data = evnt.2, aes(x = Date, y = Temperature, colour = Temperature_Location))+
 # geom_line()
###### Plot Depth
#ggplot(data = evnt.2, aes(x = Date, y = Depth, colour = Depth_Location))+
 # geom_line()
##### Advance plot attempt Event2
ggplot()+
  ggtitle("August 30th Event Thermograph")+
  geom_line(data = evnt.2.1, aes(x= Date, y = Temperature, linetype = Temperature_Location))+
  geom_line(data = evnt.2.1, aes(x= Date, y = Depth*1.0, linetype = Depth_Location))+
  scale_linetype_manual(values = c("dotted", "dashed", "twodash", "longdash", "dotdash"), labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  #discrete_scale(aesthetics = "linetype", scale_name = "", palette = 5, labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  scale_y_continuous(sec.axis = sec_axis(~.*1.0, name = "Depth (cm)"))+
  #scale_colour_manual(values = c("blue", "red"))
  labs(y = "Temperature (°C)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))


##### Event 3 + 4 subset and plotting
#### Good Thermograph
#evnt.3 <- subset(LSVFS.m1, Event == 3)
evnt.3.1 <- subset(LSVFS.m1, Date >= as.POSIXct("2017-08-31 8:00") & Date <= as.POSIXct("2017-09-01 4:00"))
##### Plot temperature
#ggplot(data = evnt.3, aes(x = Date, y = Temperature, colour = Temperature_Location))+
 # geom_line()
###### Plot Depth
#ggplot(data = evnt.3, aes(x = Date, y = Depth, colour = Depth_Location))+
 # geom_line()
##### Advance plot attempt Event3+4
ggplot()+
  ggtitle("August 31th Event Thermograph")+
  geom_line(data = evnt.3.1, aes(x= Date, y = Temperature, linetype = Temperature_Location))+
  geom_line(data = evnt.3.1, aes(x= Date, y = Depth*1.0, linetype = Depth_Location))+
  scale_linetype_manual(values = c("dotted", "dashed", "twodash", "longdash", "dotdash"), labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  #discrete_scale(aesthetics = "linetype", scale_name = "", palette = 5, labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  scale_y_continuous(sec.axis = sec_axis(~.*1.0, name = "Depth (cm)"))+
  #scale_colour_manual(values = c("blue", "red"))
  labs(y = "Temperature (°C)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))


##### Event 4 subset and plotting
#evnt.4 <- subset(LSVFS.m1, Event == 4)
#evnt.4.1 <- subset(LSVFS.m1, Date >= as.POSIXct("2017-08-30 12:00") & Date <= as.POSIXct("2017-08-31 8:00"))
##### SimplePlot temperature
#ggplot(data = evnt.4, aes(x = Date, y = Temperature, colour = Temperature_Location))+
 # geom_line()
###### Plot Depth
#ggplot(data = evnt.4, aes(x = Date, y = Depth, colour = Depth_Location))+
#  geom_line()
##### Advance plot attempt Event4
#ggplot()+
 # ggtitle("August 30th Event Thermograph")+
  #geom_line(data = evnt.4.1, aes(x= Date, y = Temperature, linetype = Temperature_Location))+
#  geom_line(data = evnt.4.1, aes(x= Date, y = Depth*1.0, linetype = Depth_Location))+
 # scale_linetype_manual(values = c("dotted", "dashed", "twodash", "longdash", "dotdash"), labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  #discrete_scale(aesthetics = "linetype", scale_name = "", palette = 5, labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
#  scale_y_continuous(sec.axis = sec_axis(~.*1.0, name = "Depth (cm)"))+
  #scale_colour_manual(values = c("blue", "red"))
 # labs(y = "Temperature (°C)", x = "Date")+
  #theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))


##### Event 5+6 subset and plotting
#evnt.5 <- subset(LSVFS.m1, Event == 5)
evnt.5.1 <- subset(LSVFS.m1, Date >= as.POSIXct("2017-09-01 4:00") & Date <= as.POSIXct("2017-09-01 20:00"))
##### SimplePlot temperature
#ggplot(data = evnt.5, aes(x = Date, y = Temperature, colour = Temperature_Location))+
 # geom_line()
###### Plot Depth
#ggplot(data = evnt.5, aes(x = Date, y = Depth, colour = Depth_Location))+
 # geom_line()
##### Advance plot attempt Event4
ggplot()+
  ggtitle("September 1st Event Thermograph")+
  geom_line(data = evnt.5.1, aes(x= Date, y = Temperature, linetype = Temperature_Location))+
  geom_line(data = evnt.5.1, aes(x= Date, y = Depth*1.0, linetype = Depth_Location))+
  scale_linetype_manual(values = c("dotted", "dashed", "twodash", "longdash", "dotdash"), labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  #discrete_scale(aesthetics = "linetype", scale_name = "", palette = 5, labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  scale_y_continuous(sec.axis = sec_axis(~.*1.0, name = "Depth (cm)"))+
  #scale_colour_manual(values = c("blue", "red"))
  labs(y = "Temperature (°C)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))


##### Event 9+10 subset and plotting
#evnt.9 <- subset(LSVFS.m1, Event == 9)
evnt.9.1 <- subset(LSVFS.m1, Date >= as.POSIXct("2017-09-05 4:00") & Date <= as.POSIXct("2017-09-06 15:00"))
##### SimplePlot temperature
#ggplot(data = evnt.9, aes(x = Date, y = Temperature, colour = Temperature_Location))+
 #geom_line()
###### Plot Depth
#ggplot(data = evnt.9, aes(x = Date, y = Depth, colour = Depth_Location))+
 #geom_line()
##### Advance plot attempt Event4
ggplot()+
  ggtitle("September 5th Event Thermograph")+
  geom_line(data = evnt.9.1, aes(x= Date, y = Temperature, linetype = Temperature_Location))+
  geom_line(data = evnt.9.1, aes(x= Date, y = Depth*1.0, linetype = Depth_Location))+
  scale_linetype_manual(values = c("dotted", "dashed", "twodash", "longdash", "dotdash"), labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  #discrete_scale(aesthetics = "linetype", scale_name = "", palette = 5, labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  scale_y_continuous(sec.axis = sec_axis(~.*1.0, name = "Depth (cm)"))+
  #scale_colour_manual(values = c("blue", "red"))
  labs(y = "Temperature (°C)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))

##### Event 24 subset and plotting
#evnt.24 <- subset(LSVFS.m1, Event == 14)
evnt.24 <- subset(LSVFS.m1, Date >= as.POSIXct("2017-09-10 4:00") & Date <= as.POSIXct("2017-09-12 15:00"))
##### SimplePlot temperature
#ggplot(data = evnt.24, aes(x = Date, y = Temperature, colour = Temperature_Location))+
#geom_line()
###### Plot Depth
#ggplot(data = evnt.24, aes(x = Date, y = Depth, colour = Depth_Location))+
#geom_line()
##### Advance plot attempt Event4
ggplot()+
  ggtitle("Event 24 Thermograph")+
  geom_line(data = evnt.24, aes(x= Date, y = Temperature, linetype = Temperature_Location))+
  geom_line(data = evnt.24, aes(x= Date, y = Depth*1.0, linetype = Depth_Location))+
  scale_linetype_manual(values = c("dotted", "dashed", "twodash", "longdash", "dotdash"), labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  #discrete_scale(aesthetics = "linetype", scale_name = "", palette = 5, labels = c("Air", "In Temp", "In Depth", "Out Temp", "Out Depth"))+
  scale_y_continuous(sec.axis = sec_axis(~.*1.0, name = "Depth (cm)"))+
  #scale_colour_manual(values = c("blue", "red"))
  labs(y = "Temperature (°C)", x = "Date")+
  theme(legend.position = "bottom", legend.title = element_blank(), plot.title = element_text(hjust = 0.5))



####### Event Summary Analysis
sum.event <- ddply(LSVFS.m1, .(Event, Temperature_Location), summarise, 
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
#View
#View(sum.event)
###### Remove event zero
sum.event.1 <- subset(sum.event, Event != 0)
#View
#View(sum.event.1)
###### Remove Temperature_Location == Air
sum.event.2 <- subset(sum.event, Temperature_Location != "Air")
#View
#View(sum.event.2)

#### Inlet/Outlet med/max temp scatter plot
###### Scatter Plot
ggplot()+
  geom_point(data = sum.event.2, aes(x = Start, y = Median_Temperature, shape = Temperature_Location))+ 
  geom_point(data = event.sum.2 , aes(x = Start, y = Maximum_Temperature, shape = Temperature_Location))+
  scale_shape_manual(values = c(0,1,15,16), labels = c("Med In", "Max In", "Med Out", "Max Out"))+
  ggtitle("Median & Maximum Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Date", y = "Temperature (°C)")
#geom_boxplot(aes(x = Temperature_Location, y = Temperature))

#View
#View(summary.stats.2)

### Wilcoxon RankSumming
##### Test of significance Median Inlet
wilcox.test(x = summary.stats.2$med.in, mu = 21, conf.int = TRUE, conf.level = 0.95)
### cooler than 21
##### Test of significance Maximum Inlet
wilcox.test(x = summary.stats.2$max.in, mu = 21, conf.int = TRUE, conf.level = 0.95)
### cooler than 21
### Wilcoxon RankSumming
##### Test of significance Median Outlet
wilcox.test(x = summary.stats.2$med.out, mu = 21, conf.int = TRUE, conf.level = 0.95)
### cooler than 21
##### Test of significance Maximum Outlet let
wilcox.test(x = summary.stats.2$max.out, mu = 21, conf.int = TRUE, conf.level = 0.95)
### cooler than 21

##### Differecne of medians
##### Test of significance Median Inlet & Outlet
wilcox.test(x = summary.stats.2$med.in, y = summary.stats.2$med.out, paired = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.3459
# 95% CI -0.1903 - 0.4769

##### Test of significance Maximum Inlet & Outlet
wilcox.test(x = summary.stats.2$max.in, y = summary.stats.2$max.out, paried = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.8755
# 95% CI -1.7171 - 1.7205


######## Recreating event summary table
View(LSVFS.2)
### Summary
summary.1 <- ddply(LSVFS.2, .(event), summarise, 
                   Start = min(Date), 
                   End = max(Date), 
                   Duration = max(Date) - min(Date), 
                   Rainfall_Accumulation = sum(Rainfall, na.rm = TRUE), 
                   Maximum_Intensity = max(Intensity), 
                   Median_In = median(In1.temp),
                   Max_In = max(In1.temp),
                   Variance_In = var(In1.temp),
                   Median_Out = median(Out1.temp),
                   Max_Out = max(Out1.temp),
                   Variance_Out = var(Out1.temp))
#View
View(summary.1)
### Remove event 0
summary.2 <- subset(summary.1, event != 0)
View(summary.2)
#### Removing small events <1.5
summary.3 <- subset(summary.2, Rainfall_Accumulation > 1.55)
View(summary.3)
length(summary.3)

### Wilcoxon RankSumming
##### Test of significance Median Inlet
wilcox.test(x = summary.3$Median_In, mu = 21, conf.int = TRUE, conf.level = 0.95)
### cooler than 21
##### Test of significance Maximum Inlet
wilcox.test(x = summary.3$Max_In, mu = 21, conf.int = TRUE, conf.level = 0.95)
### cooler than 21
### Wilcoxon RankSumming
##### Test of significance Median Outlet
wilcox.test(x = summary.3$Median_Out, mu = 21, conf.int = TRUE, conf.level = 0.95)
### cooler than 21
##### Test of significance Maximum Outlet let
wilcox.test(x = summary.3$Max_Out, mu = 21, conf.int = TRUE, conf.level = 0.95)
### cooler than 21

##### Differecne of medians
##### Test of significance Median Inlet & Outlet
wilcox.test(x = summary.3$Median_In, y = summary.3$Median_Out, paired = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.3459
# 95% CI -0.1903 - 0.4769

##### Test of significance Maximum Inlet & Outlet
wilcox.test(x = summary.3$Max_In, y = summary.3$Max_Out, paried = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.8755
# 95% CI -1.7171 - 1.7205


#### signifiacne test of in/out variance
##### Differecne of medians
##### Test of significance Median variance Inlet & Outlet
wilcox.test(x = summary.3$Variance_In, y = summary.3$Variance_Out, paired = TRUE, conf.int = TRUE, conf.level = 0.95)
### not significantly different pval = 0.99
#View(summary.3)


#### Preparing to plot median & max temp box plots
View(event.sum.2)
event.sum.short <- subset(sum.event.2,
                          Event == 3 |
                          Event == 5 |
                          Event == 7 |
                          Event == 9 |
                          Event == 11 |
                          Event == 13 |
                          Event == 14 |
                          Event == 22 |
                          Event == 25 |
                          Event == 26 |
                          Event == 28 |
                          Event == 31 |
                          Event == 34 |
                          Event == 36 |
                          Event == 37 |
                          Event == 42 |
                          Event == 54)
View(event.sum.short)

##### Boxplot Median Event Temperature 
ggplot(data = event.sum.short)+
  geom_boxplot(aes(x = Temperature_Location, y = Median_Temperature))+
  scale_x_discrete(labels = c("In", "Out"))+
  ggtitle("Median Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")


##### Boxplot Maximum Event Temperature 
ggplot(data = event.sum.short)+
  geom_boxplot(aes(x = Temperature_Location, y = Maximum_Temperature))+
  scale_x_discrete(labels = c("In", "Out"))+
  ggtitle("Maximum Event Temperatures")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(5,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")


############# Probaility exceedance plot
#### select variables from event summary table short
LSVFS.ex <- (summary.3[ ,c("Median_In", "Max_In", "Median_Out", "Max_Out")]) 
View(LSVFS.ex)                
########### sort columns
LSVFS.ex <- mutate(LSVFS.ex, 
                 Median_In = sort(Median_In, decreasing = TRUE, na.last = TRUE),
                 Max_In = sort(Max_In, decreasing = TRUE, na.last = TRUE),
                 Median_Out = sort(Median_Out, decreasing = TRUE, na.last = TRUE),
                 Max_Out = sort(Max_Out, decreasing = TRUE, na.last = TRUE))
View(LSVFS.ex)
############ Rank 
LSVFS.ex <- mutate(LSVFS.ex, 
                 In.med.1 = rank(desc(Median_In), na.last = TRUE),
                 In.max.1 = rank(desc(Max_In), na.last = TRUE),
                 Out.med.1 = rank(desc(Median_Out), na.last = TRUE),
                 Out.max.1 = rank(desc(Max_In), na.last = TRUE))
View(LSVFS.ex)
########### Calculate probabiltiy
LSVFS.ex <- mutate(LSVFS.ex, 
                 In.med.2 = (In.med.1 - 0.375) / (13 + 1 - (2 * 0.375)),
                 In.max.2 = (In.max.1 - 0.375) / (13 + 1 - (2 * 0.375)),
                 Out.med.2 = (Out.med.1 - 0.375) / (13 + 1 - (2 * 0.375)),
                 Out.max.2 = (Out.max.1 - 0.375) / (13 + 1 - (2 * 0.375)))
View(LSVFS.ex)
############# Exceedance probability plots
##### Inlet
ggplot(data = LSVFS.ex)+
  geom_point(aes(x = Median_In, y = In.med.2, shape = "Inlet"))+ 
  geom_point(aes(x = Median_Out, y = Out.med.2, shape = "Outlet"))+
  scale_shape_manual(values = c(15,16))+
  ggtitle("Median Temperature Probability Exceedance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_y_continuous(limits = c(0.0,1.0), expand = c(0,0)) +
  scale_x_continuous(limits = c(10.0, 32.5), expand = c(0,0))+
  labs(x = "Temperature (°C)", y = "Probability")
##### Outlet
ggplot(data = LSVFS.ex)+
  geom_point(aes(x = Max_In, y = In.max.2, shape = "Inlet"))+ 
  geom_point(aes(x = Max_Out, y = Out.max.2, shape = "Outlet"))+
  scale_shape_manual(values = c(15,16))+
  ggtitle("Maximum Temperature Probability Exceedance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_y_continuous(limits = c(0.0,1.0), expand = c(0,0)) +
  scale_x_continuous(limits = c(10.0, 32.5), expand = c(0,0))+
  labs(x = "Temperature (°C)", y = "Probability")
##### Inlet + Outlet 
ggplot(data = LSVFS.ex)+
  geom_point(aes(x = Median_In, y = In.med.2, shape = "Median Inlet"))+ 
  geom_point(aes(x = Median_Out, y = Out.med.2, shape = "Median Outlet"))+
  geom_point(aes(x = Max_In, y = In.max.2, shape = "Maximum Inlet"))+ 
  geom_point(aes(x = Max_Out, y = Out.max.2, shape = "Maximum Outlet"))+
  scale_shape_manual(values = c(0,1,15,16))+
  ggtitle("Inlet & Outlet Temperature Probability Exceedance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_y_continuous(limits = c(0.0,1.0), expand = c(0,0)) +
  scale_x_continuous(limits = c(10.0, 32.5), expand = c(0,0))+
  labs(x = "Temperature (°C)", y = "Probability")
