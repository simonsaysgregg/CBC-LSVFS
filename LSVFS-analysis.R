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
require("grid")
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

## 9/5 Event
## Plot depth and rainfall
# Plot 1
plot951 <- (LSVFS.m) %>%
  select(date.time,
         In.depth,
         Out.depth) 
colnames(plot951) <- c("date.time",
                            "In",
                            "Out")
# Plot2
plot952 <- (LSVFS.m) %>%
  select(date.time,
         Air.temp,
         In.temp,
         Out.temp) 
colnames(plot952) <- c("date.time",
                       "Air",
                       "In",
                       "Out")
# Prep plotting dataset1
plot951 <- (plot951) %>%
  subset(date.time >= as.POSIXct("2017-09-05 09:00:00") & date.time <= as.POSIXct("2017-09-06 16:00:00")) %>%
  melt(id = "date.time")
# View(plot951)
# Prep plotting dataset2
plot952 <- (plot952) %>%
  subset(date.time >= as.POSIXct("2017-09-05 09:00:00") & date.time <= as.POSIXct("2017-09-06 16:00:00")) %>%
  melt(id = "date.time")
# View(plot952)
# plot1
plot1 <-ggplot(data = plot951)+
            geom_line(aes(x = date.time, y = value, color = variable))+
            labs(x = "Date", y = "Depth (cm)")+
            theme(legend.position = "bottom", 
                  legend.title = element_blank())+
            scale_x_datetime(date_labels = "%m/%d %H:%M", date_breaks = "6 hours")
# Plot2
plot2 <-ggplot(data = plot952)+
            geom_line(aes(x = date.time, y = value, color = variable))+
            labs(x = "Date", y = "Temperature (°C)")+
            theme(legend.position = "bottom", 
                  legend.title = element_blank())+
            scale_x_datetime(date_labels = "%m/%d %H:%M", date_breaks = "6 hours")
grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))


## 8/30 Event
## Plot depth and rainfall
# Plot 1
plot8301 <- (LSVFS.m) %>%
  select(date.time,
         In.depth,
         Out.depth) 
colnames(plot8301) <- c("date.time",
                       "In",
                       "Out")
# Plot2
plot8302 <- (LSVFS.m) %>%
  select(date.time,
         Air.temp,
         In.temp,
         Out.temp) 
colnames(plot8302) <- c("date.time",
                       "Air",
                       "In",
                       "Out")
# Prep plotting dataset1
plot8301 <- (plot8301) %>%
  subset(date.time >= as.POSIXct("2017-08-30 09:00:00") & date.time <= as.POSIXct("2017-08-31 08:00:00")) %>%
  melt(id = "date.time")
# View(plot8301)
# Prep plotting dataset2
plot8302 <- (plot8302) %>%
  subset(date.time >= as.POSIXct("2017-08-30 09:00:00") & date.time <= as.POSIXct("2017-08-31 08:00:00")) %>%
  melt(id = "date.time")
# View(plot8302)
# plot1
plot1 <-ggplot(data = plot8301)+
  geom_line(aes(x = date.time, y = value, color = variable))+
  labs(x = "Date", y = "Depth (cm)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_x_datetime(date_labels = "%m/%d %H:%M", date_breaks = "6 hours")
# Plot2
plot2 <-ggplot(data = plot8302)+
  geom_line(aes(x = date.time, y = value, color = variable))+
  labs(x = "Date", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_x_datetime(date_labels = "%m/%d %H:%M", date_breaks = "6 hours")
grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

## 8/31 Event
## Plot depth and rainfall
# Plot 1
plot8311 <- (LSVFS.m) %>%
  select(date.time,
         In.depth,
         Out.depth) 
colnames(plot8311) <- c("date.time",
                        "In",
                        "Out")
# Plot2
plot8312 <- (LSVFS.m) %>%
  select(date.time,
         Air.temp,
         In.temp,
         Out.temp) 
colnames(plot8312) <- c("date.time",
                        "Air",
                        "In",
                        "Out")
# Prep plotting dataset1
plot8311 <- (plot8311) %>%
  subset(date.time >= as.POSIXct("2017-08-31 08:00:00") & date.time <= as.POSIXct("2017-09-01 02:00:00")) %>%
  melt(id = "date.time")
# View(plot8311)
# Prep plotting dataset2
plot8312 <- (plot8312) %>%
  subset(date.time >= as.POSIXct("2017-08-31 08:00:00") & date.time <= as.POSIXct("2017-09-01 02:00:00")) %>%
  melt(id = "date.time")
# View(plot8312)
# plot1
plot1 <-ggplot(data = plot8311)+
  geom_line(aes(x = date.time, y = value, color = variable))+
  labs(x = "Date", y = "Depth (cm)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_x_datetime(date_labels = "%m/%d %H:%M", date_breaks = "6 hours")
# Plot2
plot2 <-ggplot(data = plot8312)+
  geom_line(aes(x = date.time, y = value, color = variable))+
  labs(x = "Date", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_x_datetime(date_labels = "%m/%d %H:%M", date_breaks = "6 hours")
grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

## Additional statistical analysis

## Split into list of events
LSVFSevents <- split(LSVFS.m, LSVFS.m$event) 
# Returns a list of events 
# View(LSVFSevents)

## Calculates mean of Duration & Rainfall Accumulaiton 
# Returns a data frame of values same length as list
LSVFSsum <- LSVFSevents %>%
  map_df(function(df) {summarise(df, Date = min(date.time), 
                                 Duration = ((max(date.time)-min(date.time))/3600),
                                 Accumulation = sum(rainfall, na.rm = TRUE),
                                 max.intensity = max(intensity, na.rm = TRUE),
                                 medinT = median(In.temp, na.rm = TRUE), 
                                 maxinT = max(In.temp, na.rm = TRUE),
                                 medoutT = median(Out.temp, na.rm = TRUE), 
                                 maxoutT = max(Out.temp, na.rm = TRUE),
                                 varinT = var(In.temp, na.rm =TRUE),
                                 varoutT = var(Out.temp, na.rm = TRUE))})
# View(LSVFSsum)

## Breaking events into pre and post 
## subset to provide additional hydrology analsis
LSVFS_pre1012 <- (LSVFSsum[-c(1),]) %>%
  subset(Date <= "2017/10/12" & Accumulation >= 1.89) 
#View(LSVFS_pre1012)
## subset to provide additional hydrology analsis
LSVFS_post1012 <- (LSVFSsum[-c(1),]) %>%
  subset(Date >= "2017/10/12" & Accumulation >= 1.89) 
#View(LSVFS_post1012)

## Wilcoxon test
# median in
wilcox.test(LSVFS_pre1012$medinT, alternative = "t", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# max in
wilcox.test(LSVFS_pre1012$maxinT, alternative = "t", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# median out
wilcox.test(LSVFS_pre1012$medoutT, alternative = "t", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# max out
wilcox.test(LSVFS_pre1012$maxoutT, alternative = "t", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# Paired test
# median in to out
wilcox.test(LSVFS_pre1012$medinT, LSVFS_pre1012$medoutT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# max in to out
wilcox.test(LSVFS_pre1012$maxinT, LSVFS_pre1012$maxoutT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# Difference of variance
wilcox.test(LSVFS_pre1012$varinT, LSVFS_pre1012$varoutT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)

## Wilcoxon test
# median in
wilcox.test(LSVFS_post1012$medinT, alternative = "l", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# max in
wilcox.test(LSVFS_post1012$maxinT, alternative = "l", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# median out
wilcox.test(LSVFS_post1012$medoutT, alternative = "l", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# max out
wilcox.test(LSVFS_post1012$maxoutT, alternative = "l", mu = 21, paired = FALSE, conf.int = TRUE, conf.level = 0.95)
# Paired test
# median in to out
wilcox.test(LSVFS_post1012$medinT, LSVFS_post1012$medoutT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# max in to out
wilcox.test(LSVFS_post1012$maxinT, LSVFS_post1012$maxoutT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)
# Difference of variance
wilcox.test(LSVFS_post1012$varinT, LSVFS_post1012$varoutT, alternative = "t", paired = TRUE, conf.int = TRUE, conf.level = 0.95)

## box plots of pre-1012
# median data
LSVFSpre1012med_box <- (LSVFS_pre1012) %>%
  select(medinT,
         medoutT) %>%
  melt()
#View(LSVFSpre1012med_box)
# maximum data
LSVFSpre1012max_box <- (LSVFS_pre1012) %>%
  select(maxinT,
         maxoutT) %>%
  melt()
#View(LSVFSpre1012max_box)

# plot median temps
ggplot(data = LSVFSpre1012med_box)+
  geom_boxplot(aes(x = variable, y = value))+
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  scale_x_discrete(labels = c("In", "Out"))+
  scale_y_continuous(limits = c(10,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())

# plot max temps
ggplot(data = LSVFSpre1012max_box)+
  geom_boxplot(aes(x = variable, y = value))+
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  scale_x_discrete(labels = c("In", "Out"))+
  scale_y_continuous(limits = c(10,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())

## box plots of post-1012
# median data
LSVFSpost1012med_box <- (LSVFS_post1012) %>%
  select(medinT,
         medoutT) %>%
  melt()
#View(LSVFSpost1012med_box)
# maximum data
LSVFSpost1012max_box <- (LSVFS_post1012) %>%
  select(maxinT,
         maxoutT) %>%
  melt()
#View(LSVFSpost1012max_box)

# plot median temps
ggplot(data = LSVFSpost1012med_box)+
  geom_boxplot(aes(x = variable, y = value))+
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  scale_x_discrete(labels = c("Inlet", "Outlet"))+
  scale_y_continuous(limits = c(10,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())

# plot max temps
ggplot(data = LSVFSpost1012max_box)+
  geom_boxplot(aes(x = variable, y = value))+
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  scale_x_discrete(labels = c("Inlet", "Outlet"))+
  scale_y_continuous(limits = c(10,30), expand = c(0,0)) +
  labs(x = "Temperature Location", y = "Temperature (°C)")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())

# Scatter plot of all medians and maximums
tot.scat <- (LSVFSsum) %>%
  subset(Accumulation >= 1.89)%>%
  select(Date,
         medinT,
         maxinT,
         medoutT,
         maxoutT) 
colnames(tot.scat) <- c("Date",
                        "Median Inlet",
                        "Maximum Inlet",
                        "Median Outlet",
                        "Maximum Outlet")
# View(tot.scat)
# Melt data set
tot.scat <- (tot.scat) %>%
  melt(id = "Date")
# Plot
ggplot(data = tot.scat, aes(x = Date))+
  geom_point(aes(y = value, shape = variable))+ 
  geom_hline(aes(yintercept = 21, color = "Trout Threshold"))+
  geom_vline(aes(xintercept = as.numeric(as.POSIXct("2017-10-12")), color = "Analysis Division"))+
  scale_shape_manual(values = c(0,1,15,16))+
  theme(legend.position = "bottom", 
        legend.title = element_blank())+
  scale_y_continuous(limits = c(5,30), expand = c(0,0))+
  scale_x_datetime(date_labels = "%m/%d", date_breaks = "10 days")+
  labs(x = "Date", y = "Temperature (°C)")

## Probaility exceedance plot
## select variables from event summary table short
prob.plot <- (LSVFS_pre1012) %>%
  select(Date,
         medinT,
         maxinT,
         medoutT,
         maxoutT) 
# View(prob.plot)                
########### sort columns
prob.plot <- mutate(prob.plot, 
                    medinT = sort(medinT, decreasing = TRUE, na.last = TRUE),
                    maxinT = sort(maxinT, decreasing = TRUE, na.last = TRUE),
                    medoutT = sort(medoutT, decreasing = TRUE, na.last = TRUE),
                    maxoutT = sort(maxoutT, decreasing = TRUE, na.last = TRUE))
#View(prob.plot)
############ Rank 
prob.plot <- mutate(prob.plot, 
                   In.med.1 = rank(desc(medinT), na.last = TRUE),
                   In.max.1 = rank(desc(maxinT), na.last = TRUE),
                   Out.med.1 = rank(desc(medoutT), na.last = TRUE),
                   Out.max.1 = rank(desc(maxoutT), na.last = TRUE))
#View(prob.plot)
########### Calculate probabiltiy
prob.plot <- mutate(prob.plot, 
                   In.med.2 = (In.med.1 - 0.375) / (10 + 1 - (2 * 0.375)),
                   In.max.2 = (In.max.1 - 0.375) / (10 + 1 - (2 * 0.375)),
                   Out.med.2 = (Out.med.1 - 0.375) / (10 + 1 - (2 * 0.375)),
                   Out.max.2 = (Out.max.1 - 0.375) / (10 + 1 - (2 * 0.375)))
#View(prob.plot)

##### Inlet + Outlet 
ggplot(data = prob.plot)+
  geom_point(aes(x = medinT, y = In.med.2, shape = "Median Inlet"))+ 
  geom_point(aes(x = medoutT, y = Out.med.2, shape = "Median Outlet"))+
  geom_point(aes(x = maxinT, y = In.max.2, shape = "Maximum Inlet"))+ 
  geom_point(aes(x = maxoutT, y = Out.max.2, shape = "Maximum Outlet"))+
  geom_vline(aes(xintercept = 21, color = "Trout Threshold"))+
  scale_shape_manual(values = c(0,1,15,16))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  scale_y_continuous(limits = c(0.0,1.0), expand = c(0,0)) +
  scale_x_continuous(limits = c(10.0, 32.5), expand = c(0,0))+
  labs(x = "Temperature (°C)", y = "Probability")

## How many events greater than 1in
inch.st <- (LSVFSsum) %>%
  subset(Accumulation >= 25.4)
## Rainfall histogram
rain.acc <- (LSVFSsum) %>%
  select(Accumulation)
ggplot(data = rain.acc, aes(x = Accumulation))+
  geom_histogram(binwidth = 8.128)+
  labs(x = "Rainfall Accumulation (mm)", y = "Discrete Events (count)")