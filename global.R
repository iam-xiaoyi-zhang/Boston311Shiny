library(rgdal)
library(ggmap)
library(ggplot2)
library(scales)
library(shiny)
library(ggthemes)
library(grid)
library(Rmisc)
library(ggthemes)
library(plotly)

#### Metrics efficiency
qos2014 <- read.csv("MetricsData/qos2014.csv", stringsAsFactors=FALSE)
# Load data
qos2014 <- qos2014[-c(which(is.na(qos2014$CmO))),]
qos2014 <- qos2014[-c(which(is.na(qos2014$TmO))),]
qos2014$CdT <- qos2014$CmO/qos2014$TmO
#attach(qos2014)
# Get the department names
dpt = unique(qos2014$SUBJECT)


#### Bubble Charts
qos.depar <- read.csv("ScatterBubbleData/qos2014_department_hours.csv", header = T)
census <- read.csv("ScatterBubbleData/census.csv", header = T)
colnames(qos.depar) <- c("id", "neighborhood", "time_consume", "efficiency", "On_time_Rate", "Requests_Volume", "department" )
#We only focus on density and median income ($) of each neighborhood when we're plotting
#So we extract the information from completed census data
colnames(census) <- c("neighborhood", "population", "density",  "median_income")
qos.depar <- merge(qos.depar, census, by = "neighborhood")


#### Interactive Scatter Plots
#metric.all <- read.csv("ScatterBubbleData/qos2014_department_hours.csv", header = T, stringsAsFactors = F)
#colnames(metric.all) <- c("X", "Neighborhood", "Duration", "Efficiency", "Ontime_Rate", "Request_Volume", "Department")
#metric.all <- subset(metric.all, is.na(metric.all$Neighborhood) == F)

metric.all <- read.csv("ScatterData/qos2014_department_hours.csv", header = T, stringsAsFactors = F)



