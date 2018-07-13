##########################################
# Data Preparation for Injury Prediction
# Author: Mladen Jovanovic, Belgrade, Serbia
# Date: 09.07.2018
##########################################

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(TTR)
library(zoo)

#######################
# FUNCTIONS
######################

# Acture to Chronic Workload Ratio
ACWR <- function(acute, chronic) {
    acwr <- acute / chronic
    acwr <- ifelse(chronic == 0, 0, acwr) 
}

# Modify the injury tag based on InjuryLead paramet (global environment)
fixInjury <- function(data) {
    # Convert date to number
    data$dateIndex <- data$Date
    
    injuryList <- dplyr::select(data, dateIndex, Player.Name, Injury) %>%
        filter(Injury == "Injured")
    
    for( i in injuryList$dateIndex) {
        data$Injury <- ifelse(((i - data$dateIndex) < (injuryLead + 1)) &
                                  ((i - data$dateIndex) >= 0) | (data$Injury == "Injured"),
                              "Injured",
                              "Non.Injured")
    }
    return(dplyr::select(data, -dateIndex))    
}


#####################
# MAIN CODE
#####################

# Load Data
monitoringData <- read.csv("Monitoring Data with Injuries (raw).csv",
                           header = TRUE,
                           stringsAsFactors = FALSE)

# Create DF for the empty days
emptyDays <- expand.grid(Data.Split = "Train",
                         Season = 1,
                         Phase = "Pre Season",
                         Date = seq(-70, 0, 1),
                         Player.Name = c("Player 01", "Player 02", "Player 03"),
                         Injury.Location = "",
                         Injury.Type = "",
                         RPE.Load = 0,
                         Total.Distance = 0,
                         HS.Distance = 0,
                         stringsAsFactors = FALSE)

# Bind the two
monitoringData <- rbind(emptyDays, monitoringData)

# Sort
monitoringData <- monitoringData %>%
                  arrange(Player.Name, Date)

# Create a "long" format
monitoringDataLong <- melt(monitoringData, id.vars = 1:7)

# Create the exponential rolling averages and ACWR variables
monitoringDataLong <- monitoringDataLong %>%
    group_by(Player.Name, variable) %>%
    arrange(Date) %>%
    # Mean
    mutate(Day.Value = value,
           Acute = round(EMA(value, n = 1, ratio = 2/(7+1)), 2),
           Chronic = round(EMA(value, n = 1, ratio = 2/(42+1)), 2),
           # Create a lag to avoid overlap between Acute and Chronic
           Chronic = lag(Chronic, 7),
           
           # Add ACWR
           ACWR = round(ACWR(Acute, Chronic), 2),
           
           # Get the ACWR peak in the last rolling 7 days
           ACWRrollPeak = rollmax(ACWR, 7, fill = NA, align = "right"))

# Plot
# Acute and Chronic
gg <- ggplot(monitoringDataLong, aes(x = Date)) +
      theme_bw() +
      geom_bar(aes(y = value), stat = "identity", alpha = 0.3, color = "grey") +
      geom_line(aes(y = Chronic), color = "blue", alpha = 0.5) +
      geom_line(aes(y = Acute), color = "red", alpha = 0.5) +
      facet_grid(variable~Player.Name, scales = "free_y") +
      ylab("") + xlab("Day")
gg  

# ACWR
gg <- ggplot(monitoringDataLong, aes(x = Date)) +
    theme_bw() +
    geom_line(aes(y = ACWR), color = "grey", alpha = 0.8) +
    geom_line(aes(y = ACWRrollPeak), color = "red", alpha = 0.8) +
    facet_grid(variable~Player.Name, scales = "free_y") +
    ylab("") + xlab("Day")
gg 

# Remove first 15 days
#monitoringDataLong <- monitoringDataLong %>%
#                      filter(Date > 25)

# Convert to long again (to merge the new aggregates with the features name)
monitoringDataLong$value <- NULL
monitoringDataLong <- rename(monitoringDataLong, Metric = variable)
monitoringDataLong <- melt(monitoringDataLong, id.vars = 1:8)
monitoringDataLong$Metric <- paste(monitoringDataLong$Metric, monitoringDataLong$variable, sep = ".")
monitoringDataLong$variable <- NULL

# Create Lag variables
monitoringDataLong <- monitoringDataLong %>%
    group_by(Player.Name, Metric) %>%
    arrange(Date) %>%
    mutate(Lag.0 = lag(value, 0),
           Lag.07 = lag(value, 7),
           Lag.14 = lag(value, 14),
           Lag.21 = lag(value, 21)) %>%
    ungroup()

# Convert to long again
monitoringDataLong$value <- NULL
monitoringDataLong <- melt(monitoringDataLong, id.vars = 1:8)
monitoringDataLong$Metric <- paste(monitoringDataLong$Metric, monitoringDataLong$variable, sep = ".")
monitoringDataLong$variable <- NULL

# Finaly convert to wide format
monitoringData <- dcast(monitoringDataLong,
                    ... ~ Metric,
                    value.var = "value")

# Sort
monitoringData <- arrange(monitoringData, Player.Name, Date)

# Create monitoring feature for non-contact injuries ("Soft Tissue & Overuse")
monitoringData$Injury <- ifelse(monitoringData$Injury.Type == "Soft Tissue" | monitoringData$Injury.Type == "Overuse",
                            "Injured", "Non.Injured")

monitoringData$Injury01 <- monitoringData$Injury

# Modify the injury tag using injury lead
injuryLead <- 7
tmpData <- ddply(monitoringData, .variables = c("Player.Name"), .fun = fixInjury)
monitoringData$Injury07 <- tmpData$Injury

injuryLead <- 14
tmpData <- ddply(monitoringData, .variables = c("Player.Name"), .fun = fixInjury)
monitoringData$Injury14 <- tmpData$Injury

injuryLead <- 21
tmpData <- ddply(monitoringData, .variables = c("Player.Name"), .fun = fixInjury)
monitoringData$Injury21 <- tmpData$Injury

# Reorganize the columns 
monitoringData <- monitoringData[c(1:7, 69:72, 8:67)] 

#### Clear up the data 
# Clear up missing values
monitoringData <- na.omit(monitoringData)

# Plot 
plotDF <- melt(monitoringData, id.vars = 1:11)

# RPE.Load
gg <- ggplot(filter(plotDF, grepl("RPE", variable)),
             aes(y = value, x = Injury07, fill = Injury07)) +
      theme_bw() +
      geom_violin(alpha = 0.3) +
      facet_wrap(~variable, scales = "free_y") +
      xlab("") + ylab("")
gg

# Total Distance
gg <- ggplot(filter(plotDF, grepl("Total.Distance", variable)),
             aes(y = value, x = Injury07, fill = Injury07)) +
    theme_bw() +
    geom_violin(alpha = 0.3) +
    facet_wrap(~variable, scales = "free_y") +
    xlab("") + ylab("")
gg

# HS.Distance
gg <- ggplot(filter(plotDF, grepl("HS.Distance", variable)),
             aes(y = value, x = Injury07, fill = Injury07)) +
    theme_bw() +
    geom_violin(alpha = 0.3) +
    facet_wrap(~variable, scales = "free_y") +
    xlab("") + ylab("")
gg

# PCA
PC <- prcomp(monitoringData[-c(1:11)], center = TRUE, scale. = TRUE)

plotDF <- data.frame(Injury07 = monitoringData$Injury07,
                     predict(PC)[,c("PC1", "PC2")])
gg <- ggplot(plotDF,
             aes(y = PC1, x = PC2, color = Injury07)) +
    theme_bw() +
    geom_point(alpha = 0.5)
gg

# Save data
write.csv(monitoringData,
          file = "Monitoring Data with Injuries (processed).csv",
          row.names = FALSE)
