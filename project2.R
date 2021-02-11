storm <- read.csv(bzfile("repdata_data_StormData.csv.bz2"))
library(dplyr)
library(tidyr)
library(ggplot2)


unique(storm)
names(storm)
storm2 <- select(storm, c(EVTYPE, FATALITIES, INJURIES, PROPDMG, CROPDMG, PROPDMGEXP, CROPDMGEXP))
names(storm2)
str(storm2)
unique(storm2)
unique(storm2$PROPDMGEXP)
unique(storm2$CROPDMGEXP)
storm2$PROPDMGEXP <- toupper(storm2$PROPDMGEXP)
storm2$PROPDMGEXP[storm2$PROPDMGEXP %in% c("", "+", "-", "?")] <- "0"
storm2$PROPDMGEXP[storm2$PROPDMGEXP %in% c("B")] <- "9"
storm2$PROPDMGEXP[storm2$PROPDMGEXP %in% c("M")] <- "6"
storm2$PROPDMGEXP[storm2$PROPDMGEXP %in% c("K")] <- "3"
storm2$PROPDMGEXP[storm2$PROPDMGEXP %in% c("H")] <- "2"
storm2$PROPDMGEXP <- as.numeric(storm2$PROPDMGEXP)

storm2$CROPDMGEXP <- toupper(storm2$CROPDMGEXP)
storm2$CROPDMGEXP[storm2$CROPDMGEXP %in% c("", "+", "-", "?")] <- "0"
storm2$CROPDMGEXP[storm2$CROPDMGEXP %in% c("B")] <- "9"
storm2$CROPDMGEXP[storm2$CROPDMGEXP %in% c("M")] <- "6"
storm2$CROPDMGEXP[storm2$CROPDMGEXP %in% c("K")] <- "3"
storm2$CROPDMGEXP[storm2$CROPDMGEXP %in% c("H")] <- "2"
storm2$CROPDMGEXP <- as.numeric(storm2$CROPDMGEXP)

storm2$PROPDMGTOTAL <- storm2$PROPDMG * (10 ** storm2$PROPDMGEXP)
storm2$CROPDMGTOTAL <- storm2$CROPDMG * (10 ** storm2$CROPDMGEXP)
storm2$DMGTOTAL <- storm2$PROPDMGTOTAL + storm2$CROPDMGTOTAL
str(storm2)
SumData <- storm2 %>%
    group_by(EVTYPE) %>%
    summarize(SUMFATALITIES = sum(FATALITIES),
              SUMINJURIES = sum(INJURIES),
              SUMPROPDMG = sum(PROPDMGTOTAL),
              SUMCROPDMG = sum(CROPDMGTOTAL),
              TOTALDMG = sum(DMGTOTAL))

head(SumData)

Fatality <- arrange(SumData, desc(SUMFATALITIES))
FatalityData <- Fatality[1:10,]
Injury <- SumData[order(-SumData$SUMINJURIES), ]
InjuryData <- Injury[1:10,]
Damage <- SumData[order(-SumData$TOTALDMG), ]
DamageData <- Damage[1:10,]

FatalityData$EVTYPE <- with(FatalityData, reorder(EVTYPE, -SUMFATALITIES))
ggplot(FatalityData, aes(EVTYPE, SUMFATALITIES, label = SUMFATALITIES)) +
    geom_bar(stat = "identity", aes(color = EVTYPE)) +
    geom_text(nudge_y = 200) +
    xlab("Event Type") +
    ylab("Total Fatalities") +
    ggtitle("Most Fatal Events") +
    theme(plot.title = element_text(hjust = 0.5))

InjuryData$EVTYPE <- with(InjuryData, reorder(EVTYPE, -SUMINJURIES))
ggplot(InjuryData, aes(EVTYPE, SUMINJURIES, label = SUMINJURIES)) +
    geom_bar(stat = "identity", aes(color = EVTYPE)) +
    geom_text(nudge_y = 3000) +
    xlab("Event Type") +
    ylab("Total Injuries") +
    ggtitle("Most Injury Events") +
    theme(plot.title = element_text(hjust = 0.5))

DamageData$EVTYPE <- with(DamageData, reorder(EVTYPE, -TOTALDMG))
DamageDataLong <- DamageData %>%
    gather(key = "Type", value = "TOTALDAMAGE", c("SUMPROPDMG", "SUMCROPDMG")) %>%
    select(EVTYPE, Type, TOTALDAMAGE)
DamageDataLong$Type[DamageDataLong$Type %in% c("SUMPROPDMG")] <- "Property damage"
DamageDataLong$Type[DamageDataLong$Type %in% c("SUMCROPDMG")] <- "Crop damage"

# Plot
ggplot(DamageDataLong, aes(x = EVTYPE, y = TOTALDAMAGE, fill = Type)) +
    geom_bar(stat = "identity", position = "stack") +
    xlab("Event Type") +
    ylab("Total Damage") +
    ggtitle("Events with Most Damage") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
