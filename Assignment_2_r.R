
####Loading needed packages####

library(knitr)
library(markdown)
library(rmarkdown)
library(plyr)
library(stats)

##Information about hardware and software used to derive theses results
sessionInfo()


####getting and cleaning data####

setwd("C:/Users/toddhim/Desktop/Coursera_Data_Science/5.Reproducible_Research/Assignment_2")


fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" 
download.file(fileUrl, destfile = "FStormData.csv.bz2") 
dateDownloaded <- date() #capturing download date
dateDownloaded



storm_data <- read.csv("FStormData.csv.bz2")




################################################################################################



Neededvariables <- c("BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
storm_data_2 <- storm_data[Neededvariables]
##dim(storm_data_2)
##head(storm_data_2)

##Convert the date into date class
storm_data_2$BGN_DATE<-as.Date(storm_data_2$BGN_DATE, "%m/%d/%Y")


unique(storm_data_2$PROPDMGEXP)
unique(storm_data_2$CROPDMGEXP)


##Page 12 of the instructions, the letter "K" stands for thousands, while "M" for millions and "B" for billions. 
##however, we find both upper and lower case from these letters. 
##The first thing we need to do is to transform the exponential terms back into actual values.

storm_data_2[storm_data_2$PROPDMGEXP == "K", ]$PROPDMG <- storm_data_2[storm_data_2$PROPDMGEXP == "K", ]$PROPDMG * 1000
storm_data_2[storm_data_2$PROPDMGEXP == "k", ]$PROPDMG <- storm_data_2[storm_data_2$PROPDMGEXP == "k", ]$PROPDMG * 1000
storm_data_2[storm_data_2$PROPDMGEXP == "M", ]$PROPDMG <- storm_data_2[storm_data_2$PROPDMGEXP == "M", ]$PROPDMG * 1000000
storm_data_2[storm_data_2$PROPDMGEXP == "m", ]$PROPDMG <- storm_data_2[storm_data_2$PROPDMGEXP == "m", ]$PROPDMG * 1000000
storm_data_2[storm_data_2$PROPDMGEXP == "B", ]$PROPDMG <- storm_data_2[storm_data_2$PROPDMGEXP == "B", ]$PROPDMG * 1000000000
storm_data_2[storm_data_2$PROPDMGEXP == "b", ]$PROPDMG <- storm_data_2[storm_data_2$PROPDMGEXP == "b", ]$PROPDMG * 1000000000

storm_data_2[storm_data_2$CROPDMGEXP == "K", ]$CROPDMG <- storm_data_2[storm_data_2$CROPDMGEXP == "K", ]$CROPDMG * 1000
storm_data_2[storm_data_2$CROPDMGEXP == "k", ]$CROPDMG <- storm_data_2[storm_data_2$CROPDMGEXP == "k", ]$CROPDMG * 1000
storm_data_2[storm_data_2$CROPDMGEXP == "M", ]$CROPDMG <- storm_data_2[storm_data_2$CROPDMGEXP == "M", ]$CROPDMG * 1000000
storm_data_2[storm_data_2$CROPDMGEXP == "m", ]$CROPDMG <- storm_data_2[storm_data_2$CROPDMGEXP == "m", ]$CROPDMG * 1000000
storm_data_2[storm_data_2$CROPDMGEXP == "B", ]$CROPDMG <- storm_data_2[storm_data_2$CROPDMGEXP == "B", ]$CROPDMG * 1000000000
storm_data_2[storm_data_2$CROPDMGEXP == "b", ]$CROPDMG <- storm_data_2[storm_data_2$CROPDMGEXP == "b", ]$CROPDMG * 1000000000






sumFatalities <- aggregate(FATALITIES ~ EVTYPE, data = storm_data_2,  FUN="sum")
dim(sumFatalities)  ## 985 observations

top5fatalities <- sumFatalities[order(-sumFatalities$FATALITIES), ][1:5, ]
dim(top5fatalities)


sumInjuries <- aggregate(INJURIES ~ EVTYPE, data = storm_data_2,  FUN="sum")
dim(sumInjuries)  ## 985 observations

top5injuries <- sumInjuries[order(-sumInjuries$INJURIES), ][1:5, ]
dim(top5injuries)






##graph results - both fatalities and injuries

par(mfrow = c(1,2), mar = c(8,5,5,1))
    
barplot(top5fatalities$FATALITIES, names.arg = top5fatalities$EVTYPE, las = 3, 
        main = "Top 5 Fatalities", ylab = "Number of Fatalities", col="blue")


barplot(top5injuries$INJURIES, names.arg = top5injuries$EVTYPE, las = 3, 
        main = "Top 5 Injuries", ylab = "Number of Injuries", col="red")


################################

PropertyDamage <- aggregate(PROPDMG ~ EVTYPE, data = storm_data_2,  FUN="sum")
top5propdamage <- PropertyDamage[order(-PropertyDamage$PROPDMG), ][1:5, ]


CropDamage <- aggregate(CROPDMG ~ EVTYPE, data = storm_data_2,  FUN="sum")
top5cropdamage <- CropDamage[order(-CropDamage$CROPDMG), ][1:5, ]


par(mfrow = c(1,2), mar = c(8,5,5,1))

barplot(top5propdamage$PROPDMG, names.arg = top5propdamage$EVTYPE, las = 3,
        main = "Top 5 Property Damage", ylab = "Cost of damage", col="blue")

barplot(top5cropdamage$CROPDMG, names.arg = top5cropdamage$EVTYPE, las = 3,
        main = "Top 5 Crop Damage", ylab = "Cost of damage", col="red")





##########################
###Past 20 years##


##subsetting data by date
storm_data_20years <- storm_data_2[storm_data_2$BGN_DATE > '1996-10-1' ,]


sumFatalities_20 <- aggregate(FATALITIES ~ EVTYPE, data = storm_data_20years,  FUN="sum")
top5fatalities_20 <- sumFatalities_20[order(-sumFatalities_20$FATALITIES), ][1:5, ]


sumInjuries_20 <- aggregate(INJURIES ~ EVTYPE, data = storm_data_20years,  FUN="sum")
top5injuries_20 <- sumInjuries_20[order(-sumInjuries_20$INJURIES), ][1:5, ]


##graph results - both fatalities and injuries

par(mfrow = c(1,2), mar = c(8,5,5,1))

barplot(top5fatalities_20$FATALITIES, names.arg = top5fatalities_20$EVTYPE, las = 3, 
        main = "Top 5 Fatalities", ylab = "Number of Fatalities", col="blue")


barplot(top5injuries_20$INJURIES, names.arg = top5injuries_20$EVTYPE, las = 3, 
        main = "Top 5 Injuries", ylab = "Number of Injuries", col="red")



PropertyDamage_20 <- aggregate(PROPDMG ~ EVTYPE, data = storm_data_20years,  FUN="sum")
top5propdamage_20 <- PropertyDamage_20[order(-PropertyDamage_20$PROPDMG), ][1:5, ]


CropDamage_20 <- aggregate(CROPDMG ~ EVTYPE, data = storm_data_20years,  FUN="sum")
top5cropdamage_20 <- CropDamage_20[order(-CropDamage_20$CROPDMG), ][1:5, ]


par(mfrow = c(1,2), mar = c(8,5,5,1))

barplot(top5propdamage_20$PROPDMG, names.arg = top5propdamage_20$EVTYPE, las = 3,
        main = "Top 5 Property Damage", ylab = "Cost of damage", col="blue")

barplot(top5cropdamage_20$CROPDMG, names.arg = top5cropdamage_20$EVTYPE, las = 3,
        main = "Top 5 Crop Damage", ylab = "Cost of damage", col="red")






#####################################


