---
title: "NOAA STORM DATABASE: EFFECTS ON POPULATION HEALTH AND THE ECONOMY"
author: Rhyz C. Gomez
output: html_document
keep_md: true
---

## SYNOPSIS
AT MOST 10 SENTENCES


## DATA PROCESSING

The data that is used in this project was downloaded from the [course web site][1]. There is some documentation of the database available at National Weather Service [Website][2].  

The Storm Data is taken from the National Oceanic and Atmospheric Administration (NOAA) official publication. It documents the occurrence of weather phenomena that have intensive effect on human lives and the economy.  The data contains information from year 1950 to November 2011. 

### Reading the Data

The data is unzipped to the user's working directory and the `dplyr` and `qqplot2` packages are used to transform and plot the data.


```r
stormdata <- read.csv("repdata-data-StormData.csv")
str(stormdata)
```

The `str` function is used to further understand the data. Further, the `table` function is utilized multiple times to see the EVTYPE columns and to organize the event types into their proper categories.

### Cleaning the Data

The columns needed for this analysis are the EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG and CROPDMGEXP. However, before subsetting the dataset there was a need to clean the data.  


```r
library(dplyr)
library(ggplot2)
```

Notice that the date column is in factor type and the state column in numeric type. Hence, transform columns first in the right format for better analysis. The event time could represent a single time or it could represent the beginning and ending times of an event. Again, the EVTYPE column needs to be processed and cleaned. Since the code for cleaning the data is long, the complete code is placed at the end of this document (Appendix A).     


```r
storm <- readRDS("stormdata.csv")
stormdata <- storm
```


when transforming the data, the event type under excessive wind, excessive snow, excessively dry and excessive were put under the same category EXCESSIVE CONDITION. The event type under extreme wet, extreme wind, and extreme heat were put under EXTREME CONDITION. The event type under early freeze, early rain and early snow were now under EARLY CONDITION. Further, event types that fall in the same category are categorized into one. 

### Subsetting the Data (FATALITIES & INJURIES)

To analyze which types of event conditions are most harmful with respect to population health, we consider the FATALITIES and INJURIES column. Further a new column (FATALITIES_INJURIES) is created to give the total number of individuals that suffered and died in the said event. Then, we find the total number of fatalities/injuries per event type and consider only the events with more than 1,000 casualties/injuries.



```r
FATALINJURY <- subset(stormdata,FATALITIES_INJURIES!=0)
FATALINJURY <- FATALINJURY %>% select(EVTYPE,FATALITIES,INJURIES,FATALITIES_INJURIES)
FATALINJURY<- FATALINJURY %>% group_by(EVTYPE)%>%summarise(Total_HealthRisk=sum(FATALITIES_INJURIES))
FATALINJURY <- subset(FATALINJURY,Total_HealthRisk>1000)
FATALINJURY$EVTYPE <- as.factor(FATALINJURY$EVTYPE)
```



### Subsetting the Data (Property and Crop Damages) 

On the other hand, we consider the PROPDMG, PROPDMGEXP, CROPDMG and CROPDMGEXP columns to analyze which event types have the greatest economic consequences. These columns are considered because property damages and crop damages impact the economic state of an individual and a state.


```r
PROPDAMAGE <- subset(stormdata,PROPDMG!=0|CROPDMG!=0)
```

The code for cleaning and processing PROPDMG and CROPDMG is found at Appendix B. The new columns (PROPDMG and CROPDMG) were multiplied by their exponential components (PROPDMGEXP and CROPDMGEXP).


```r
PROPDAMAGE[PROPDAMAGE$PROPDMGEXP=="K","PROPDMG"]<-PROPDAMAGE[PROPDAMAGE$PROPDMGEXP=="K","PROPDMG"]*1000 
PROPDAMAGE[PROPDAMAGE$PROPDMGEXP=="m"|PROPDAMAGE$PROPDMGEXP=="M","PROPDMG"] <-PROPDAMAGE[PROPDAMAGE$PROPDMGEXP=="m"|PROPDAMAGE$PROPDMGEXP=="M","PROPDMG"]*1000000 
PROPDAMAGE[PROPDAMAGE$PROPDMGEXP=="h"|PROPDAMAGE$PROPDMGEXP=="H"|PROPDAMAGE$PROPDMGEXP==2,"PROPDMG"] <-PROPDAMAGE[PROPDAMAGE$PROPDMGEXP=="h"|PROPDAMAGE$PROPDMGEXP=="H"|PROPDAMAGE$PROPDMGEXP==2,"PROPDMG"]*100
PROPDAMAGE[PROPDAMAGE$PROPDMGEXP==3,"PROPDMG"] <-PROPDAMAGE[PROPDAMAGE$PROPDMGEXP==3,"PROPDMG"]*1000
PROPDAMAGE[PROPDAMAGE$PROPDMGEXP==4,"PROPDMG"] <-PROPDAMAGE[PROPDAMAGE$PROPDMGEXP==4,"PROPDMG"]*10000 
PROPDAMAGE[PROPDAMAGE$PROPDMGEXP==5,"PROPDMG"] <-PROPDAMAGE[PROPDAMAGE$PROPDMGEXP==5,"PROPDMG"]*100000 
PROPDAMAGE[PROPDAMAGE$PROPDMGEXP==6,"PROPDMG"] <-PROPDAMAGE[PROPDAMAGE$PROPDMGEXP==6,"PROPDMG"]*1000000 
PROPDAMAGE[PROPDAMAGE$PROPDMGEXP==7,"PROPDMG"] <-PROPDAMAGE[PROPDAMAGE$PROPDMGEXP==7,"PROPDMG"]*10000000 
PROPDAMAGE[PROPDAMAGE$PROPDMGEXP=="B","PROPDMG"] <-PROPDAMAGE[PROPDAMAGE$PROPDMGEXP=="B","PROPDMG"]*1000000000
PROPDAMAGE[PROPDAMAGE$CROPDMGEXP=="B","CROPDMG"] <-PROPDAMAGE[PROPDAMAGE$CROPDMGEXP=="B","CROPDMG"]*1000000000 
PROPDAMAGE[PROPDAMAGE$CROPDMGEXP=="K"|PROPDAMAGE$CROPDMGEXP=="k","CROPDMG"] <-PROPDAMAGE[PROPDAMAGE$CROPDMGEXP=="K"|PROPDAMAGE$CROPDMGEXP=="k","CROPDMG"]*1000 
PROPDAMAGE[PROPDAMAGE$CROPDMGEXP=="M"|PROPDAMAGE$CROPDMGEXP=="m","CROPDMG"] <-PROPDAMAGE[PROPDAMAGE$CROPDMGEXP=="M"|PROPDAMAGE$CROPDMGEXP=="m","CROPDMG"]*1000000 
PROPDAMAGE$TOTALDMG <- PROPDAMAGE$PROPDMG + PROPDAMAGE$CROPDMG
PROPDAMAGE <- PROPDAMAGE %>% group_by(EVTYPE)%>%summarise(TOTALDMG=sum(TOTALDMG))
PROPDAMAGE$EVTYPE <- as.factor(PROPDAMAGE$EVTYPE)
PROPDAMAGE2 <- PROPDAMAGE
```

Since we only need to consider event types with greatest economic consequences, we subset the data and consider the event type with high damage cost.


```r
PROPDAMAGE2 <- subset(PROPDAMAGE2,TOTALDMG>6000000000)
```


## RESULTS

### TOTAL FATALITIES/INJURIES PER EVENT TYPE ACROSS THE US


```r
EVENT <- FATALINJURY$EVTYPE
qplot(factor(EVTYPE),Total_HealthRisk,data=FATALINJURY,fill=EVENT,main="TOTAL CASUALTIES AND INJURIES ACROSS THE UNITED STATES PER EVENT TYPE",xlab="EVENT TYPE",ylab="TOTAL CASUALTIES/INJURIES",geom="bar",stat="identity")+geom_text(data=FATALINJURY,aes(x=EVTYPE,y=Total_HealthRisk+1000,label = Total_HealthRisk),cex=3,col="red")+theme_bw()
```

![plot of chunk plot](figure/plot-1.png) 


### TOTAL DAMAGES PER EVENT TYPE ACROSS THE US

```r
qplot(factor(EVTYPE),TOTALDMG/1000000,data=PROPDAMAGE2,fill=PROPDAMAGE2$EVTYPE,main="TOTAL DAMAGES OF EVENTS ACROSS THE UNITED STATES IN YEAR 1950 to 2011",xlab="EVENT TYPE",ylab="TOTAL DAMAGES IN MILLIONS OF DOLLARS",geom="bar",stat="identity")+theme_classic()
```

![plot of chunk plot2](figure/plot2-1.png) 







[1]: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2 "course web site"
[2]:  http://www.nws.noaa.gov/directives/ "Website"





