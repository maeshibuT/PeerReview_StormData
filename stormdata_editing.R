#Download file
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
destfile<-"C:/Users/maesh/Desktop/r for study/PeerReview_StormData"
download.file(url,destfile)

#read.csv.bz2 file
SData<-read.csv("StormData.csv.bz2")

#selet required column for analysis
#EVTYPE,FATALITIES,INJURIES,PROPDMG,CROPDMG,REFNUM

library(dplyr)
data<-SData%>%select(EVTYPE,FATALITIES,INJURIES,PROPDMG,CROPDMG,REFNUM)

#check NA in each column
pNA<-length(data$PROPDMG[is.na(data$PROPDMG)])
cNA<-length(data$CROPDMG[is.na(data$CROPDMG)])
iNA<-length(data$INJURIES[is.na(data$INJURIES)])
fNA<-length(data$FATALITIES[is.na(data$FATALITIES)])

paste(pNA,cNA,iNA,fNA)

for (i in 1:length(data$EVTYPE)){
    if (data$EVTYPE[i]=="ASTRONOMICAL LOW TIDE"|data$EVTYPE[i]=="AVALANCHE"|
        data$EVTYPE[i]=="BLIZZARD"|data$EVTYPE[i]=="COASTAL FLOOD"|
        data$EVTYPE[i]=="COLD/WIND CHILL"|data$EVTYPE[i]=="DEBRIS FLOW"|
        data$EVTYPE[i]=="DENSE FOG"|data$EVTYPE[i]=="DENSE SMOKE"|
        data$EVTYPE[i]=="DROUGHT"|data$EVTYPE[i]=="DUST DEVIL"|
        data$EVTYPE[i]=="DUST STORM"|data$EVTYPE[i]=="EXCESSIVE HEAT"|
        data$EVTYPE[i]=="EXTREME COLD/WIND CHILL"|data$EVTYPE[i]=="FLASH FLOOD"|
        data$EVTYPE[i]=="FLOOD"|data$EVTYPE[i]=="FREEZING FOG"|
        data$EVTYPE[i]=="FROST/FREEZE"|data$EVTYPE[i]=="FUNNEL CLOUD"|
        data$EVTYPE[i]=="HAIL"|data$EVTYPE[i]=="HEAT"|data$EVTYPE[i]=="HEAVY RAIN"|
        data$EVTYPE[i]=="HEAVY SNOW"|data$EVTYPE[i]=="HIGH SURF"|
        data$EVTYPE[i]=="HIGH WIND"|data$EVTYPE[i]=="HURRICANE/TYPHOON"|
        data$EVTYPE[i]=="ICE STORM"|data$EVTYPE[i]=="LAKESHORE FLOOD"|
        data$EVTYPE[i]=="LAKE-EFFECT SNOW"|data$EVTYPE[i]=="LIGHTNING"|
        data$EVTYPE[i]=="MARINE HAIL"|data$EVTYPE[i]=="MARINE HIGH WIND"|
        data$EVTYPE[i]=="MARINE THUNDERSTORM WIND"|data$EVTYPE[i]=="RIP CURRENT"|
        data$EVTYPE[i]=="SEICHE"|data$EVTYPE[i]=="SLEET"|data$EVTYPE[i]=="STORM TIDE"|
        data$EVTYPE[i]=="STRONG WIND"|data$EVTYPE[i]=="THUNDERSTORM WIND"|
        data$EVTYPE[i]=="TORNADO"|data$EVTYPE[i]=="TROPICAL DEPRESSION"|data$EVTYPE[i]=="TROPICAL STORM"|data$EVTYPE[i]=="TSUNAMI"|data$EVTYPE[i]=="VOLCANIC ASH"|data$EVTYPE[i]=="WATERSPOUT"|data$EVTYPE[i]=="WILDFIRE"|data$EVTYPE[i]=="WINTER STORM"|data$EVTYPE[i]=="WINTER WEATHER"){data$EVTYPE2[i]<-data$EVTYPE[i]
    } else {data$EVTYPE2[i]<-"OTHERS"
    }
}

data2<-transform(data,EVTYPE2=factor(EVTYPE2,levels=c("ASTRONOMICAL LOW TIDE","AVALANCHE","BLIZZARD","COASTAL FLOOD","COLD/WIND CHILL","DEBRIS FLOW",
                                                      "DENSE FOG","DENSE SMOKE","DROUGHT","DUST DEVIL","DUST STORM",
                                                      "EXCESSIVE HEAT","EXTREME COLD/WIND CHILL","FLASH FLOOD","FLOOD",
                                                      "FREEZING FOG","FROST/FREEZE","FUNNEL CLOUD","HAIL","HEAT",
                                                      "HEAVY RAIN","HEAVY SNOW","HIGH SURF","HIGH WIND","HURRICANE/TYPHOON",
                                                      "ICE STORM","LAKESHORE FLOOD","LAKE-EFFECT SNOW","LIGHTNING",
                                                      "MARINE HAIL","MARINE HIGH WIND","MARINE STRONG WIND","MARINE THUNDERSTORM WIND",
                                                      "RIP CURRENT","SEICHE","SLEET","STORM TIDE","STRONG WIND",
                                                      "THUNDERSTORM WIND","TORNADO","TROPICAL DEPRESSION","TROPICAL STORM","TSUNAMI","VOLCANIC ASH","WATERSPOUT","WILDFIRE","WINTER STORM","WINTER WEATHER","OTHERS")))


library(ggplot2)
library(gridExtra)

#plot the relationships between event types and FATALITIES
fgraph<-ggplot(data=data2,aes(EVTYPE2,FATALITIES))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(title="Fatalities Caused by Each Event Type", x="Event Types",y="FATALITIES")

#plot the relationships between event types and INJURIES
igraph<-ggplot(data=data2,aes(EVTYPE2,INJURIES))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(title="Injuries Caused by Each Event Type", x="Event Types",y="INJURIES")

#plot the relationships between event types and property damage
pgraph<-ggplot(data=data2,aes(EVTYPE2,PROPDMG))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(title="Property Damage Caused by Each Event Type", x="Event Types",y="Property Damage")

#plot the relationships between event types and crop damage
cgraph<-ggplot(data=data2,aes(EVTYPE2,CROPDMG))+geom_boxplot()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(title="Crop Damage Caused by Each Event Type", x="Event Types",y="Crop Damage")

grid.arrange(fgraph,igraph,pgraph,cgraph)

