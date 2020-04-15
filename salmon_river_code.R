# FIRST STEP: import 'salmon_metadata' data set, sheet "MERGER", and 
# name it 'salmon_data_1'
# KEY tell the program to understand 'year' as a factor, but both 
# 'snow'&'flow' MUST BE NUMERIC!

#____________REMOVE ABSENT DATA__________
# Here I subset out all of the cases where either 'snow' or 'flow' are NA,
# as those points dont lend themselfes to a regression analysis
subflow<-subset(salmon_data_1, subset=flow>1)
river<-subset(subflow, subset=snow>1)
#"river"  now contains all 49 cases with data for both flow & snow.

# Assigning independent and response variables. Snowfall, in theory, 
# determines the amount of size of the source which will become flow the 
# following summer, and thus is assigned to x while flow is assigned to y.
y <- river$flow
x <- river$snow 

#____________VISUALIZING DATA__________________
#### WINTER TOTAL VS PEAK, WITH YEAR LABELS. FINE ASS GRAPH RIGHT HERE
par(mfcol=c(1,1))
plot(y~x, data=river, col="blue",labels=row.names(year[1:49]), main="Winter Cumulative Snowfall vs Summer Peak Flow",
            xlab="Winter Cumulative Snowfall (inches)", ylab="Summer Peak Discharge (cfs)", xlim=c(0, 190), ylim=c(2400, 8700))
lines(x, fitted(regmodel), col="red")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "solid")
text(x,y, labels=river$year, cex=.7, pos=4)


#____________RUNNING THE MODEL__________________
regmodel=lm(y~x)
summary(regmodel)
#__________________DIAGNOSTIC PLOTS____________ 
#basic diagnostic plots
#made with code copied over from comandr
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(regmodel, main = "Winter Cumulative Snowfall vs Summer Peak Flow")
par(oldpar)

#_______________NOTES__________________
#at least say your going to do a power test as a way to 
#demonstrate conceptual learning, even if you dont do it, 
#also just say if it is not a powerful enough test, this is the 
#most common response






#################______________________________________________________
#################______________________________________________________
#______________________PROJECT 2!!!!___________________________________

# FIRST STEP: import 'salmon_metadata' data set, sheet "MEREGE_2", 
# and name it 'salmon_2'
# KEY tell the program to understand 'year' as a date, but everything else
#MUST BE NUMERIC!

#____________REMOVE ABSENT DATA__________
# Here I subset out all of the cases where any variable is NA
# as those points dont lend themselfes to a regression analysis

SUB1<-subset(salmon_2, subset=salmon_2$`Flow Change 0-24 Hours`>-100)
data2<-subset(SUB1, subset=SUB1$`Temp Change 48-72 Hours`>-100)

#"data2"  now contains all cases with data for 48-72 hour temp change, 
# and 0-24 hour flow change, which is what we will regress first.

# Assigning independent and response variables. temp change, in theory, temperature (x)
# determines the amount of melting snow the snow and effects discharge rates (y).

# 'y2a' is going to be for the discharge change in 24 hours,
y2a <- data2$`Flow Change 0-24 Hours`

##### PROBLEM< CHECK LOGIC ORDER
##x2b <- data2$`Temp Change 24-48 Hours`
x2a <- data2$`Temp Change 0-24 Hours`
x2b <- data2$`Temp Change 24-48 Hours`
x2c <- data2$`Temp Change 48-72 Hours`
x2d <- data2$`Temp Change 72-96 Hours`

#____________VISUALIZING DATA ROUND 1__________________
#Grouping Command Δ
par(mfrow=c(2,2))

regmodelt21p999=lm(y2a~x2a)
summary(regmodelt21p1)
plot(y2a~x2a, data=river, col="blue", main="(1A) Mean Temperature Δ vs Mean Flow Δ",
     xlab="Mean Temperature Δ Yesterday->Today (°f)", ylab="Mean Flow Δ Yesterday->Today (cfs)", xlim=c(-20, 20), ylim=c(-100, 250))
lines(x2a, fitted(regmodelt21p1), col="red")


regmodelt21p2=lm(y2a~x2b)
summary(regmodelt21p2)
plot(y2a~x2b, data=river, col="blue", main="(1B) Mean Temperature Δ vs Mean Flow Δ ",
            xlab="Mean Temperature Δ  2->1 Days Prior (°f)", ylab="Mean Flow Δ Yesterday->Today (cfs)", xlim=c(-20, 20), ylim=c(-100, 250))
lines(x2b, fitted(regmodelt21p2), col="red")

regmodelt21p3=lm(y2a~x2c)
summary(regmodelt21p3)
plot(y2a~x2c, data=river, col="blue", main="(1C) Mean Temperature Δ vs Mean Flow Δ ",
            xlab="Mean Temperature Δ 3->2 Days Prior (°f)", ylab="Mean Flow Δ Yesterday->Today (cfs)", xlim=c(-20, 20), ylim=c(-100, 250))
lines(x2c, fitted(regmodelt21p3), col="red")

regmodelt21p4=lm(y2a~x2d)
summary(regmodelt21p4)
plot(y2a~x2d, data=river, col="blue", main="(1D) Mean Temperature Δ vs Mean Flow Δ ",
     xlab="Mean Temperature Δ 4->3 Days Prior (°f)", ylab="Mean Flow Δ Yesterday->Today (cfs)", xlim=c(-20, 20), ylim=c(-100, 250))
lines(x2d, fitted(regmodelt21p4), col="red")




##########____________ ROUND 2 VISUALIZATIONS______________
# PLOTTING THE FLOW, AGAINST THE PAST FEW DAYS ABSOLUTE TEMPERATURE
y3a <- data2$`Mean flow`
x3a <- data2$`Mean temperature`
x3b <- data2$`Temp 1 day ago`
x3c <- data2$`Temp 2 days ago`
x3d <- data2$`Temp 3 days ago`
x3e <- data2$`Temp 4 days ago`

par(mfcrow=c(2,2))

regmodelt23p1=lm(y3a~x3a)
summary(regmodelt23p1)
plot(y3a~x3a, data=river, col="blue", main="(2A) Mean Temperature vs Mean Flow Δ",
     xlab="Mean Temperature Today (°f)", ylab="Mean Flow Today (cfs)", xlim=c(3, 48), ylim=c(70, 930))
lines(x3a, fitted(regmodelt22p1), col="red")

regmodelt23p2=lm(y3a~x3b)
summary(regmodelt23p2)
plot(y3a~x3b, data=river, col="blue", main="(2B) Mean Temperature vs Mean Flow",
     xlab="Mean Temperature Yesterday (°f)", ylab="Mean Flow Today (cfs)", xlim=c(3, 48), ylim=c(70, 930))
lines(x3b, fitted(regmodelt22p2), col="red")

regmodelt23p3=lm(y3a~x3c)
summary(regmodelt23p3)
plot(y3a~x3c, data=river, col="blue", main="(2C) Mean Temperature vs Mean Flow",
            xlab="Mean Temperature 2 Days Prior (°f)", ylab="Mean Flow Today (cfs)", xlim=c(3, 48), ylim=c(70, 930))
lines(x3c, fitted(regmodelt22p3), col="red")

regmodelt23p4=lm(y3a~x3d)
summary(regmodelt23p4)
plot(y3a~x3d, data=river, col="blue", main="(2D) Mean Temperature vs Mean Flow",
            xlab="Mean Temperature 2 Days Prior (°f)", ylab="Mean Flow Today (cfs)", xlim=c(3, 48), ylim=c(70, 930))
lines(x3d, fitted(regmodelt22p4), col="red")




##########____________ ROUND 3 VISUALIZATIONS______________
# PLOTTING THE FLOW CHANGE, AGAINST THE PAST FEW DAYS ABSOLUTE TEMPERATURE

## nah fuck it dont do this one.


par(mfcol=c(2,2))

regmodelt23p1=lm(y2a~x3a)
plot(y2a~x3a, data=river, col="blue", main="Mean Temperature vs Mean Flow CHANGE",
     xlab="Mean Temperature Today (°f)", ylab="Mean Flow Today (cfs)", xlim=c(3, 48), ylim=c(-100, 250))
lines(x3a, fitted(regmodelt23p1), col="red")

regmodelt23p2=lm(y2a~x3b)
plot(y2a~x3b, data=river, col="blue", main="Mean Temperature vs Mean Flow",
     xlab="Mean Temperature Yesterday (°f)", ylab="Mean Flow Today (cfs)", xlim=c(3, 48), ylim=c(-100, 250))
lines(x3b, fitted(regmodelt23p2), col="red")

regmodelt23p3=lm(y2a~x3c)
plot(y2a~x3c, data=river, col="blue", main="Mean Temperature vs Mean Flow",
     xlab="Mean Temperature Yesterday (°f)", ylab="Mean Flow Today (cfs)", xlim=c(3, 48), ylim=c(-100, 250))
lines(x3c, fitted(regmodelt23p3), col="red")

regmodelt23p4=lm(y2a~x3d)
plot(y2a~x3d, data=river, col="blue", main="Mean Temperature vs Mean Flow",
     xlab="Mean Temperature 2 Days Prior (°f)", ylab="Mean Flow Today (cfs)", xlim=c(3, 48), ylim=c(-100, 250))
lines(x3d, fitted(regmodelt23p4), col="red")


