summary(who)
mean(who$Over60)
who[,]
?which(min(who$Over60))
who[which.min(who$Over60), ]
who[which.max(who$LiteracyRate),]
min(tapply(who$ChildMortality, who$Region, mean))
plot(usda$Protein, usda$TotalFat)
boxplot(who$LifeExpectancy ~ who$Region)
rm(mvtWeek1)
max(mvt$ID)
min(mvt$Beat)
table(mvt$Arrest)
table(mvt$LocationDescription)
match('ALLEY', mvt$LocationDescription)
nrow(subset(mvt, mvt$LocationDescription=='ALLEY'))
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
mvt$Month = months(DateConvert)

mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
min(table(mvt$Month))
max(table(mvt$Weekday))

table(mvt$Month, mvt$Arrest)

arr <- subset(mvt, mvt$Arrest == TRUE)
max(table(arr$Month))

summary(arr)


hist(mvt$Date, breaks=100)
boxplot(Date~Arrest, data=mvt)
boxplot(mvt$Date ~ mvt$Arrest)
mvt2001 <- subset(mvt, mvt$Year == 2001)
table(mvt2001$Arrest)
prop.table(mvt2001$Arrest)


table(cps$Race, cps$Hispanic)
is.na(cps)
sapply(cps, function(x)  length(which(is.na(x))))
sapply(cps, function(x)  is.na(x))


length(which(is.na(cps)))
table(cps$Region, is.na(cps$Married))
table(cps$Sex, is.na(cps$Married))
table(cps$Age, is.na(cps$Married))
table(cps$Citizenship, is.na(cps$Married))

table(cps$State, is.na(cps$MetroAreaCode))
alaska <- subset(cps, cps$State == 'Alaska')
table(cps$Region, mean(is.na(cps$MetroAreaCode)))
cps$Metro <- is.na(cps$MetroAreaCode)

metroMean <- data.frame(tapply(cps$Metro, cps$State, mean))
metroMean30 <- subset(metroMean, metroMean$tapply.cps.Metro..cps.State..mean. >= 0.3)
sort(metroMean$tapply.cps.Metro..cps.State..mean.)
cps = merge(cps, metro, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
length(which(is.na(cps$MetroArea)))
metro <- data.frame(sort(table(cps$MetroArea)))

nrow(subset(cps, cps$MetroArea == 'Atlanta-Sandy Springs-Marietta, GA'))
nrow(subset(cps, cps$MetroArea == 'San Francisco-Oakland-Fremont, CA'))

area <- c('Atlanta-Sandy Springs-Marietta, GA', 'Baltimore-Towson, MD', 'Boston-Cambridge-Quincy, MA-NH', 'San Francisco-Oakland-Fremont, CA')
sum(metro$sort.table.cps.MetroArea..)

hisp <- data.frame(sort(tapply(cps$Hispanic, cps$MetroArea, mean)))
cps$Asian <- cps$Race == 'Asian'

asian <- data.frame(sort(tapply(cps$Asian, cps$MetroArea, mean)))

noDiploma <- data.frame(sort(tapply(cps$Education == "No high school diploma", cps$MetroArea, mean, na.rm=TRUE)))


cps = merge(cps, country, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

length(which(is.na(cps$Country)))

bCountry <- data.frame(sort(table(cps$Country)))

nyCps <- subset(cps, cps$MetroArea == 'New York-Northern New Jersey-Long Island, NY-NJ-PA')


length(which(is.na(nyCps$Country)))

nyCps$IsUSA <- nyCps$Country == 'United States'

1 - mean(nyCps$IsUSA, na.rm=TRUE)

tapply(nyCps$Country != "No high school diploma", cps$MetroArea, mean, na.rm=TRUE
tapply(cps$MetroArea, cps$Country, max, na.rm=TRUE)
subCountry <- subset(cps, cps$Country == 'Somalia', na.rm=TRUE)
subCountry$MetroArea <- as.factor(subCountry$MetroArea)
mData <- data.frame(sort(table(subCountry$MetroArea)))

rm(list=ls())

table(poll$Smartphone)
length(which(is.na(poll$Smartphone)))

table(poll$State, poll$Region)

sessionInfo()
