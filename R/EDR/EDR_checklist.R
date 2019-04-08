# EDA Checklist
## 1. Formulate your question
## 2. Read in your data
## 3. Check the packaging
## 4. Run str()
## 5. Look at the top and the bottom of your data
## 6. Check your “n”s
## 7. Validate with at least one external data source
## 8. Try the easy solution first
## 9. Challenge your solution
## 10. Follow up

library(dplyr)
# 1 Question
## Which counties in the United States have the highest levels of ambient ozone pollution?

# 2 Data
library(readr)
ozone <- read_csv("hourly_44201_2014.csv", col_types = "ccccinnccccccncnnccccccc")
## to remove any spaces
names(ozone)<-make.names(names(ozone))

# 3 Check
nrow(ozone)
ncol(ozone)
dim(ozone)

# 4 str()
str(ozone)

# 5 Top and Bottom
head(ozone[,c(6:7,10)])
tail(ozone[,c(6:7,10)])

# 6 'n's
table(ozone$Time.Local)
##look at which obs were measured at time '01:00'
filter(ozone,Time.Local == '01:00')%>%
  select(State.Name,County.Name,Date.Local,Time.Local,Sample.Measurment)
##pulled all mearurements taken at this monitor on this date
filter(ozone,State.Code=='36'&County.Code=='033'&Date.Local=='2014-09-30')%>%
  select(Date.Local,Time.Local,Sample.Measurement)%>%
  as.data.frame()
##how many states
select(ozone,State.Name)%>%unique%>%nrow
###We knew that there are only 50 states in the U.S., so seeing 53 state names was an immediate trigger that something might be off.

# 7 Validate with external data source
summary(ozone$Sample.Measurement)
quantile(ozone$Sample.Measurement,seq(0,1,0.1))

# 8 easy solution first
ranking <- group_by(ozone,State.Name,County.Name)%>%
  summarise(ozone=mean(Sample.Measurement))%>%
  as.data.frame%>%
  arrange(desc(ozone))
head(ranking,10)
tail(ranking,10)

## look at one the highert level counties
filter(ozone,State.Name=='California'&County.Name=='Mariposa')%>% nrow
## there’s 24 hours in a day and 365 days per, which gives us 8760, which is close to that number of observations.
ozone <- mutate(ozone,Date.Local = as.Date(Date.Local))
## split the data by month to look at the average hourly levels
filter(ozone,State.Name=='California' & County.Name=='Mariposa')%>%
  mutate(month=factor(months(Date.Local),levels = month.name))%>%
  group_by(month)%>%
  summarise(ozone=mean(Sample.Measurement))
## look at one of the lowest level counties
filter(ozone,State.Name=='Oklahoma' & County.Name=='Caddo')%>%nrow
filter(ozone,State.Name=='Oklahoma' & County.Name=='Caddo')%>%
  mutate(month=factor(months(Date.Local),levels = month.name))%>%
  group_by(month)%>%
  summarise(ozone=mean(Sample.Measurement))

# 9 Challenge your solution
## Resample to reduce sample deviation
set.seed(10234)
N <- nrow(ozone)
idx <- sample(N,N,replace = TRUE)
ozone2<-ozone[idx,]

ranking2 <- group_by(ozone2,State.Name,County.Name)%>%
  summarise(ozone=mean(Sample.Measurement))%>%
  as.data.frame%>%
  arrange(desc(ozone))

cbind(head(ranking,10),head(ranking2,10))
cbind(tail(ranking,10),tail(ranking2,10))

# 10 Follow up
## Do you have the right data?
## Do you need other data?
## Do you have the right question?
