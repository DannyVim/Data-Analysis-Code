Sys.setlocale(category = "LC_CTYPE", locale = "Chinese") 

library(dplyr)
library(lubridate)

chicago <- readRDS('chicago.rds')
dim(chicago)
str(chicago)

#select() to choose columns
names(chicago)[1:3]
subset <- select(chicago, city:dptp)
head(subset)
##to omit var
select(chicago, -(city:dptp))
##keep every var that ends with a '2'
subset <- select(chicago, ends_with('2'))
str(subset)
##keep every var that starts with a 'd'
subset <- select(chicago, starts_with('d'))
str(subset)

# filter() to choose rows
## to extract the rows where the levels of pm.5 are greater than 30
chic.f <- filter(chicago,pm25tmean2>30)
str(chic.f)
summary(chic.f$pm25tmean2)
## complex logical sequence
chic.f <- filter(chicago,pm25tmean2>30&tmpd>80)
select(chic.f,date,tmpd,pm25tmean2)

# arrange() to reorder rows
chicago<-arrange(chicago,date)
head(select(chicago,date,pm25tmean2),3)
tail(select(chicago,date,pm25tmean2),3)
## descending order
chicago<-arrange(chicago,desc(date))
head(select(chicago,date,pm25tmean2),3)
tail(select(chicago,date,pm25tmean2),3)

# rename() to name a var in df
chicago <- rename(chicago, dewpoint = dptp, pm25 = pm25tmean2)

# mutate() to compute transformations of vars
chicago<- mutate(chicago,pm25detrend=pm25-mean(pm25,na.rm = TRUE))
head(chicago)
## transmute() drops all non-transformed vars
head(transmute(
  chicago,
  pm10detrend = pm10tmean2 - mean(pm10tmean2, na.rm = TRUE),
  o3detrend = o3tmean2 - mean(o3tmean2, na.rm = TRUE)
))

# group_by() to generate summary stat from strata defined by a var
chicago<-mutate(chicago,year=year(date))
years<-group_by(chicago,year)
summarise(years,pm25=mean(pm25,na.rm = TRUE),
          o3=max(o3tmean2,na.rm = TRUE),
          no2=median(no2tmean2,na.rm = TRUE))

qq <- quantile(chicago$pm25,seq(0,1,0.2),na.rm = TRUE)
chicago<-mutate(chicago,pm25.quint=cut(pm25,qq))
quint <- group_by(chicago,pm25.quint)
summarise(quint,o3=mean(o3tmean2,na.rm = TRUE),
          no2=mean(no2tmean2,na.rm = TRUE))

# %>% pipeline operator
mutate(chicago,pm25.quint=cut(pm25,qq))%>%
  group_by(pm25.quint)%>%
  summarise(o3=mean(o3tmean2,na.rm = TRUE),
            no2=mean(no2tmean2,na.rm = TRUE))

mutate(chicago,month=month(date))%>%
  group_by(month)%>%
  summarise(pm25=mean(pm25,na.rm = TRUE),
            o3=mean(o3tmean2,na.rm = TRUE),
            no2=median(no2tmean2,na.rm = TRUE))

