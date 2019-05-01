##adding 311 calls data.
library(data.table)
library(lubridate)
library(dplyr)
library(tigris)
options(tigris_use_cache = TRUE)
library(sp)
library(tidycensus)
library(tidyverse)





##clean and remove unwated

calls$CaseID <- NULL
calls$`Request Type` <- NULL
calls$`Request Details` <- NULL
calls$`Supervisor District` <- NULL
calls$Neighborhood <- NULL
calls$`Police District` <- NULL
calls$Point <- NULL
calls[,c(1,2,5,6,7,8,9,13,14)] <- NULL
names(calls)[c(3,4)] <- c("lat", "lng")

calls$Updated <- mdy_hms(calls$Updated)
calls$year <- year(calls$Updated)
calls$quarter <- quarter(calls$Updated)


##lat and long are zero together
calls <- calls[lng != 0]
calls[lat == 0]


##filter out for the years, call it call. lol 

calls <- calls[year > 2009 & year < 2019]
write.csv(call, "calls_311.csv")

##next time just load this:
calls <- fread("~/Documents/Capstone_Project/calls_311.csv")

#forandrea <- call[year == 2010 & year == 2011]
#write.csv(forandrea, "calls_311_2010_2011.csv")
##now to add GEOID tract.

callz <- tbl_df(call)

library(tigris)
options(tigris_use_cache = TRUE)
library(sp)

## everything the same after 2010
join_mapLook<- function(data, tract_map){
  
  data1 = data[,c("lng","lat")]
  spatial_data  <- SpatialPointsDataFrame(coordinates(data1), data)
  
  proj4string(spatial_data) <- proj4string(tract_map)
  data$GEOID <-  over(spatial_data, tract_map, returnList=FALSE)$GEOID # just grab geoid
  
  return(data)
}

mapLookup <-  function(data){
  mapped_data <- data.frame()
  tract_map <- tracts(state = "CA", county='San Francisco', cb =TRUE, year = 2016)
  
  for(y in 2010:2018){
    print(y)
    data0 = data %>% filter(year==y) %>% drop_na(lat, lng)
    mapped_data <- rbind(mapped_data, join_mapLook(data0, tract_map))
  }
  return(mapped_data)
}

library(lubridate)

calls_geoid <- mapLookup(calls)

###START HERE!
calls_geoid <- as.data.table(calls_geoid)

calls_geoid <- calls_geoid[order(date)]
sum(is.na(calls_geoid$GEOID))



calls_geoid <- as.data.table(calls_geoid)
##remove rows with NA

calls_geoid <- calls_geoid[!is.na(calls_geoid$GEOID),]






##quarterly counts

calls_geoid <- as.data.table(calls_geoid)
calls_geoid$quarter <- quarter(calls_geoid$date)
calls_quarterly <- calls_geoid[, .N, by=list(year, quarter, GEOID)]
names(calls_quarterly)[4] <- 'N_calls_311'

housing_tract_quarter$GEOID <- as.numeric(housing_tract_quarter$GEOID)

data_quarterly <- merge(finaldata, calls_quarterly, by = c('year', 'quarter', 'GEOID'), all.x = TRUE)
data_quarterly <- merge(data_quarterly, housing_tract_quarter, by = c('year', 'quarter', 'GEOID'))
data_quarterly[is.na(data_quarterly$N_calls_311)]$N_calls_311 <- 0

sum(is.na(data_quarterly$GEOID))


###merging with census data
##why these variables?
##Wang, H., Kifer, D., Graif, C., Li, Z.: Crime rate inference with big data. In: KDD???16 (2016)

names(vars)






####FINAL MERGE OF DATA

##adding areas!

areas <- fread("~/Documents/Capstone_Project/areas.csv")
areas$G <- NULL
areas$V1 <- NULL

temp <- data_quarterly[year != 2018]
tract_wide$GEOID <- as.numeric(tract_wide$GEOID)
temp <- merge(data_quarterly, tract_wide, by = c('year', 'GEOID'))
temp <- merge(temp, areas, by = "GEOID")
temp$V1.x <- NULL
temp$V1.y <- NULL

write.csv(temp, "final_clean_data.csv")

####
library(RankAggreg)

analysis <- temp[,c(4:19)]
library(randomForest)
poiz <- randomForest(N ~., data = analysis, importance = TRUE)
summary(poiz)
for_2018 <- analysis[,-1]
est <- predict(poiz, for_2018)

sum(est)
  sum(temp[year==2018]$N)

crime_2018 <- k[year==2018]

write.csv(crime_2018,"crime_2018.csv")

crime_2018[,.N, on = year]

lm(N~., data = analysis)









