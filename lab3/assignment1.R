#install.packages("geosphere")
set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", fileEncoding = "latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")
st = st[, c('date', 'time', 'latitude', 'longitude', 'air_temperature')] #Choose relevant columns

st$date = as.Date(st$date)

date <- as.Date("2013-11-04") # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00",
           "16:00:00", "18:00:00","20:00:00", "22:00:00","24:00:00")

filtered = st[st$date < date,]

filtered$date <- as.Date(sub('\\d{4}(?=-)','2000', filtered$date, perl=TRUE)) #Leap-year

filtered$time <- strptime(filtered$time, "%H:%M:%S")

dist_date <- function(date, refdate="2000-01-01"){
  return(as.numeric(difftime(date, as.Date(refdate), units='days')))
}

dist_time <- function(time, reftime="00:00:00"){
  return(as.numeric(difftime(time, strptime(reftime, "%H:%M:%S"), units='mins')))
}

# Smoothing factors
h_distance <- sd(distHaversine(c(0,0), cbind(filtered$longitude, filtered$latitude))) * 20
h_date <- sd(dist_date(filtered$date)) / 20 #SD of intra-year minute difference
h_time <- sd(dist_time(filtered$time)) / 2000 #SD of intra-day minute difference 
a <- 58.4274 # Lat
b <- 14.826 # Long
temp <- vector(length=length(times))

gausskern <- function(u, h){
  u1 <- u/h
  r <- exp(-u1^2)
  return(r)
}

k1 <- gausskern(distHaversine(c(b,a), cbind(filtered$longitude, filtered$latitude)), h_distance)

k2 <- gausskern(dist_date(filtered$date, refdate = "2000-11-04"), h_date)

for (i in 1:length(times)){
  k3 <- gausskern(dist_time(filtered$time, reftime=times[i]), h_time)
  
  temp[i] = sum((k1 + k2 + k3)*filtered$air_temperature) / sum(k1 + k2 + k3)
}

mintimes <- numeric(length(times))
tmp <- strptime(times, "%H:%M:%S")
for (i in 1:length(times)){
  mintimes[i] <- dist_time(tmp[i])
}

plot(mintimes, temp, type="o", xlab="Minutes since midnight", main="Predicted temperature 2017-11-04", ylim=c(3,6))
same_day = filtered[dist_date(filtered[,'date'], refdate = "2000-11-04")==0,] 
points(dist_time(same_day$time), same_day$air_temperature)

