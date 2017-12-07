#install.packages("geosphere")
set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", fileEncoding = "latin1")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")
st = st[, c('date', 'time', 'latitude', 'longitude', 'air_temperature')] #Choose relevant columns

# Features to predict with
pred_date_text <- "2013-11-04"
pred_date <- as.Date(pred_date_text) # The date to predict (up to the students)
pred_times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00",
                "16:00:00", "18:00:00","20:00:00", "22:00:00","24:00:00")
pred_times <- strptime(pred_times, "%H:%M:%S")
pred_temp_sum <- vector(length=length(pred_times))
pred_temp_prod <- vector(length=length(pred_times))
a <- 58.4274 # a
b <- 14.826 # b
pred_pos <- c(b, a)

# Preprocessing
st$date = as.Date(st$date)
filtered = st[st$date < pred_date,]
filtered$time <- strptime(filtered$time, "%H:%M:%S")

# Distance functions
dist_meters <- function(ref_pos, long, lat){
  return(distHaversine(ref_pos, cbind(long, lat)))
}

dist_date <- function(date, refdate=as.Date("2000-01-01")){
  return(as.numeric(difftime(date, refdate, units='days'))%%365)
}

dist_time <- function(time, reftime=strptime("00:00:00", "%H:%M:%S")){
  return(as.numeric(difftime(time, reftime, units='mins')))
}

# Smoothing factors
h_distance <- sd(dist_meters(c(0,0),filtered$longitude, filtered$latitude))
h_date <- sd(dist_date(filtered$date)) #SD of intra-year minute difference
h_time <- sd(dist_time(filtered$time)) #SD of intra-day minute difference 

# Gaussian kernel
gausskern <- function(u, h){
  u1 <- u/h
  r <- exp(-u1^2)
  return(r)
}

k1 <- gausskern(dist_meters(pred_pos, filtered$longitude, filtered$latitude), h_distance)

k2 <- gausskern(dist_date(filtered$date, refdate = pred_date), h_date)

for (i in 1:length(pred_times)){
  k3 <- gausskern(dist_time(filtered$time, reftime=pred_times[i]), h_time)
  
  pred_temp_sum[i] = sum((k1 + k2 + k3)*filtered$air_temperature) / sum(k1 + k2 + k3)
}




plot(pred_times, pred_temp_sum, type="o", col='red', xlab="Time during day", ylab="Temperature", main=paste("Predicted temperature ", pred_date_text), ylim=c(min(pred_temp_sum)-2,max(pred_temp_sum)+2))
same_day = filtered[(dist_date(filtered[,'date'], refdate = pred_date)==0) | (dist_date(filtered[,'date'], refdate = pred_date)==1) | (dist_date(filtered[,'date'], refdate = pred_date)==364),] 
rbPal <- colorRampPalette(c('red','yellow'))
col_vec <- rbPal(20)[as.numeric(cut(dist_meters(pred_pos, same_day$longitude, same_day$latitude),breaks = 40))]
points(same_day$time, same_day$air_temperature, col=col_vec)




for (i in 1:length(pred_times)){
  k3 <- gausskern(dist_time(filtered$time, reftime=pred_times[i]), h_time)
  
  pred_temp_prod[i] = sum((k1 * k2 * k3)*filtered$air_temperature) / sum(k1 * k2 * k3)
}


plot(pred_times, pred_temp_prod, type="o", col="red", xlab="Time during day", ylab="Temperature", main=paste("Predicted temperature ", pred_date_text), ylim=c(min(pred_temp_prod)-2,max(pred_temp_prod)+2))
same_day = filtered[(dist_date(filtered[,'date'], refdate = pred_date)==0) | (dist_date(filtered[,'date'], refdate = pred_date)==1) | (dist_date(filtered[,'date'], refdate = pred_date)==364),] 
rbPal <- colorRampPalette(c('red','yellow'))
col_vec <- rbPal(20)[as.numeric(cut(dist_meters(pred_pos, same_day$longitude, same_day$latitude), breaks = 40))]
points(same_day$time, same_day$air_temperature, col=col_vec)
