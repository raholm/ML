## ---- assign1-init
library(geosphere)

set.seed(1234567890)

stations <- read.csv("../data/stations.csv",
                     stringsAsFactors=FALSE,
                     fileEncoding="latin1")
temps <- read.csv("../data/temps50k.csv", stringsAsFactors=FALSE)

st <- merge(stations, temps, by="station_number")
data <- st[, c("longitude", "latitude", "date", "time", "air_temperature")]

h_distance <- 10000 # These three values are up to the students
h_date <- 7
h_time <- 3

pred_latitude <- 58.409158 # The point to predict (up to the students)
pred_longitude <- 15.607452
pred_date <- "2016-12-24" # The date to predict (up to the students)
pred_times <- c("02:00:00", "04:00:00", "06:00:00", "08:00:00",
                "10:00:00", "12:00:00", "14:00:00", "16:00:00",
                "18:00:00", "20:00:00", "22:00:00", "24:00:00")

pred_temp <- vector(length=length(pred_times))

## Students’ code here
gaussian.kernel <- function(u) {
    exp(-u^2)
}

distance.pred <- function(X, lat, long, h) {
    distances <- distHaversine(X[, c("longitude", "latitude")],
                               c(long, lat))

    weights <- gaussian.kernel(distances / h)
    sum(X$air_temperature * weights / sum(weights))
}

## distance.pred(st_filtered, pred_latitude, pred_longitude, h_distance)

date.pred <- function(X, date, h) {
    distances <- as.numeric(difftime(X$date, date, units="days"))
    weights <- gaussian.kernel(distances / h)
    sum(X$air_temperature * weights / sum(weights))
}

## date.pred(st_filtered, pred_date, h_date)

time.pred <- function(X, time, h) {
    distances <- abs(as.numeric(difftime(X$time, time, units="hours")))

    weights <- gaussian.kernel(distances / h)

    sum(X$air_temperature * weights / sum(weights))
}

## time.pred(st_filtered, pred_times[1], h_time)

filter <- function(X, date, time) {
    complete_dates <- paste(X$date, X$time)
    complete_dates <- as.POSIXct(complete_dates, format="%Y-%m-%d %H:%M:%S")

    complete_date <- paste(date, time)
    complete_date <- as.POSIXct(complete_date, format="%Y-%m-%d %H:%M:%S")

    idx <- which(complete_dates <= complete_date)

    X <- X[idx,]
    X$time <- as.POSIXct(X$time, format="%H:%M:%S")
    X$date <- as.Date(X$date)
    X
}

kernel.model <- function(X, lat, long, h_dist, date, h_date, time, h_time) {
    date <- as.Date(date)
    X <- filter(X, date, time)
    time <- as.POSIXct(time, format="%H:%M:%S")
    distance.pred(X, lat, long, h_dist) + date.pred(X, date, h_date) + time.pred(X, time, h_time)
}

for (i in 1:length(pred_times)) {
    pred_temp[i] <- kernel.model(data, pred_latitude, pred_longitude, h_distance,
                                 pred_date, h_date, pred_times[i], h_time)
}

x_breaks <- seq(1, length(pred_times), 2)
plot(y=pred_temp, x=1:length(pred_times), type="o", xaxt = "n",
     xlab="Time", ylab="Estimated Temperature")
axis(1, at=x_breaks, labels=pred_times[x_breaks])
## ---- end-of-assign1-init
