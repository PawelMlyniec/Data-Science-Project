setwd("./data")

data <- read.csv("one.csv",header=TRUE, sep=",")

keep <- c("station", "month", "day", "hour", "weekday", "isHoliday", "windMaxSpeed.m.s", "windMeanSpeed.m.s", "windDirection.grades", "temperature.C", "relHumidity.HR", "airPressure.mb", "precipitation.l.m2", "bikes")

data <- data[keep]

any(apply(data, 2, function(x) any(is.na(x))))
apply(data, 2, function(x) any(is.na(x)))

sum(is.na(data$bikes))
data <- data[!is.na(data$bikes),]
sum(is.na(data$bikes))


mean <- mean(data$windDirection.grades, na.rm=TRUE)
data$windDirection.grades[is.na(data$windDirection.grades)] <- mean

any(apply(data, 2, function(x) any(is.na(x))))
apply(data, 2, function(x) any(is.na(x)))
nrow(data)
