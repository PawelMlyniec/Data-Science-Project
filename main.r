setwd("./data")

data <- read.csv("station_1_deploy_full.csv",header=TRUE, sep=",")

sum(is.na(data$bikes))

data <- data[!is.na(data$bikes),]

sum(is.na(data$bikes))

