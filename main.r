setwd("~/DS/finalProject")

data <- read.csv("one.csv",header=TRUE, sep=",")

keep <- c("station", "month", "day", "hour", "weekday", "isHoliday", "windMaxSpeed.m.s", "windMeanSpeed.m.s", "windDirection.grades", "temperature.C", "relHumidity.HR", "airPressure.mb", "precipitation.l.m2", "bikes")
# Select only columns we want to keep
data <- data[keep]

# Remove columns where number of bikes is NA
data <- data[!is.na(data$bikes),]

# Find the mean of windDirection and replace all NAs with it
mean <- mean(data$windDirection.grades, na.rm=TRUE)
data$windDirection.grades[is.na(data$windDirection.grades)] <- mean


require(caret)
set.seed(123)


library(lattice) 
attach(data)

forplot<-make.groups(station=data.frame(value=station,bikes),
                     month =data.frame(value=month,bikes),
                     day=data.frame(value=day,bikes))

detach(data)
xyplot(bikes~value|which, data=forplot,scales=list(relation="free"))


train <- createDataPartition(data$station, times = 1, p=.7, list=FALSE)
train_set <- data[train,]
test_set <- data[-train,]

control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

performance <- function(model, name){
  preds_train <- predict(model, train_set)
  preds_test <- predict(model, test_set)
  print(paste("Model:", name, "RMSE:", RMSE(preds_train, train_set$bikes), 
              RMSE(preds_test, test_set$bikes), "MAE:", 
              MAE(preds_train, train_set$bikes), MAE(preds_test, test_set$bikes)))
  plot(preds_train, train_set$bikes)
  plot(preds_test, test_set$bikes)
}

# Neural networks
require(nnet)
nn <- train(bikes ~ ., data=train_set, method="nnet", linout=TRUE)
performance(nn, "Neural network(nnet)")

avnn <- train(bikes ~., data=train_set, method="avNNet", linout=TRUE)
performance(avnn, "Model averaged nerual network(avNNet)")

require(neuralnet)
nnetwork <- train(bikes~.,data=train_set, method="neuralnet")

tgrid <- expand.grid(
  .mtry = 16:20,
  .splitrule = c("variance", "extratrees", "maxstat"),
  .min.node.size = c(2, 5, 10, 20)
)
# Random forests
treefit <- train(bikes ~., data = train_set, method="ranger", tuneGrid = tgrid, trControl = control)
performance(treefit, "Random forest(ranger)")

# TODO: change into classification problem, ROC analysis for regression(overpredictions more expensive)

near_n <- train(bikes ~., data=test_set, method="knn")

