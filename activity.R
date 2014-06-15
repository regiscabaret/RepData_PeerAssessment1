activity.data <- read.csv("activity.csv")
'Converting the date field to POSIXlt class while adding time to the date'
activity.data$date <- as.Date(activity.data$date)
activity.data.tb <- data.table(activity.data)
nb.steps.per.day <- na.omit(activity.data.tb[,sum(steps),by=date]$V1)
hist(nb.steps.per.day,col=4,main="",xlab="Number of Steps Taken per Day",breaks=25,labels=TRUE,xlim=c(min(nb.steps.per.day),max(nb.steps.per.day)))
