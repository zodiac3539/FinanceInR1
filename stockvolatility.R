library(tseries)
#Getting data from server

data <- get.hist.quote("AAPL", #Tick mark
                       start="2014-01-01", #Start date YYYY-MM-DD
                       end="2014-12-31" #End date YYYY-MM-DD
                       )

#We only take into account "Closing price", the price when the market closes
yesterdayprice <- data$Close
#This is a unique feature of R better than Excel
#I need to calculate everyday return
#The stock return is defined as (today price - yesterday price)/today price
todayprice <- lag(yesterdayprice)
#ret <- log(lag(price)) - log(price)
rets <- (todayprice - yesterdayprice)/todayprice
#Annualized and percentage 
vol <- sd(rets) * sqrt(length(todayprice))

#Draw the histogram on the return
hist(rets, xlab="return", ylab="Frequency", main="Histogram of Apple stock in 2015")

#String concatenation
output <- paste("Volitility: ", vol, sep="")
output <- paste(output, "%", sep="")
print(output)
