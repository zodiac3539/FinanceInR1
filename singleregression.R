#Single Regression and Covariance
#Let's figure out the relationship between WTI and Stock price of exxon
#You can download the data from https://research.stlouisfed.org/fred2/series/DCOILWTICO/downloaddata
#I store the data in download folder you can change it.

library(tseries)
library(zoo)

xom <- get.hist.quote("XOM", #Tick mark
                       start="2015-01-01", #Start date YYYY-MM-DD
                       end="2015-12-31" #End date YYYY-MM-DD
)

#I am going to use close value only
xom_zoo <- xom$Close

#Plese download the file from stlouisfed
#Limit the range from 2015-01-01 to 2015-12-31 to compare it apple to apple
wti <- read.csv("/Users/seokbongchoi/Downloads/DCOILWTICO.csv")
#When it reads file first, it has categorical format we need to convert it.
wti$VALUE <- as.character(wti$VALUE)
#It also has garbage value "." in data. You can see in Excel. We can clean this with below command
wti <- wti[wti$VALUE!=".", #Get rid of any value that contains "."
           1:2 #I need first column, and second column as well.
           ]
#Finally I want to convert character to numeric value.
wti$VALUE <- as.numeric(wti$VALUE)
wti_zoo <- read.zoo(wti, format="%Y-%m-%d")

#What we need is return.
xom_rate <- (xom_zoo - lag(xom_zoo))/xom_zoo
wti_rate <- (wti_zoo - lag(wti_zoo))/wti_zoo

regression_result <- lm(xom_rate~wti_rate)

plot(y=xom_rate,
     x=wti_rate,
     pch=19, #I want to use dot
     cex = .5, #The size of dot
     main="The regression between Exxon & WTI in 2015",
     ylab="Exxon return",
     xlab="WTI return"
     )
abline(regression_result, col="red") # regression line (y~x)

print(summary(regression_result))


