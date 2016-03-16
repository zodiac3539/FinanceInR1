#You need to install "XML" package first.
#If you don't have it, please execute install.packages("XML") first.
library("XML")
libor_temp <- readHTMLTable("http://www.bankrate.com/rates/interest-rates/libor.aspx")
#This is a list type. List includes any data type. That's why we use double bracket [[]]
libor_temp <- libor_temp[[1]]
period_temp <- c("1 month", "3 month", "6 month", "1 year")

#As the type of these data is factor(Class), we need to convert this to numeric type
#Direct conversion doesn't work as it interpret each class into just single number
libor_temp <- as.character(libor_temp$`This week`)
libor_temp <- as.numeric(libor_temp)

#The web site contains a lot of informations. All we need is just Libor
libor_temp <- c(libor_temp[3], libor_temp[4], libor_temp[5], libor_temp[7])
libor <- data.frame(period=period_temp, libor=libor_temp)

x <- c(1,3,6,12)
y <- libor$libor

#xlab => Label for X-axis
#ylab => Label for Y-axis
#ylim => The range on Y-axis

plot(x,y,xlab = "Month", ylab="Libor Rate (%)", ylim=c(0,max(libor$libor)))
lines(x,y)
