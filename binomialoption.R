#Binomial option pricing
#U = exp(volatility)
#D = exp(-volatility)
#p = 0.5 (We have the equal chance of making or losing money)
#Risk free rate = 0.02 => exp(0.02)
#For those who are not familiar with data structure, I deliberately used just array.
#I'll make a new code for those who are familiar with tree data structure

library(igraph)
G <- graph.tree(n=7, children=2) #I'll make a graph whose nodes are 7, and each node has two children
rate <- exp(0.02)
volatility <- 0.2
exercise_price <- 100

a <- NULL
a[1] <- 100 #Time0
a[2] <- 100 * exp(volatility) #Time1 when the stock price goes up
a[3] <- 100 * exp(-volatility) #Time1 when the stock price goes down
a[4] <- a[2] * exp(volatility) #Time2 Up-Up
a[5] <- a[2] * exp(-volatility) #Time2 Up-Down
a[6] <- a[3] * exp(volatility) #Time2 Down-up
a[7] <- a[3] * exp(-volatility) #Time2 Down-down => worst case

V(G)$name <- round(a) #Name of the tree
lay <- layout.reingold.tilford(G) #It's tree. You can try other shape with other layout options
plot(G, layout=lay, vertex.size=50, edge.arrow.size=0.5) #Draw the tree.

#As opposed to the stock price, the option pricing starts out with end nodes (bottom nodes)
#I already explained the logic. Just follow it from one by one.
option_price<-(1:7)
for(i in 4:7) {
  after_option <- a[i] - exercise_price
  
  if( after_option >0 ) {
    option_price[i] <- after_option
  } else {
    option_price[i] <- 0
  }
}

#I assumed that the each case(price up & price down) has an equal chance 50:50
#Let's get an expectation from them
option_price[2]<-(0.5*option_price[4]+0.5*option_price[5])/rate
option_price[3]<-(0.5*option_price[6]+0.5*option_price[7])/rate
option_price[1]<-(0.5*option_price[2]+0.5*option_price[3])/rate

V(G)$name <- round(option_price)
plot(G, layout=lay, vertex.size=50, edge.arrow.size=0.5)

