#Binomial option pricing (2)
#We can expand the tree to more than 3rd depth.
#U = exp(volatility)
#D = exp(-volatility)
#p = 0.5 (We have the equal chance of making or losing money)
#Risk free rate = 0.02 => exp(0.02)
#For those who are not familiar with data structure, I deliberately used just array.
#I'll make a new code for those who are familiar with tree data structure

library(igraph)

#Define the variable
depth<-5 #How many steps (tree depth) do you want to make
upside_probability<-0.5 #The chance that the stop price goes up
rate <- exp(0.02) #Risk Free rate
volatility <- 0.2
exercise_price <- 100
stock_price <- 100

total_node<-2^depth-1 #Total number of node
G <- graph.tree(n=total_node, children=2) #I'll make a graph whose nodes are 7, and each node has two children
stock_tree <- (1:total_node)

stock_tree[1]<-stock_price
tree_traverse <- 2^(depth-1) -1

for(i in 1:tree_traverse) {
    #We are going to use mathematical trick to represent tree.
    stock_tree[i*2] <- stock_tree[i] * exp(volatility)
    stock_tree[i*2 + 1] <- stock_tree[i] * exp(-volatility)
}

V(G)$name <- round(stock_tree) #Name of the tree
lay <- layout.reingold.tilford(G) #It's tree. You can try other shape with other layout options
plot(G, layout=lay, vertex.size=15, edge.arrow.size=0.1) #Draw the tree.

#As opposed to the stock price, the option pricing starts out with end nodes (bottom nodes)
#I already explained the logic. Just follow it from one by one.
option_price<-(1:total_node)
bottom_node<-tree_traverse + 1

#In order to value the option, we need to calculate bottom line first.
for(i in bottom_node:total_node) {
  after_option <- stock_tree[i] - exercise_price
  
  if( after_option >0 ) {
    option_price[i] <- after_option
  } else {
    option_price[i] <- 0
  }
}

#Discount it back to current time while considering the probabilty of up and down
for(i in tree_traverse:1) {
    option_price[i]<-upside_probability*option_price[i*2]
    option_price[i]<-option_price[i]+(1-upside_probability)*option_price[i*2+1]
    option_price[i]<-option_price[i]/rate
}

V(G)$name <- round(option_price)
plot(G, layout=lay, vertex.size=15, edge.arrow.size=0.1)

