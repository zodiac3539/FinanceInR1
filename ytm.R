#[How to calculate YTM in r]
#You don't need any library in here.

f.ytm = function(ytm) 
{ 
  E = NULL 
  
  price <- 100
  maturity <- 10
  coupon <- 5
  par <- 100
  
  for (i in 1 : mature) 
  { 
      E[i] <- coupon/(1+ytm)^i
  } 
  E[maturity] <- E[maturity] + par/(1+ytm)^maturity
  sum_e <- sum(E)
  return(sum_e - price)
} 

solution = uniroot(f.ytm, interval=c(0,1))  
ytm = solution$root 
ytm﻿