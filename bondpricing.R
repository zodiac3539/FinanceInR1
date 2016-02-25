#Pricing Bond example
#Don't need any library
#mature = maturity, coupon: 3%@100 => 3
#par => Normally 100. Discount: YTM

f.bondprice = function (mature, coupon, par, discount)
{
  E = NULL
  sum_e = 0;
  if(mature == 1) {
    E[1] <- (coupon +par) /(1+discount)
    sum_e <- sum(E)
  } else {
    for (i in 1:mature) 
    { 
        E[i] <- coupon/((1+discount)^i)
    } 
    E[mature] <- E[mature] + par/((1+discount)^mature)
    sum_e <- sum(E)
  }

  return (sum_e)
}

