#Calculate convexcivity of a bond
#EX) f.convexity(maturity=10, coupon=7, discount=0.07)

f.convexity = function(maturity, par=100, coupon, discount, k=1)
{
    real_maturity <- maturity * k
    real_coupon <- coupon / k
    real_discount <- discount / k
    
    period <- 1:real_maturity
    cash_flow = NULL
    tt = NULL
    pv_cash_flow = NULL
    
    for(i in 1:real_maturity) {
      cash_flow[i] <- real_coupon
    }
    cash_flow[real_maturity] <- cash_flow[real_maturity] + par

    for(i in 1:real_maturity) {
      pv_cash_flow[i] <- cash_flow[i] / (1+real_discount)^i
    }
    
    for(i in 1:real_maturity) {
        tt[i] <- (period[i] * (period[i] + 1))/k
    }
    
    convexivity <- tt * pv_cash_flow
    convex <- sum(convexivity) / (par * (1+real_discount)^2)
    return(convex)
}
