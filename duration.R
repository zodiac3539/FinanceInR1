#This function allows you to get the macaulay duration of the bond
#Maturity = maturity of the bond i.e., 1yr, 2yr
#par = par value of the bond i.e., 100
#coupon = coupon rate i.e. if it is 8% -> 100*0.8=8
#discount = discount rate or YYM i.e., 0.03, 0.04
#k = how often coupon is given in a year. i.e. semiannual=> k=2
#Example: f.duration(maturity=2, par=100, coupon=8, discount=0.08, k=2)

f.duration = function(maturity, par, coupon, discount, k=1)
{
    duration = NULL
    coupon_payment <- k*maturity
    if(coupon == 0) {
      #zero coupon bond
      return(maturity)
    }
    
    for(i in 1:coupon_payment) {
        if(i==coupon_payment) {
          duration[i] <- ((i/k) * ((coupon/k)+par))/(1+(discount/k))^i
        } else {
          duration[i] <- ((i/k) * (coupon/k))/(1+(discount/k))^i  
        }
    }

    return(sum(duration)/par)
}