#NPV Calculation
#When you define the cash flow, just use below like
#cf <- c(10, 20, 30, 40)
#<Example>
#f.npv(discount=0.05, upfront=-100, cf=c(20,30,40,50))

f.npv = function(discount, upfront, cf) {
  npv <- upfront
  for(i in 1:length(cf)) {
      npv <- npv + cf[i]/(1+discount)^i
  }
  return(npv)
}
