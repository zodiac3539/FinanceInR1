#IRR (Internal Rate of Return)
#The discount rate that makes NPV value zero.
#You can't use IRR function, if there is a cash outflow in the future.
#IRR can be used only when the all cash outflows take place at time 0.
#Please make sure that cash flow(cf) should take a form of vector
#cf=c(50,50,50)
#<Example>
#f.irr(upfront=-100, cf=c(50,50,50))

f.irr = function(upfront, cf) {
  f.npv = function(x) {
      npv <- upfront
      for(i in 1:length(cf)) {
          npv <- npv + cf[i]/(1+x)^i
      }
      return(npv)
  }
  solution <- uniroot(f.npv, interval=c(0,1))  
  return(solution$root)
}
