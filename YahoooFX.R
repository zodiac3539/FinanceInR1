#Get Currency 
library(quantmod)
from <- c("USD")
to <- c("KRW", "EUR")
getQuote(paste0(from, to, "=X"))
