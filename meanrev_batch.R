library(xts)
library(quantmod)

check_pair <- function(sym1, sym2, dateFilter='::')
{
  t.xts <- getCombined(sym1, sym2, dateFilter=dateFilter)

  # cat("Date range is", format(start(t.xts)), "to", format(end(t.xts)), "\n")

  # Build linear model
  m <- buildLM(t.xts)

  # Note beta -- http://en.wikipedia.org/wiki/Beta_(finance)
  beta <- getBeta(m)
  # cat("Assumed hedge ratio is", beta, "\n")

  # Build spread
  sprd <- buildSpread(t.xts, beta)

  # Test cointegration
  ht <- testCoint(sprd)

  if (as.double(ht$p.value) < 0.05)
  {
    cat( names(sym1)[1], " ", names(sym2)[1] , " ", as.double(ht$p.value), " ")
    cat( as.double(sd(Delt(sym1[,6], sym2[,6]))), "\n" )
  }
  else
  {
    cat("")
    # cat("The spread is not mean-reverting.\n")
  }
}

getCombined <- function(sym1, sym2, dateFilter='::')
{
  # Grab historical data for both symbols
  # one <- getSymbols(sym1, auto.assign=FALSE)
  # two <- getSymbols(sym2, auto.assign=FALSE)

  one <- sym1
  two <- sym2
  
  # Give columns more usable names
  colnames(one) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
  colnames(two) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')

  # Build combined object
  return(merge(one$Close, two$Close, all=FALSE)[dateFilter])
}

buildLM <- function(combined)
{
  return(lm(Close ~ Close.1 + 0, combined))
}

getBeta <- function(m)
{
  return(as.double(coef(m)[1]))
}

buildSpread <- function(combined, beta)
{
  return(combined$Close - beta*combined$Close.1)
}

testCoint <- function(sprd)
{
  return(PP.test(sprd, lshort = FALSE))
}