library(xts)
library(quantmod)

# quickly re-source this file
s <- function() source('meanrev.R')

checkPair <- function(sym1, sym2, dateFilter='::')
{
  t.xts <- getCombined(sym1, sym2, dateFilter=dateFilter)

  cat("Date range is", format(start(t.xts)), "to", format(end(t.xts)), "\n")

  # Build linear model
  m <- buildLM(t.xts)

  # Note beta -- http://en.wikipedia.org/wiki/Beta_(finance)
  beta <- getBeta(m)
  cat("Assumed hedge ratio is", beta, "\n")

  # Build spread
  sprd <- buildSpread(t.xts, beta)

  # Test cointegration
  ht <- testCoint(sprd)
  cat("ADF p-value is", as.double(ht$p.value), "\n")

  if (as.double(ht$p.value) < 0.05)
  {
    cat("The spread is likely mean-reverting.\n")
  }
  else
  {
    cat("The spread is not mean-reverting.\n")
  }
}

getCombined <- function(sym1, sym2, dateFilter='::')
{
  # Grab historical data for both symbols
  one <- getSymbols(sym1, auto.assign=FALSE)
  two <- getSymbols(sym2, auto.assign=FALSE)

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