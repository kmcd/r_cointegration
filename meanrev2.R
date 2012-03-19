library(zoo)
library(tseries)
library(xts)
library(quantmod)

gld <- read.csv("/home/kmcd/src/hawk_moth.bak/data/gld.csv", stringsAsFactors=F)
gdx <- read.csv("/home/kmcd/src/hawk_moth.bak/data/gld.csv", stringsAsFactors=F)

gld <- zoo(gld[,7], as.Date(gld[,2]))
gdx <- zoo(gdx[,7], as.Date(gdx[,2]))

t.zoo <- merge(gld, gdx, all=FALSE)
t <- as.data.frame(t.zoo)

cat("Date range is", format(start(t.zoo)), "to", format(end(t.zoo)), "\n")

m <- lm(gld ~ gdx + 0, data=t)
beta <- coef(m)[1]

cat("Assumed hedge ratio is", beta, "\n")

sprd <- t$gld - beta*t$gdx
ht <- adf.test(sprd, alternative="stationary", k=0)

cat("ADF p-value is", ht$p.value, "\n")

if (ht$p.value < 0.05) {
    cat("The spread is likely mean-reverting\n")
} else {
    cat("The spread is not mean-reverting.\n")
}
