################################################################################
# Replication "On Binscatter"
# Cattaneo, Crump, Farrell and Feng (2022)
# Date: 31-OCT-2022
# ACS Application
# source("CCFF_2022_Binscatter_ACS.R")
################################################################################

rm(list=ls())

library(haven)
library(binsreg)
library(ggplot2)
library(dplyr)

data <- read_dta(file='CCFF_2022_ACS_1.dta')
data <- subset(data, select=c(uninsuredRate,perCapitaIncome,percentBachelorsEdu,medianAge,percentHsEdu,ueRate))

################################################################################
# 1(a) Plain scatter
################################################################################
png('graphs/Census_Scatter.png')
plot(data$perCapitaIncome, data$uninsuredRate, col='navy', pch=19 ,xlab="Per Capita Income", ylab="Percent Uninsured" )
dev.off()

################################################################################
# 1(b) Demonstration of scatter to binscatter
################################################################################
png('graphs/Census_ScatterAndBinscatter.png')
res <- binsreg(data$uninsuredRate, data$perCapitaIncome, nbins=10, line=c(0,0), randcut=1)
line <- res$data.plot$`Group Full Sample`$data.line
dot <- res$data.plot$`Group Full Sample`$data.dots

line <- as.list(line[line$isknot == 1,]$x)

plot(data$perCapitaIncome, data$uninsuredRate, col='gray', pch=19 ,xlab="Per Capita Income", ylab="Percent Uninsured", xlim=c(0,80000), ylim=c(0,40))
points(dot$x, dot$fit, col='blue', pch=19, cex=1.2)

for (l in line){
  abline(v=l, col="black",  lty=2)
}
dev.off()

################################################################################
# 1(c) Demonstration of conventional binscatter plot
################################################################################
png('graphs/Census_BinscatterAndLine.png')
res <- binsreg(data$uninsuredRate, data$perCapitaIncome, nbins=10, plotxrange=c(0,80000), polyreg=1, randcut=1)
line <- res$data.plot$`Group Full Sample`$data.poly
dots <- res$data.plot$`Group Full Sample`$data.dots

plot(dots$x, dots$fit, col='blue', pch=19, xlab="Per Capita Income", ylab="Percent Uninsured", xlim=c(0,80000), ylim=c(0,40))
lines(line$x, line$fit, col='forestgreen')
dev.off()

################################################################################
# 1(d) Demonstration of piecwise constant conditional mean estimate
################################################################################
png('graphs/Census_BinscatterAndCondExp.png')
res <- binsreg(data$uninsuredRate, data$perCapitaIncome, nbins=10, plotxrange=c(0,80000), polyreg=1, randcut=1)
line <- res$data.plot$`Group Full Sample`$data.poly
dots <- res$data.plot$`Group Full Sample`$data.dots

plot(line$x, line$fit, type="l", col='forestgreen', xlab="Per Capita Income", ylab="Percent Uninsured", xlim=c(0,80000), ylim=c(0,40))

dots <- dots$fit
stops <- c(0, line[line$isknot == 1, ]$x, 10000000)

counter = 2
for (dot in dots){
  lines(c(stops[counter-1], stops[counter]) , c(dot, dot))
  counter = counter + 1
}
dev.off()

################################################################################
# 1(e) Confidence Band
################################################################################
res <- binsreg(data$uninsuredRate, data$perCapitaIncome, randcut=1, cb=T)
dev.off
tmp <- res$bins_plot + xlab("Per Capita Income") + ylab("Percent Uninsured") + xlim(0,80000) + ylim(0,40)
png('graphs/Census_CB_noControls.png')
plot(tmp)
dev.off()

################################################################################
# 1(f) Conditional quantiles
################################################################################
png('graphs/Census_CQ_noControls.png')
res_1 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, quantile=.1, randcut=1)
res_5 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, quantile=.5, randcut=1)
res_9 <- binsqreg(data$uninsuredRate, data$perCapitaIncome, quantile=.9, randcut=1)

dots_1 <- res_1$data.plot$`Group Full Sample`$data.dots
dots_5 <- res_5$data.plot$`Group Full Sample`$data.dots
dots_9 <- res_9$data.plot$`Group Full Sample`$data.dots

plot(dots_9$x, dots_9$fit, col='gray', pch=19, xlab="Per Capita Income", ylab="Percent Uninsured", xlim=c(0,100000), ylim=c(0,40))
points(dots_5$x, dots_5$fit, col='black', pch=19)
points(dots_1$x, dots_1$fit, col='gray', pch=19)
legend("topright", legend=c("10th", "50th", "90th"), col=c("gray", "black",'gray'), pch=19)
dev.off()

################################################################################
# (2) Conditioning on Covariates
################################################################################
w <- select(data, c('percentBachelorsEdu','medianAge','percentHsEdu','ueRate'))
data2 = data[complete.cases(data), ]
w2 <- select(data2, c('percentBachelorsEdu','medianAge','percentHsEdu','ueRate'))

reg_x <- lm(formula = perCapitaIncome ~ percentBachelorsEdu + medianAge + percentHsEdu + ueRate, data=data2)
resid_x <- resid(reg_x) + mean(data2$perCapitaIncome)

reg_y <- lm(formula = uninsuredRate ~ percentBachelorsEdu + medianAge + percentHsEdu + ueRate, data=data2)
resid_y <- resid(reg_y) + mean(data2$uninsuredRate)

png('graphs/Census_binscatter_withControls.png')
res <- binsreg(resid_y, resid_x, w2, nbins=20, polyreg=1, plotxrange =c(0,80000), randcut=1)
res$bins_plot + xlab("Per Capita Income") + ylab("Percent Uninsured")
dev.off()

png('graphs/Census_binsreg_withControls.png')
res <- binsreg(data$uninsuredRate, data$perCapitaIncome, w, randcut=1, polyreg=1, plotxrange =c(0,80000))
res$bins_plot + xlab("Per Capita Income") + ylab("Percent Uninsured")
dev.off()

################################################################################
# (6) Logit Specification
################################################################################
data$high_uninsured = 1*(data$uninsuredRate >=10)

png('graphs/Census_scatterLDV.png')
plot(data$perCapitaIncome, data$high_uninsured,  pch=19, xlab="Per Capita Income", ylab="")
dev.off()

res <- binsglm(data$high_uninsured,data$perCapitaIncome, family=binomial(link='logit'), cb=T, randcut=1)
dev.off()
tmp <- res$bins_plot + xlab("Per Capita Income") + ylab("") + xlim(0,100000) + ylim(0,1)
png("graphs/Census_binslogit.png")
plot(tmp)
dev.off()

res <- binsglm(data$high_uninsured,data$perCapitaIncome, w, family=binomial(link='logit'), cb=T, randcut=1)
dev.off()
tmp <- res$bins_plot + xlab("Per Capita Income") + ylab("") + xlim(0,100000) + ylim(0,1)
png("graphs/Census_binslogit_withControls.png")
plot(tmp)
dev.off()

################################################################################
# (4) Graphical Representation of Parametric Specification Testing
################################################################################
dataT <- subset(data, select=c(uninsuredRate,perCapitaIncome))
dataT = dataT[complete.cases(dataT), ]

res <- binsreg(dataT$uninsuredRate, dataT$perCapitaIncome, randcut=1, polyreg=3, cb=T, plotxrange =c(0,80000), plotyrange = c(0,25))
lin <- binsreg(dataT$uninsuredRate, dataT$perCapitaIncome, randcut=1, polyreg=1, cb=T, plotxrange =c(0,80000), plotyrange = c(0,25))
lin <- lin$data.plot$`Group Full Sample`$data.poly

tmp <- res$bins_plot + geom_line(aes(lin$x, lin$fit, color='black')) + xlab("Per Capita Income") + ylab("Percent Uninsured") + ylim(0,25)
png('graphs/Census_binsreg_withParametricFit.png')
plot(tmp)
dev.off()

res <- binsreg(data2$uninsuredRate, data2$perCapitaIncome, w2, randcut=1, polyreg=3, cb=T, plotxrange =c(0,80000), plotyrange = c(5,15))
lin <- binsreg(data2$uninsuredRate, data2$perCapitaIncome, w2, randcut=1, polyreg=1, cb=T, plotxrange =c(0,80000), plotyrange = c(5,15))
lin <- lin$data.plot$`Group Full Sample`$data.poly

tmp <- res$bins_plot + geom_line(aes(lin$x, lin$fit, color='black')) + xlab("Per Capita Income") + ylab("Percent Uninsured") + ylim(5,15)
png('graphs/Census_binsreg_withParametricFit_withControls.png')
plot(tmp)
dev.off()

################################################################################
# (3) Parametric Specification Testing
################################################################################
min <- apply(w, 2, FUN = min, na.rm=T)
max <- apply(w, 2, FUN = max, na.rm=T)
mean <- apply(w, 2, FUN = mean, na.rm=T)

png('graphs/Census_binsreg_withControls_atMin.png')
res <- binsreg(data$uninsuredRate, data$perCapitaIncome, w, at=min, randcut=1, polyreg=1, plotxrange=c(0,80000))
res$bins_plot + xlab("Per Capita Income") + ylab("Percent Uninsured")
dev.off()

png('graphs/Census_binsreg_withControls_atMax.png')
res <- binsreg(data$uninsuredRate, data$perCapitaIncome, w, at=max, randcut=1, polyreg=1, plotxrange=c(0,80000))
res$bins_plot + xlab("Per Capita Income") + ylab("Percent Uninsured")
dev.off()

png('graphs/Census_binsreg_withControls_atMean.png')
res <- binsreg(data$uninsuredRate, data$perCapitaIncome, w, at=mean, randcut=1, polyreg=1, plotxrange=c(0,80000))
res$bins_plot + xlab("Per Capita Income") + ylab("Percent Uninsured")
dev.off()

#### Shape Tests
## Polynomial
# No controls
binstest(data$uninsuredRate, data$perCapitaIncome, randcut=1, testmodelpoly=1, nsims=50000)
binstest(data$uninsuredRate, data$perCapitaIncome, randcut=1, lp=2, testmodelpoly=1, nsims=50000)
binstest(data$uninsuredRate, data$perCapitaIncome, randcut=1, testmodelpoly=3, nsims=50000)
binstest(data$uninsuredRate, data$perCapitaIncome, randcut=1, lp=2, testmodelpoly=3, nsims=50000)
# With Controls
binstest(data$uninsuredRate, data$perCapitaIncome, w, at=min, randcut=1, testmodelpoly=1, nsims=50000)
binstest(data$uninsuredRate, data$perCapitaIncome, w, at=min, randcut=1, lp=2, testmodelpoly=1, nsims=50000)
binstest(data$uninsuredRate, data$perCapitaIncome, w, at=min, randcut=1, testmodelpoly=3, nsims=50000)
binstest(data$uninsuredRate, data$perCapitaIncome, w, at=min, randcut=1, lp=2, testmodelpoly=3, nsims=50000)

binstest(data$uninsuredRate, data$perCapitaIncome, w, at=max, randcut=1, testmodelpoly=1, nsims=50000)
binstest(data$uninsuredRate, data$perCapitaIncome, w, at=max, randcut=1, lp=2, testmodelpoly=1, nsims=50000)
binstest(data$uninsuredRate, data$perCapitaIncome, w, at=max, randcut=1, testmodelpoly=3, nsims=50000)
binstest(data$uninsuredRate, data$perCapitaIncome, w, at=max, randcut=1, lp=2, testmodelpoly=3, nsims=50000)

binstest(data$uninsuredRate, data$perCapitaIncome, w, at=mean, randcut=1, testmodelpoly=1, nsims=50000)
binstest(data$uninsuredRate, data$perCapitaIncome, w, at=mean, randcut=1, lp=2, testmodelpoly=1, nsims=50000)
binstest(data$uninsuredRate, data$perCapitaIncome, w, at=mean, randcut=1, testmodelpoly=3, nsims=50000)
binstest(data$uninsuredRate, data$perCapitaIncome, w, at=mean, randcut=1, lp=2, testmodelpoly=3, nsims=50000)
## Derivatives
binstest(data$uninsuredRate, data$perCapitaIncome, randcut=1, testshapel=0, deriv=1, nsims=50000)
binstest(data$uninsuredRate, data$perCapitaIncome, w, randcut=1, testshapel=0, deriv=1, nsims=50000)

binstest(data$uninsuredRate, data$perCapitaIncome, randcut=1, testshaper=0, deriv=2, nsims=50000)
binstest(data$uninsuredRate, data$perCapitaIncome, w, randcut=1, testshaper=0, deriv=2, nsims=50000)

####################################################################################
# (5) Two-Sample comparison
####################################################################################
data3 <- read_dta(file='CCFF_2022_ACS_2.dta')
w3 <- select(data3, c('percentBachelorsEdu','medianAge','percentHsEdu','ueRate'))

res <- binsreg(data3$uninsuredRate, data3$perCapitaIncome, by=data3$idxpopdens, cb=T, randcut=1, legendTitle="Population Density")
dev.off()
tmp <- res$bins_plot + geom_boxplot() + xlab('Per Capita Income') + ylab('Uninsured Rate') + theme(legend.position=c(.85,.90)) + xlim(0,80000) + ylim(0,30)
png('graphs/Census_binsreg_byPopDensity.png')
plot(tmp)
dev.off()

res <- binsreg(data3$uninsuredRate, data3$perCapitaIncome, w3, by=data3$idxpopdens, cb=T, randcut=1, plotxrange=c(0,80000), plotyrange=c(0,30),  legendTitle='Population Density')
dev.off()
tmp <- res$bins_plot + geom_boxplot() + xlab('Per Capita Income') + ylab('Uninsured Rate') + theme(legend.position=c(.85,.90)) + xlim(0,80000) + ylim(0,30)
png('graphs/Census_binsreg_byPopDensity_withControls.png')
plot(tmp)
dev.off()
