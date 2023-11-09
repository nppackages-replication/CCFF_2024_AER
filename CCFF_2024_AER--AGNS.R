################################################################################
# Replication "On Binscatter"
# Cattaneo, Crump, Farrell and Feng (2023)
# Date: 8-JUL-2023
# AGNS Application
################################################################################

# source("CCFF_2023_Binscatter_AGNS.R")

rm(list=ls())

library(haven)
library(binsreg)
library(ggplot2)
library(plyr)
library(dplyr)
library(lfe)

##### Note that some figures may differ from the manuscript because of the evaluation point of the control variables; see the discussion in the Supplemental Appendix

data <- read_dta(file='CCFF_2023_AGNS.dta')
data2 <- data[complete.cases(data), ]

################################################################################
# Figure 1(a) Scatter plot
################################################################################
png('graphs/AGNS_scatter.png')
plot(data$mtr90_lag3, data$lnpat, xlab="Combined Marginal Tax Rate for 90th Percentile", ylab="Log Patents", pch=19, col='navy', xlim=c(-.9,0), ylim=c(0,10))
dev.off()

################################################################################
# Figure 1(b) Demonstration of scatter to binscatter
################################################################################
res <- binsreg(data$lnpat, data$mtr90_lag3, nbins=10, line=c(0,0))
lines <- res$data.plot$`Group Full Sample`$data.line
l <- dim(lines)
verts <- rbind(lines[!duplicated(lines$bin),],lines[l[1],])
dots <- res$data.plot$`Group Full Sample`$data.dots

ggplot() + geom_point(aes(data$mtr90_lag3, data$lnpat), colour="blue", alpha=.10) + geom_point(aes(dots$x, dots$fit),colour="blue", size=5) + geom_vline(xintercept=verts$x, linetype="dashed")
ggsave('graphs/AGNS_ScatterAndBinscatter.png')
dev.off()

################################################################################
# Figure 1(c) Demonstration of conventional binscatter plot
################################################################################

res <- binsreg(data$lnpat, data$mtr90_lag3, nbins=10, polyreg=1)
dev.off()
tmp <- res$bins_plot + xlab("Combined Marginal Tax Rate for 90th Percentile") + ylab("Log Patents") + xlim(-0.9, 0) + ylim(4, 8)
png('graphs/AGNS_BinscatterAndLine.png')
plot(tmp)
dev.off()

################################################################################
# Figure 1(d) Demonstration of piecewise constant conditional mean estimate
################################################################################

res <- binsreg(data$lnpat, data$mtr90_lag3, nbins=10, line=c(0,0), polyreg=1)
dev.off()
tmp <- res$bins_plot + xlab("Combined Marginal Tax Rate for 90th Percentile") + ylab("Log Patents") + xlim(-0.9, 0) + ylim(4, 8)
png('graphs/AGNS_BinscatterAndCondExp.png')
plot(tmp)
dev.off()


################################################################################
# Figure 2 Conditioning on covariates
################################################################################

# Residualized Binscatter
reg <- felm(lnpat ~ top_corp_lag3 + lreal_gdp_pc + lpopulation_density + rd_credit_lag3 + as.factor(statenum) + as.factor(year), data2, weight=data2$pop1940)
resid_lnpat <- resid(reg) + mean(data$lnpat, na.rm=T)
reg <- felm(mtr90_lag3 ~ top_corp_lag3 + lreal_gdp_pc + lpopulation_density + rd_credit_lag3 + as.factor(statenum) + as.factor(year), data2, weight=data2$pop1940)
resid_mtr90_lag3 <- resid(reg) + mean(data$mtr90_lag3, na.rm=T)

png('graphs/AGNS_binscatter.png')
res <- binsreg(resid_lnpat, resid_mtr90_lag3, weights=data2$pop1940, nbins=50, polyreg=1)
res$bins_plot + xlab("Combined Marginal Tax Rate for 90th Percentile") + ylab("Log Patents") + ylim(5.7,6.2) + xlim(-.525,-.325)
dev.off()

# Correct handling of covariates (w/ regression line)
w <- model.matrix(~ top_corp_lag3 + lreal_gdp_pc + lpopulation_density + rd_credit_lag3 + as.factor(year) + as.factor(statenum) - 1, data=data2)

png('graphs/AGNS_covariateAdjustments_binsreg_wRegLine.png')
res <- binsreg(data2$lnpat, data2$mtr90_lag3, w, weights=data2$pop1940, at="mean", nbins=50, polyreg=1)
res$bins_plot + xlab("Combined Marginal Tax Rate for 90th Percentile") + ylab("Log Patents")
dev.off()

# Correct handling of covariates (w/out regression line)
png('graphs/AGNS_covariateAdjustments_binsreg_woutRegLine.png')
res <- binsreg(data2$lnpat, data2$mtr90_lag3, w, weights=data2$pop1940, at="mean", nbins=50)
res$bins_plot + xlab("Combined Marginal Tax Rate for 90th Percentile") + ylab("Log Patents")
dev.off()

################################################################################
# Figure 3 Confidence bands
################################################################################

#### Note that Figure 3 will differ from the manuscript because two-way clustering is not available in current version of R code

# Direct binsreg implementation of confidence band
png('graphs/AGNS_confidenceBandwDots.png')
res <- binsreg(data2$lnpat, data2$mtr90_lag3, w, weights=data2$pop1940, at="mean", cb=T, randcut=1, cluster=data2$fiveyrblockbystate)
dev.off()

################################################################################
# Figure 4 choice of J
################################################################################

#### Note that Figure 4(b) will differ from the manuscript because two-way clustering is not available in current version of R code

res <- binsreg(data2$lnpat, data2$mtr90_lag3, w, weights=data2$pop1940, at="mean", nbins=5, polyreg=1)
dev.off()
tmp <- res$bins_plot + xlab("Combined Marginal Tax Rate for 90th Percentile") + ylab("Log Patents") + xlim(-0.9, 0) + ylim(6,9)
png('graphs/AGNS_nbins5.png')
plot(tmp)
dev.off()

res <- binsreg(data2$lnpat, data2$mtr90_lag3, w, weights=data2$pop1940, at="mean", randcut=1, polyreg=1, cluster=data2$fiveyrblockbystate)
dev.off()
tmp <- res$bins_plot + xlab("Combined Marginal Tax Rate for 90th Percentile") + ylab("Log Patents") + xlim(-0.9, 0) + ylim(6,9)
png('graphs/AGNS_nbinsOptimal.png')
plot(tmp)
dev.off()

res <- binsreg(data2$lnpat, data2$mtr90_lag3, w, weights=data2$pop1940, at="mean", nbins=50, polyreg=1)
dev.off()
tmp <- res$bins_plot + xlab("Combined Marginal Tax Rate for 90th Percentile") + ylab("Log Patents") + xlim(-0.9, 0) + ylim(6,9)
png('graphs/AGNS_nbins50.png')
plot(tmp)
dev.off()

################################################################################
# Figure 5 Confidence bands
################################################################################

#### Note that Figure 5 will differ from the manuscript because two-way clustering is not available in current version of R code

res <- binsreg(data2$lnpat, data2$mtr90_lag3, w, weights=data2$pop1940, at='mean', nbins=5, randcut=1, cluster=data2$fiveyrblockbystate, cb=c(0,0))
dev.off()
tmp <- res$bins_plot + geom_hline(yintercept = 7.055) + xlab("Combined Marginal Tax Rate for 90th Percentile") + ylab("Log Patents") + xlim(-0.9, 0) + ylim(6.8,7.3) 
png('graphs/AGNS_fixedJconfidenceBandwDots.png')
plot(tmp)
dev.off()


################################################################################
# Figure SA-1 Evaluation Point
################################################################################
rm(list=ls())

data <- read_dta(file='CCFF_2023_AGNS.dta')
data2 <- data[complete.cases(data), ]

w0 <- model.matrix(~ top_corp_lag3 + lreal_gdp_pc + lpopulation_density + rd_credit_lag3, data=data2)

res <- binsreg(data2$lnpat, data2$mtr90_lag3, w0, weights=data2$pop1940, at=apply(w0,2,min), polyreg=1, randcut=1)
dev.off()
tmp <- res$bins_plot + xlab("Combined Marginal Tax Rate for 90th Percentile") + ylab("Log Patents") 
png('graphs/AGNS_wControls_atWmin.png')
plot(tmp)
dev.off()

res <- binsreg(data2$lnpat, data2$mtr90_lag3, w0, weights=data2$pop1940, at="mean", polyreg=1, randcut=1)
dev.off()
tmp <- res$bins_plot + xlab("Combined Marginal Tax Rate for 90th Percentile") + ylab("Log Patents") 
png('graphs/AGNS_wControls_atWmean.png')
plot(tmp)
dev.off()

res <- binsreg(data2$lnpat, data2$mtr90_lag3, w0, weights=data2$pop1940, at="median", polyreg=1, randcut=1)
dev.off()
tmp <- res$bins_plot + xlab("Combined Marginal Tax Rate for 90th Percentile") + ylab("Log Patents") 
png('graphs/AGNS_wControls_atWmedian.png')
plot(tmp)
dev.off()

res <- binsreg(data2$lnpat, data2$mtr90_lag3, w0, weights=data2$pop1940, at=apply(w0,2,max), polyreg=1, randcut=1)
dev.off()
tmp <- res$bins_plot + xlab("Combined Marginal Tax Rate for 90th Percentile") + ylab("Log Patents") 
png('graphs/AGNS_wControls_atWmax.png')
plot(tmp)
dev.off()
