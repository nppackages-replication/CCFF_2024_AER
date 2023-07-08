################################################################################
# Replication "On Binscatter"
# Cattaneo, Crump, Farrell and Feng (2023)
# Date: 8-JUL-2023
# Moretti (2021) Application
################################################################################

# source("CCFF_2023_Binscatter_M.R")

rm(list=ls())

library(haven)
library(binsreg)
library(ggplot2)
library(lfe)

data <- read_dta(file='CCFF_2023_M_1.dta')
data <- subset(data, select=c(y,x, bea_code,year, zd2))
data = data[complete.cases(data), ]

######################################################################################
# (6) Relation between productivity of top inventors and high-tech clusters
######################################################################################

################################################################################
# 6(a)
################################################################################
png('graphs/M_scatter.png')
plot(data$x, data$y, xlab="log Cluster Size", ylab="log Number of Patents per Inventor per Year", pch=19, col='blue')
dev.off()

################################################################################
# 6(b)
################################################################################
reg <- felm(y ~ as.factor(bea_code) + as.factor(year) + as.factor(zd2), data)
resid_y <- resid(reg) + mean(data$y)
reg <- felm(x ~ as.factor(bea_code) + as.factor(year) + as.factor(zd2), data)
resid_x <- resid(reg) + mean(data$x)

res <- binsreg(resid_y, resid_x, nbins=40,  polyreg=1)
dev.off()
tmp <- res$bins_plot + xlab("log Cluster Size") + ylab("log Number of Patents per Inventor per Year") + ylim(-.35, -.15) + xlim(-5.5, -2.5)
png('graphs/M_binscatter.png')
plot(tmp)
dev.off()

################################################################################
# 6(c)
################################################################################
resid_inc <-res$data.plot$`Group Full Sample`$data.dots

res <- binsreg(resid_y, resid_x, nbins=40,  line=c(0,0), linegrid=1000)
dev.off()
line <- res$data.plot$`Group Full Sample`$data.line
tmp <- res$bins_plot + geom_line(data=line[!is.na(line$fit),], aes(x=x,y=fit, color='orange')) + xlab("log Cluster Size") + ylab("log Number of Patents per Inventor per Year") + ylim(-.35, -.15) + xlim(-5.5, -2.5)
png('graphs/M_binscatter_pwc.png')
plot(tmp)
dev.off()

################################################################################
# 6(d)
################################################################################

#### Note that Figure 6(d) may differ from the manuscript because of the evaluation point of the control variables; see the discussion in the Supplemental Appendix

w <- model.matrix(~ as.factor(bea_code) + as.factor(year) + as.factor(zd2) - 1, data=data)

res <- binsreg(data$y, data$x, w, randcut=1, nbins=40)
tmp <- res$bins_plot + geom_point(data=resid_inc, aes(x=x, y=fit, color='orange'))+ xlab("log Cluster Size") + ylab("log Number of Patents per Inventor per Year") + ylim(-.4,0) + xlim(-8,-1)
dev.off()
png('graphs/M_covariateAdjustments_binscatter.png')
plot(tmp)
dev.off()


################################################################################
# 6(e)
################################################################################

#### Note that Figure 6(e) may differ from the manuscript because of the evaluation point of the control variables; see the discussion in the Supplemental Appendix

data <- read_dta(file='CCFF_2023_M_2.dta')
data <- subset(data, select=c(y, x, bea_code, year, zd2, cluster1))
data2 <- data[complete.cases(data), ]

w <- model.matrix(~ as.factor(bea_code) + as.factor(year) + as.factor(zd2) - 1, data2)

res <- binsreg(data2$y, data2$x, w, cluster=data2$cluster1, randcut=1, cb=T)
tmp <- res$bins_plot + xlab("log Cluster Size") + ylab("log Number of Patents per Inventor per Year") + ylim(-.5,.1) + xlim(-8,-1)
dev.off()
png('graphs/M_confidenceBand.png')
plot(tmp)
dev.off()

#### Note that Figure 6(f) is omitted because of the large sample size; please see the Stata code for replication


