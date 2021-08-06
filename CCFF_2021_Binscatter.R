################################################################################
## Replication file: "On Binscatter"
## Authors: M. D. Cattaneo, R. Crump, M. Farrell and Y. Feng
## Last update: 05-AUG-2021
################################################################################
rm(list=ls(all=TRUE))
library(binsreg); library(ggplot2); library(Hmisc)

################################################################################
## DGP and Setup
################################################################################

if(FALSE){
  set.seed(666)
  
  N <- 1000
  sig <- 0.5
  xmin <-0
  xmax <- 1
  mu.1 <- function(x) {(0.54-1.11*x+2.81*x^2-2.47*x^3+0.6*x^4)*40}
  mu.2 <- function(x) {(0.54-1.11*x+2.81*x^2-2.47*x^3+0.6*x^4)*40 - 25*(x-0.5)^2+2}
  
  ############################################################
  ## Generate data
  ############################################################
  
  ## Main dataset (t=1)
  t <- rep(1,N)
  x <- rbeta(N, 2, 4)
  w <- 3*(x-(xmax+xmin)/2) + runif(N, -0.5, 0.5)
  y.true <- mu.1(x)
  y <- y.true + w + rnorm(N, 0, sig)
  
  ## Used to illustrate covariate adjustment (w indep of x)
  w.x  <- runif(N, -1, 1)
  y.wx <- y.true + w.x + rnorm(N, 0, sig)
  
  ## Add conditional heteroskedasticity
  knots <- quantile(x, seq(0, 1, 1/10), names = F)
  bin.membership <- findInterval(x, knots, rightmost.closed = TRUE)
  y <- y + 10*((abs(bin.membership - 3.5) - 4.5)^2/50) * rnorm(N, 0, sig)
  y.wx <- y.wx + 10*((abs(bin.membership - 3.5) - 4.5)^2/50) * rnorm(N, 0, sig)
  
  ## Create dataframe
  data1 <- data.frame(t, y, x, w, y.true, y.wx, w.x)
  
  
  ## Second group dataset (t=2)
  t <- rep(2,N)
  x2 <- rbeta(N, 2, 4)
  w2 <- 3*(x2-(xmax+xmin)/2) + runif(N, -0.5, 0.5)
  y2.true <- mu.2(x2)
  y2 <- y2.true + w2 + rnorm(N, 0, sig)
  
  ## Add conditional heteroskedasticity
  knots <- quantile(x2, seq(0, 1, 1/10), names = F)
  bin.membership <- findInterval(x2, knots, rightmost.closed = TRUE)
  y2 <- y2 + 10*((abs(bin.membership - 3.5) - 4.5)^2/50) * rnorm(N, 0, sig)
  
  ## Create dataframe
  data2 <- data.frame(t, y=y2, x=x2, w=w2, y.true=y2.true, y.wx=NA, w.x=NA)
  
  ## Final dataset
  data <- rbind(data1,data2)
  write.csv(data, "CCFF_2021_Binscatter.csv", row.names = FALSE)
}

set.seed(1234)

## Read data
data <- read.csv("CCFF_2021_Binscatter.csv")

## Main dataset (t=1)
data1 <- data[data[,"t"]==1,];
x <- data1$x; w <- data1$w; y.true <- data1$y.true; y <- data1$y;
## Used to illustrate cov adj. (w indep of x)
w.x <- data1$w.x; y.wx <- data1$y.wx

## Second group dataset (t=2)
data2 <- data[data[,"t"]==2,];
x2 <- data2$x; w2 <- data2$w; y2.true <- data2$y.true; y2 <- data2$y;

## Specify knots
nbins <- 10
knots <- quantile(x, seq(0, 1, 1/nbins), names = F)

pdf.width <- 6
pdf.height <- 4.5

################################################################################
## Figure 1: Canonical Binscatter
################################################################################
fig <- ggplot() + theme_bw()
fig <- fig + geom_point(data=data1, aes(x=x, y=y), shape=16, col="lightgrey") +  
             geom_vline(xintercept=knots, colour="black", lty="dashed", lwd=0.2) +
             ylim(12,22) + 
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.text.x=element_blank(), axis.text.y=element_blank(),
                   axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                   legend.position="none") + labs(x="X", y="Y")

dots <- binsreg(y, x, nbins=10)$data.plot[[1]]$data.dots
fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19)
ggsave("figures/addDots.pdf", width=pdf.width, height=pdf.height)

## dots and lm fit
dotsAndlm <- binsreg(y, x, nbins=10, polyreg = 1)
dots <- dotsAndlm$data.plot[[1]]$data.dots; dots$Sname <- "binscatter"
lin  <- dotsAndlm$data.plot[[1]]$data.poly; lin$Sname <- "linear fit"
fig <- ggplot() +  theme_bw() + ylim(12,22) + 
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         axis.text.x=element_blank(), axis.text.y=element_blank(),
                         axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                         legend.position = c(0.9,0.95), 
                         legend.background = element_rect(fill = "transparent")) + 
                         labs(x="X", y="Y")
fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=19)
fig <- fig + geom_line(data=lin, aes(x=x, y=fit, colour=Sname), size=0.1)
fig <- fig + scale_color_manual(name="", values = c("blue", "black"),
                                guide=guide_legend(override.aes = list(
                                linetype=c(0,1), shape=c(19, NA))))
ggsave("figures/dotsAndLine.pdf", width=pdf.width, height=pdf.height)


################################################################################
## Figure 2: Extended Binscatter
################################################################################

## p=1, s=0
fig <- ggplot() + ylim(12,22) +
       geom_point(data=data1, aes(x=x, y=y), shape=16, col="lightgrey", size=1) + 
       theme_bw() +
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text.x=element_blank(), axis.text.y=element_blank(),
             axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
             legend.position = "none") + labs(x="X", y="Y")
dotsAndline <- binsreg(y, x, w, at=0, nbins = 10, dots=c(1,0), line = c(1,0), linegrid=300)
dots <- dotsAndline$data.plot[[1]]$data.dots
line <- dotsAndline$data.plot[[1]]$data.line
fig <- fig + geom_line(data=data1, aes(x=x, y=y.true), colour="darkgrey") +
             geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5)
ggsave("figures/p1-s0.pdf", width=pdf.width, height=pdf.height)

## p=s=2
fig <- ggplot() + ylim(12,22) +
       geom_point(data=data1, aes(x=x, y=y), shape=16, col="lightgrey", size=1) + 
       theme_bw() +
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text.x=element_blank(), axis.text.y=element_blank(),
             axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
             legend.position = "none") + labs(x="X", y="Y")
dotsAndline <- binsreg(y, x, w, at=0, nbins = 10, dots=c(2,2), line = c(2,2), linegrid=300)
dots <- dotsAndline$data.plot[[1]]$data.dots
line <- dotsAndline$data.plot[[1]]$data.line
fig <- fig + geom_line(data=data1, aes(x=x, y=y.true), colour="darkgrey") +
             geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5) 

ggsave("figures/p2-s2.pdf", width=pdf.width, height=pdf.height)


################################################################################
## Figure 3: Covariate Adjustment
################################################################################

## Case 1: x dependent on w
fig <- ggplot() +  
       theme_bw() + ylim(15,22) + 
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text.x=element_blank(), axis.text.y=element_blank(),
             axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
             legend.position = c(0.8,0.9), 
             legend.background = element_rect(fill = "transparent")) + 
       labs(x="X", y="Y")
data.more <- data1; data.more$Sname <- "true function"
fig <- fig + geom_line(data=data.more, aes(x=x, y=y.true, colour=Sname), size=0.5, linetype=1)

## Correct way
model <- binsreg(y, x, w=w, data=data.more, at=0, nbins=10, line=c(0,0))
dots  <- model$data.plot[[1]]$data.dots; dots$Sname <- "semi-linear canonical binscatter"; line <- model$data.plot[[1]]$data.line

fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=3, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5)

## Incorrect way
res.y <- lm(y~w)$residuals + mean(y)  # mimic the current binscatter command in STATA
res.x <- lm(x~w)$residuals + mean(x)

model <- binsreg(res.y, res.x, w=NULL, nbins=10, line=c(0,0))
dots  <- model$data.plot[[1]]$data.dots; dots$Sname <- "residualized canonical binscatter"; line <- model$data.plot[[1]]$data.line

fig   <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=3, shape=15) +
               geom_line(data=line, aes(x=x, y=fit), col="red", size=0.5, linetype=2)

fig <- fig + scale_color_manual(name="", values = c("red", "blue", "darkgrey"),
                                guide=guide_legend(override.aes = list(
                                      linetype=c(2,1,1), shape=c(15, 19, NA)))) +
       annotate("segment", x = 0.45, xend = max(res.x), y = 15, yend = 15, colour = "red",
                size=0.1, arrow=arrow(length=unit(0.25, "cm"))) +
       annotate("segment", x = 0.25, xend = min(res.x), y = 15, yend = 15, colour = "red",
                size=0.1, arrow=arrow(length=unit(0.25, "cm"))) +
       annotate("text", x = c(0.35), y = c(15), label = c("supp. narrowed"), 
                color="red", size=3.5) + 
       annotate("segment", x = min(res.x), xend = min(res.x), y = 14.8, yend = 15.2, colour = "red", size=0.1) +
       annotate("segment", x = max(res.x), xend = max(res.x), y = 14.8, yend = 15.2, colour = "red", size=0.1)
ggsave("figures/CovAdjust_xDepw.pdf", width=pdf.width, height=pdf.height)


## Case 2: x ind of w, w of mean = 0!
data.true1 <- cbind(x, data1$true, NA, NA); colnames(data.true1) <- c("X", "tau.cl","tau.bc", "se.rb")
true1 <- list(Estimate=data.true1, opt=list(s=1, s.i=1, deriv=0))

fig <- ggplot() +  theme_bw() + ylim(15,22) + 
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text.x=element_blank(), axis.text.y=element_blank(),
             axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
             legend.position = c(0.8,0.9), 
             legend.background = element_rect(fill = "transparent")) + 
       labs(x="X", y="Y")
data.more <- data1; data.more$Sname <- "true function."
fig <- fig + geom_line(data=data.more, aes(x=x, y=y.true, colour=Sname), size=0.5, linetype=1)

## Correct way
model <- binsreg(y.wx, x, w=w.x, data=data.more, at=0, nbins=10, line=c(0,0))
dots <- model$data.plot[[1]]$data.dots; dots$Sname <- "semi-linear canonical binscatter"; line <- model$data.plot[[1]]$data.line

fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=3, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5)

## Incorrect way
res.y <- lm(y.wx~w.x)$residuals + mean(y.wx)  # mimic the current binscatter command in STATA
res.x <- lm(x~w.x)$residuals + mean(x)

model <- binsreg(res.y, res.x, w=NULL, nbins=10, line=c(0,0))
dots  <- model$data.plot[[1]]$data.dots; dots$Sname <- "residualized canonical binscatter"; line <- model$data.plot[[1]]$data.line

fig   <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=3, shape=15) +
               geom_line(data=line, aes(x=x, y=fit), col="red", size=0.5, linetype=2)

fig <- fig + scale_color_manual(name="", values = c( "red", "blue", "darkgrey"),
                                guide=guide_legend(override.aes = list(
                                  linetype=c(2,1,1), shape=c(15, 19, NA))))
ggsave("figures/CovAdjust_xIndw.pdf", width=pdf.width, height=pdf.height)


################################################################################
## Figure 4: IMSE-optimal J
################################################################################

## small J
fig <- ggplot() + theme_bw() + ylim(12, 22) +
                  geom_point(data=data1, aes(x=x, y=y), shape=16, col="lightgrey", size=1) +
                  geom_line(data=data1, aes(x=x, y=y.true), colour="darkgrey") + 
                  theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        axis.text.x=element_blank(), axis.text.y=element_blank(),
                        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                        legend.position = "none", legend.background = element_rect(fill = "transparent")) +
                  labs(x="X", y="Y")
dotsAndline <- binsreg(y, x, w, at=0, nbins = 10, line=c(0,0))
dots <- dotsAndline$data.plot[[1]]$data.dots
line <- dotsAndline$data.plot[[1]]$data.line

fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5)
ggsave("figures/regressogram_Jsmall.pdf", width=pdf.width, height=pdf.height)

## IMSE optimal J
J.opt <- binsregselect(y, x, w)$nbinsdpi
fig <- ggplot() + theme_bw() + ylim(12, 22) +
                  geom_point(data=data1, aes(x=x, y=y), shape=16, col="lightgrey", size=1) +
                  geom_line(data=data1, aes(x=x, y=y.true), colour="darkgrey") + 
                  theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        axis.text.x=element_blank(), axis.text.y=element_blank(),
                        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                        legend.position = "none", legend.background = element_rect(fill = "transparent")) +
                  labs(x="X", y="Y")
dotsAndline <- binsreg(y, x, w, at=0, nbins = J.opt, line=c(0,0))
dots <- dotsAndline$data.plot[[1]]$data.dots
line <- dotsAndline$data.plot[[1]]$data.line

fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5)
ggsave("figures/regressogram_Jopt.pdf", width=pdf.width, height=pdf.height)


################################################################################
##  Figure 5: Uncertainty Quantification
################################################################################

fig <- ggplot() +  theme_bw() + ylim(12,23)
fig <- fig + geom_point(data=data1, aes(x=x, y=y), shape=16, col="lightgrey") + 
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.text.x=element_blank(), axis.text.y=element_blank(),
                   axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                   legend.position="none") + labs(x="X", y="Y")
model <- binsreg(y, x, w, at=0, line = c(0,0), ci=c(1,1), cb=c(1,1))
dots <- model$data.plot[[1]]$data.dots
line <- model$data.plot[[1]]$data.line
ci <- model$data.plot[[1]]$data.ci
cb <- model$data.plot[[1]]$data.cb
fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5) +
             geom_errorbar(data=ci, aes(x=x, ymin=ci.l, ymax=ci.r), col="blue", size=0.5, linetype=1) +
             geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), fill="blue", alpha=0.2)
ggsave("figures/inference_hsk1_p0s0.pdf", width=pdf.width, height=pdf.height)

fig <- ggplot() +  theme_bw() + ylim(12,23)
fig <- fig + geom_point(data=data1, aes(x=x, y=y), shape=16, col="lightgrey") +
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.text.x=element_blank(), axis.text.y=element_blank(),
                   axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                   legend.position="none") + labs(x="X", y="Y")
model <- binsreg(y, x, w, at=0, dots=c(2,2), line = c(2,2), ci=c(3,3), cb=c(3,3))
dots <- model$data.plot[[1]]$data.dots
line <- model$data.plot[[1]]$data.line
ci <- model$data.plot[[1]]$data.ci
cb <- model$data.plot[[1]]$data.cb
fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5) +
             geom_errorbar(data=ci, aes(x=x, ymin=ci.l, ymax=ci.r), col="blue", size=0.5, linetype=1, width=0.02) +
             geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), fill="blue", alpha=0.2)
ggsave("figures/inference_hsk1_p2s2.pdf", width=pdf.width, height=pdf.height)


################################################################################
## Figure 6: Hypothesis Testing -- Parametric Specification
################################################################################

## p=s=0
est.bin <- binsreg(y, x, w, at=0, dots=c(0,0), line = c(0,0), cb=c(1,1), polyreg = 1)
dots    <- est.bin$data.plot[[1]]$data.dots; dots$Sname <- "binscatter"
line    <- est.bin$data.plot[[1]]$data.line; line$Sname <- "binscatter"
cb      <- est.bin$data.plot[[1]]$data.cb
lfit    <- est.bin$data.plot[[1]]$data.poly; lfit$Sname <- "linear"
est.bin <- binsreg(y, x, w, at=0, polyreg = 2)
qfit    <- est.bin$data.plot[[1]]$data.poly; qfit$Sname <- "quadratic"
est.bin <- binsreg(y, x, w, at=0, polyreg = 0)
const   <- est.bin$data.plot[[1]]$data.poly; const$Sname <- "constant"

fig <- ggplot() + geom_point(data=data1, aes(x=x, y=y), shape=16, col="lightgrey", size=1) +
       theme_bw() + ylim(12,23) + 
       theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             axis.text.x=element_blank(), axis.text.y=element_blank(),
             axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
             legend.position = c(0.9,0.9), 
             legend.background = element_rect(fill = "transparent")) + 
       labs(x="X", y="Y")
fig <- fig + geom_line(data=const, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=2)
fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2)
fig <- fig + geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.1)
fig <- fig + geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2, fill="blue")
fig <- fig + geom_line(data=lfit, aes(x=x, y=fit, colour=Sname), size=0.1)
fig <- fig + geom_line(data=qfit, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=4)

fig <- fig + scale_color_manual(name="", values = c("blue", "grey", "black", "black"),
                                guide=guide_legend(override.aes = list(
                                  linetype=c(1,2,1,4), shape=c(19, NA, NA, NA))))

ggsave("figures/testspec_p0s0.pdf", width=pdf.width, height=pdf.height)


## p=s=2
est.bin <- binsreg(y, x, w, at=0, dots=c(2,2), line = c(2,2), cb=c(3,3), polyreg = 1)
dots <- est.bin$data.plot[[1]]$data.dots; dots$Sname <- "binscatter"
line <- est.bin$data.plot[[1]]$data.line; line$Sname <- "binscatter"
cb <- est.bin$data.plot[[1]]$data.cb
lfit <- est.bin$data.plot[[1]]$data.poly; lfit$Sname <- "linear"
est.bin <- binsreg(y, x, w, at=0, polyreg = 2)
qfit <- est.bin$data.plot[[1]]$data.poly; qfit$Sname <- "quadratic"
est.bin <- binsreg(y, x, w, at=0, polyreg = 0)
const <- est.bin$data.plot[[1]]$data.poly; const$Sname <- "constant"

fig <- ggplot() + geom_point(data=data1, aes(x=x, y=y), shape=16, col="lightgrey", size=1) +
                  theme_bw() + ylim(12,23) + 
                  theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        axis.text.x=element_blank(), axis.text.y=element_blank(),
                        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                        legend.position = c(0.9,0.9), 
                        legend.background = element_rect(fill = "transparent")) + 
                  labs(x="X", y="Y")
fig <- fig + geom_line(data=const, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=2)
fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2)
fig <- fig + geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.1)
fig <- fig + geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2, fill="blue")
fig <- fig + geom_line(data=lfit, aes(x=x, y=fit, colour=Sname), size=0.1)
fig <- fig + geom_line(data=qfit, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=4)

fig <- fig + scale_color_manual(name="", values = c("blue", "grey", "black", "black"),
                                guide=guide_legend(override.aes = list(
                                  linetype=c(1,2,1,4), shape=c(19, NA, NA, NA))))

ggsave("figures/testspec_p2s2.pdf", width=pdf.width, height=pdf.height)


################################################################################
## Table 1: Hypothesis Testing -- Parametric Specification
################################################################################
result.test <- matrix(NA, 6, 6)

## p=s=0
## constant?
est <- binstest(y, x, w=w, at=0, bins = c(0,0), testmodel=c(1,1), testmodelpoly=0)
result.test[1,1:3] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)
est <- binstest(y, x, w=w, at=0, bins = c(0,0), testmodel=c(1,1), testmodelpoly=0, lp=2)
result.test[1,4:6] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)

## linear?
est <- binstest(y, x, w=w, at=0, bins = c(0,0), testmodel=c(1,1), testmodelpoly=1)
result.test[2,1:3] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)
est <- binstest(y, x, w=w, at=0, bins = c(0,0), testmodel=c(1,1), testmodelpoly=1, lp=2)
result.test[2,4:6] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)

## Quadratic?
est <- binstest(y, x, w=w, at=0, bins = c(0,0), testmodel=c(1,1), testmodelpoly=2)
result.test[3,1:3] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)
est <- binstest(y, x, w=w, at=0, bins = c(0,0), testmodel=c(1,1), testmodelpoly=2, lp=2)
result.test[3,4:6] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)

## p=s=2
## constant
est <- binstest(y, x, w=w, at=0, bins = c(2,2), testmodelpoly=0)
result.test[4,1:3] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)
est <- binstest(y, x, w=w, at=0, bins = c(2,2), testmodelpoly=0, lp=2)
result.test[4,4:6] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)

## linear?
est <- binstest(y, x, w=w, at=0, bins = c(2,2), testmodelpoly=1)
result.test[5,1:3] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)
est <- binstest(y, x, w=w, at=0, bins = c(2,2), testmodelpoly=1, lp=2)
result.test[5,4:6] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)

## Quadratic?
est <- binstest(y, x, w=w, at=0, bins = c(2,2), testmodelpoly=2)
result.test[6,1:3] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)
est <- binstest(y, x, w=w, at=0, bins = c(2,2), testmodelpoly=2, lp=2)
result.test[6,4:6] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)

spec.test <- cbind(format(round(result.test[,c(1:2,4:5)], 2), nsmall=2), result.test[,6])

## make table
n.cgroup <- c(2,2,1)
cgroup   <- c("Sup norm", "$L_2$ norm", "")
colheads <- c(rep(c("Test Statistic", "P-value"), 2), "$\\hat{J}_{\\texttt{IMSE}}$")
rowname <- c("Constant", "Linear", "Quadratic") 
#             "Negativity", "Decreasingness", "Concavity")
n.rgroup <- c(3,3)
rgroup <- c("Canonical: $p=s=0$", "Extended: $p=s=2$")

latex(spec.test, file=paste("figures/testing-spec", ".txt", sep = ""), 
      append=FALSE, table.env=FALSE, center="none", title="",
      n.cgroup=n.cgroup, cgroup=cgroup, colheads=colheads,
      n.rgroup=n.rgroup, rgroup=rgroup, rowname=rowname, rowlabel="Binscatter")


################################################################################
## Table 2: Hypothesis Testing -- Shape Restriction
################################################################################
result.test <- matrix(NA, 6, 3)

## p=s=0
## negativity: <=0?
est <- binstest(y, x, w=w, at=0, deriv=0, testshapel=0, bins=c(0,0), testshape=c(1,1))
result.test[1, 1:3] <- c(est$testshapeL$stat.shapeL, est$testshapeL$pval, est$opt$nbins)

## p=s=1
## negativity: <=0 ?
est <- binstest(y, x, w=w, at=0, deriv=0, testshapel=0, bins=c(1,1), testshape=c(2,2))
result.test[2, 1:3] <- c(est$testshapeL$stat.shapeL, est$testshapeL$pval, est$opt$nbins)
## Decreasing: first-order deriv <= 0 ?
est <- binstest(y, x, w=w, at=0, deriv=1, testshapel=0, bins=c(1,1), testshape=c(2,2))
result.test[3, 1:3] <- c(est$testshapeL$stat.shapeL, est$testshapeL$pval, est$opt$nbins)

## p=s=2
## negativity: <=0?
est <- binstest(y, x, w=w, at=0, deriv=0, testshapel=0, bins=c(2,2), testshape=c(3,3))
result.test[4, 1:3] <- c(est$testshapeL$stat.shapeL, est$testshapeL$pval, est$opt$nbins)
## Decreasing: first-order deriv <= 0 ?
est <- binstest(y, x, w=w, at=0, deriv=1, testshapel=0, bins=c(2,2), testshape=c(3,3))
result.test[5, 1:3] <- c(est$testshapeL$stat.shapeL, est$testshapeL$pval, est$opt$nbins)
## Concavity: second-order deriv <= 0 ?
est <- binstest(y, x, w=w, at=0, deriv=2, testshapel=0, bins=c(2,2), testshape=c(3,3))
result.test[6, 1:3] <- c(est$testshapeL$stat.shapeL, est$testshapeL$pval, est$opt$nbins)

shape.test <- cbind(format(round(result.test[,1:2], 2), nsmall=2), result.test[,3])

n.cgroup <- c(3)
cgroup <- NULL
colheads <- rep(c("Test Statistic", "P-value", "$\\hat{J}_{\\texttt{IMSE}}$"), 1)
rowname <- c("Negativity", c("Negativity", "Decreasingness"), c("Negativity", "Decreasingness", "Concavity"))
n.rgroup <- c(1,2,3)
rgroup <- c("Canonical: $p=s=0$", "Extended: $p=s=1$", "Extended: $p=s=2$")

latex(shape.test, file=paste("figures/testing-shape", ".txt", sep = ""), 
      append=FALSE, table.env=FALSE, center="none", title="",
      n.cgroup=n.cgroup, cgroup=cgroup, colheads=colheads,
      n.rgroup=n.rgroup, rgroup=rgroup, rowname=rowname, rowlabel="\\bf{Binscatter}")


################################################################################
## Figure 7: Two-group Comparison
################################################################################

## Graphical illustration
fig <-  ggplot() +  theme_bw() + ylim(13,22) + 
        geom_vline(xintercept=0.22, colour="black", lty="dashed", lwd=0.2) +
        geom_vline(xintercept=0.25, colour="black", lty="dashed", lwd=0.2) + 
        geom_rect(aes(xmin=0.22, xmax=0.25, ymin=-Inf, ymax=Inf), linetype="dashed", fill="lightgrey", alpha=0.3) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
              legend.position = c(0.9,0.95), 
              legend.background = element_rect(fill = "transparent")) + 
        labs(x="X", y="Y")
model <- binsreg(y, x, w, at=0, line=c(0,0), cb=c(1,1))
dots <- model$data.plot[[1]]$data.dots; dots$Sname <- "Group 0"
line <- model$data.plot[[1]]$data.line; line$Sname <- "Group 0"
cb <- model$data.plot[[1]]$data.cb

fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=19)
fig <- fig + geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=1)
fig <- fig + geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2, fill="blue")

model <- binsreg(y2, x2, w2, at=0, line=c(0,0), cb=c(1,1))
dots <- model$data.plot[[1]]$data.dots; dots$Sname <- "Group 1"
line <- model$data.plot[[1]]$data.line; line$Sname <- "Group 1"
cb <- model$data.plot[[1]]$data.cb

fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=15)
fig <- fig + geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=2)
fig <- fig + geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2, fill="green4")

fig <- fig + scale_color_manual(name="", values = c("blue", "green4"),
                                guide=guide_legend(override.aes = list(
                                  linetype=c(1,2), shape=c(19, 15))))
ggsave("figures/groupDiff_p0s0.pdf", width=pdf.width, height=pdf.height)


fig <-  ggplot() +  theme_bw() + ylim(13,22) + 
        geom_vline(xintercept=0.22, colour="black", lty="dashed", lwd=0.2) +
        geom_vline(xintercept=0.25, colour="black", lty="dashed", lwd=0.2) + 
        geom_rect(aes(xmin=0.22, xmax=0.25, ymin=-Inf, ymax=Inf), linetype="dashed", fill="lightgrey", alpha=0.3) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
              legend.position = c(0.9,0.95), 
              legend.background = element_rect(fill = "transparent")) + 
        labs(x="X", y="Y")
model <- binsreg(y, x, w, at=0, dots=c(2,2), line=c(2,2), cb=c(3,3))
dots  <- model$data.plot[[1]]$data.dots; dots$Sname <- "Group 0"
line  <- model$data.plot[[1]]$data.line; line$Sname <- "Group 0"
cb    <- model$data.plot[[1]]$data.cb

fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=19)
fig <- fig + geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=1)
fig <- fig + geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2, fill="blue")

model <- binsreg(y2, x2, w2, at=0, dots=c(2,2), line=c(2,2), cb=c(3,3))
dots <- model$data.plot[[1]]$data.dots; dots$Sname <- "Group 1"
line <- model$data.plot[[1]]$data.line; line$Sname <- "Group 1"
cb <- model$data.plot[[1]]$data.cb

fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=15)
fig <- fig + geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=2)
fig <- fig + geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2, fill="green4")

fig <- fig + scale_color_manual(name="", values = c("blue", "green4"),
                                guide=guide_legend(override.aes = list(
                                  linetype=c(1,2), shape=c(19, 15))))
ggsave("figures/groupDiff_p2s2.pdf", width=pdf.width, height=pdf.height)


################################################################################
## Table 3: Two-group Comparison
################################################################################

pwc.test <- matrix(NA, 4, 4)
data$subset <- (data$x >= 0.22 & data$x <= 0.25) 

## p=s=0
est <- binspwc(y, x, w=w, at=0, by=t, data=data, bins=c(0,0), pwc=c(1,1), subset=subset)
pwc.test[1, 1:4] <- c(est$tstat[1], est$pval, est$opt$nbins.by)
est <- binspwc(y, x, w=w, at=0, by=t, data=data, bins=c(0,0), pwc=c(1,1))
pwc.test[2, 1:4] <- c(est$tstat[1], est$pval, est$opt$nbins.by)

## p=s=2
est <- binspwc(y, x, w=w, at=0, by=t, data=data, bins=c(2,2), pwc=c(3,3), subset=subset)
pwc.test[3, 1:4] <- c(est$tstat[1], est$pval, est$opt$nbins.by)
est <- binspwc(y, x, w=w, at=0, by=t, data=data, bins=c(2,2), pwc=c(3,3))
pwc.test[4, 1:4] <- c(est$tstat[1], est$pval, est$opt$nbins.by)

pwc.test <- cbind(format(round(pwc.test[,1:2], 2), nsmall=2), pwc.test[,3:4])

## make table
n.cgroup <- c(4)
cgroup <- NULL
colheads <- rep(c("Test Statistic", "P-value", "$\\hat{J}_{\\texttt{IMSE},0}$", "$\\hat{J}_{\\texttt{IMSE},1}$"), 1)
rowname  <- rep(c("$0.22\\leq x\\leq 0.25$", "full sample"),2)
n.rgroup <- c(2,2)
rgroup   <- c("Canonical: $p=s=0$", "Extended: $p=s=2$")

latex(pwc.test, file=paste("figures/testing-2sample", ".txt", sep = ""), 
      append=FALSE, table.env=FALSE, center="none", title="",
      n.cgroup=n.cgroup, cgroup=cgroup, colheads=colheads,
      n.rgroup=n.rgroup, rgroup=rgroup, rowname=rowname, rowlabel="\\bf{Binscatter}")



################################################################################
## Figure 8: Generalized Non-linear Binscatter
################################################################################

############################################################
## Row 1: Quantile Regression
############################################################

## p=s=0
fig <- ggplot() + theme_bw() + ylim(12,23)
fig <- fig + geom_point(data=data1, aes(x=x, y=y), shape=16, col="lightgrey") + 
             geom_vline(xintercept=0.12, colour="black", lty="dashed", lwd=0.2) +
             geom_vline(xintercept=0.5, colour="black", lty="dashed", lwd=0.2) + 
             geom_rect(aes(xmin=0.12, xmax=0.5, ymin=-Inf, ymax=Inf), linetype="dashed", fill="lightgrey", alpha=0.3) +
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.text.x=element_blank(), axis.text.y=element_blank(),
                   axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                   legend.position=c(0.85,0.95),
                   legend.background = element_rect(fill = "transparent")) + labs(x="X", y="Y")
model <- binsqreg(y, x, w, at=0, quantile=0.25, line = c(0,0), ci=c(1,1), cb=c(1,1))
dots <- model$data.plot[[1]]$data.dots; dots$Sname <- "0.25 quantile"
line <- model$data.plot[[1]]$data.line; line$Sname <- "0.25 quantile"
ci <- model$data.plot[[1]]$data.ci
cb <- model$data.plot[[1]]$data.cb
fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.5, linetype=1) +
             geom_errorbar(data=ci, aes(x=x, ymin=ci.l, ymax=ci.r), col="blue", size=0.5, linetype=1) +
             geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), fill="blue", alpha=0.2)

model <- binsqreg(y, x, w, at=0, quantile=0.75, line = c(0,0), ci=c(1,1), cb=c(1,1))
dots <- model$data.plot[[1]]$data.dots; dots$Sname <- "0.75 quantile"
line <- model$data.plot[[1]]$data.line; line$Sname <- "0.75 quantile"
ci <- model$data.plot[[1]]$data.ci
cb <- model$data.plot[[1]]$data.cb
fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=15) +
             geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.5, linetype=2) +
             geom_errorbar(data=ci, aes(x=x, ymin=ci.l, ymax=ci.r), col="green4", size=0.5, linetype=2) +
             geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), fill="green4", alpha=0.2)

fig <- fig + scale_color_manual(name="", values = c("blue", "green4"),
                                guide=guide_legend(override.aes = list(
                                  linetype=c(1,2), shape=c(19, 15))))

ggsave("figures/QR_p0s0.pdf", width=pdf.width, height=pdf.height)

## p=s=2
fig <- ggplot() + theme_bw() + ylim(12,23)
fig <- fig + geom_point(data=data1, aes(x=x, y=y), shape=16, col="lightgrey") + 
             geom_vline(xintercept=0.12, colour="black", lty="dashed", lwd=0.2) +
             geom_vline(xintercept=0.5, colour="black", lty="dashed", lwd=0.2) + 
             geom_rect(aes(xmin=0.12, xmax=0.5, ymin=-Inf, ymax=Inf), linetype="dashed", fill="lightgrey", alpha=0.3) +
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.text.x=element_blank(), axis.text.y=element_blank(),
                   axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                   legend.position=c(0.85,0.95),
                   legend.background = element_rect(fill = "transparent")) + labs(x="X", y="Y")
model <- binsqreg(y, x, w, at=0, quantile=0.25, dots=c(2,2), line = c(2,2), ci=c(3,3), cb=c(3,3))
dots <- model$data.plot[[1]]$data.dots; dots$Sname <- "0.25 quantile"
line <- model$data.plot[[1]]$data.line; line$Sname <- "0.25 quantile"
ci <- model$data.plot[[1]]$data.ci
cb <- model$data.plot[[1]]$data.cb
fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.5, linetype=1) +
             geom_errorbar(data=ci, aes(x=x, ymin=ci.l, ymax=ci.r), col="blue", size=0.5, linetype=1, width=0.02) +
             geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), fill="blue", alpha=0.2)

model <- binsqreg(y, x, w, at=0, quantile=0.75, dots=c(2,2), line = c(2,2), ci=c(3,3), cb=c(3,3))
dots <- model$data.plot[[1]]$data.dots; dots$Sname <- "0.75 quantile"
line <- model$data.plot[[1]]$data.line; line$Sname <- "0.75 quantile"
ci <- model$data.plot[[1]]$data.ci
cb <- model$data.plot[[1]]$data.cb
fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=15) +
             geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.5, linetype=2) +
             geom_errorbar(data=ci, aes(x=x, ymin=ci.l, ymax=ci.r), col="green4", size=0.5, linetype=2, width=0.02) +
             geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), fill="green4", alpha=0.2)

fig <- fig + scale_color_manual(name="", values = c("blue", "green4"),
                                guide=guide_legend(override.aes = list(
                                  linetype=c(1,2), shape=c(19, 15))))

ggsave("figures/QR_p2s2.pdf", width=pdf.width, height=pdf.height)


############################################################
## Row 2: Logistic Regression
############################################################

## Define a binary response
d <- 1*(y<=quantile(y, probs=1/3))
subset <- (x>=0.12 & x<=0.5)

## p=s=0
fig <- ggplot() +  theme_bw()
fig <- fig  +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
        legend.position=c(0.85,0.95),
        legend.background = element_rect(fill = "transparent")) + labs(x="X", y="Y")
model <- binsglm(d, x, w, at=0, family=binomial(), line = c(0,0), ci=c(1,1), cb=c(1,1), subset=subset)
dots  <- model$data.plot[[1]]$data.dots
line  <- model$data.plot[[1]]$data.line
ci    <- model$data.plot[[1]]$data.ci
cb    <- model$data.plot[[1]]$data.cb
fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5, linetype=1) +
             geom_errorbar(data=ci, aes(x=x, ymin=ci.l, ymax=ci.r), col="blue", size=0.5, linetype=1, width=0.01) +
             geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), fill="blue", alpha=0.2)

ggsave("figures/Logit_p0s0.pdf", width=pdf.width, height=pdf.height)

## p=s=2
fig <- ggplot() +  theme_bw()
fig <- fig  +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
        legend.position=c(0.85,0.95),
        legend.background = element_rect(fill = "transparent")) + labs(x="X", y="Y")
model <- binsglm(d, x, w, at=0, family=binomial(), dots=c(2,2), line = c(2,2), ci=c(3,3), cb=c(3,3), subset=subset)
dots  <- model$data.plot[[1]]$data.dots
line  <- model$data.plot[[1]]$data.line
ci    <- model$data.plot[[1]]$data.ci
cb    <- model$data.plot[[1]]$data.cb
fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5, linetype=1) +
             geom_errorbar(data=ci, aes(x=x, ymin=ci.l, ymax=ci.r), col="blue", size=0.5, linetype=1, width=0.01) +
             geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), fill="blue", alpha=0.2)

ggsave("figures/Logit_p2s2.pdf", width=pdf.width, height=pdf.height)



############################################################
## Row 3: Quantile Regression -- Two-group comparison
############################################################

fig <-  ggplot() +  theme_bw() + ylim(12,23) + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
              legend.position = c(0.9,0.95), 
              legend.background = element_rect(fill = "transparent")) + 
        labs(x="X", y="Y")
model <- binsqreg(y, x, w, at=0, quantile=0.75, line=c(0,0), cb=c(1,1))
dots <- model$data.plot[[1]]$data.dots; dots$Sname <- "Group 0"
line <- model$data.plot[[1]]$data.line; line$Sname <- "Group 0"
cb <- model$data.plot[[1]]$data.cb

fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=19)
fig <- fig + geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=1)
fig <- fig + geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2, fill="blue")

model <- binsqreg(y2, x2, w2, at=0, quantile=0.75, line=c(0,0), cb=c(1,1))
dots <- model$data.plot[[1]]$data.dots; dots$Sname <- "Group 1"
line <- model$data.plot[[1]]$data.line; line$Sname <- "Group 1"
cb <- model$data.plot[[1]]$data.cb

fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=15)
fig <- fig + geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=2)
fig <- fig + geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2, fill="green4")

fig <- fig + scale_color_manual(name="", values = c("blue", "green4"),
                                guide=guide_legend(override.aes = list(
                                  linetype=c(1,2), shape=c(19, 15))))
ggsave("figures/QR_groupDiff_p0s0.pdf", width=pdf.width, height=pdf.height)


fig <-  ggplot() +  theme_bw() + ylim(12,23) + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x=element_blank(), axis.text.y=element_blank(),
              axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
              legend.position = c(0.9,0.95), 
              legend.background = element_rect(fill = "transparent")) + 
        labs(x="X", y="Y")
model <- binsqreg(y, x, w, at=0, quantile=0.75, dots=c(2,2), line=c(2,2), cb=c(3,3))
dots  <- model$data.plot[[1]]$data.dots; dots$Sname <- "Group 0"
line  <- model$data.plot[[1]]$data.line; line$Sname <- "Group 0"
cb    <- model$data.plot[[1]]$data.cb

fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=19)
fig <- fig + geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=1)
fig <- fig + geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2, fill="blue")

model <- binsqreg(y2, x2, w2, at=0, quantile=0.75, dots=c(2,2), line=c(2,2), cb=c(3,3))
dots  <- model$data.plot[[1]]$data.dots; dots$Sname <- "Group 1"
line  <- model$data.plot[[1]]$data.line; line$Sname <- "Group 1"
cb    <- model$data.plot[[1]]$data.cb

fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=15)
fig <- fig + geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=2)
fig <- fig + geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2, fill="green4")

fig <- fig + scale_color_manual(name="", values = c("blue", "green4"),
                                guide=guide_legend(override.aes = list(
                                  linetype=c(1,2), shape=c(19, 15))))
ggsave("figures/QR_groupDiff_p2s2.pdf", width=pdf.width, height=pdf.height)


