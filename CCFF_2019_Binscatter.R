################################################################################
# Binscatter: illustration file
# Authors: M. D. Cattaneo, R. Crump, M. Farrell and Y. Feng
# Last update: 20-MAR-2019
################################################################################
rm(list=ls(all=TRUE))
library(binsreg); library(ggplot2); library(Hmisc)


#############################################################
############# Section 2 #####################################

################################################
##            DGP and Setup                   ##
################################################

dgp <- function(N, sig, fun, xmin=0, xmax=1, control=F, corr=F) {
  x <- rbeta(N, 2, 4)
  y <- fun(x)  + rnorm(N, 0, sig)
  w <- NA
  if (control) {
    if (corr) {
      w <- 3*(x-(xmax+xmin)/2) + runif(N, -0.5, 0.5)
      y <- y + w
    } else {
      w <- runif(N, -1, 1); y <- y + w
    }
  }
  true <- fun(x)
  data <- data.frame(y=y, x=x, w=w, true=true)
  return(data)
}

# par
N <- 1000
sig <- 0.5
# mu <- function(x) {sin(12*(x+0.5))/(x+0.5)}
mu  <- function(x) {(0.54-1.11*x+2.81*x^2-2.47*x^3+0.6*x^4)*40}
mu1 <- function(x) {(0.54-1.11*x+2.81*x^2-2.47*x^3+0.6*x^4)*40 + 1}
pdf.width <- 6
pdf.height <- 4.5

##############################################
## generate data

set.seed(1234)
data <- dgp(N=N, sig=sig, fun=mu, control = T)
y <- data$y; x <- data$x; w <- data$w

## Specify knots
nbins <- 10
knots <- quantile(x, seq(0, 1, 1/nbins), names = F)

######################################
##     binscatter: construction     ##
######################################
fig <- ggplot() +  theme_bw()
fig <- fig + geom_point(data=data, aes(x=x, y=y), shape=16, col="lightgrey") +  
             geom_vline(xintercept=knots, colour="black", lty="dashed", lwd=0.2) +
             ylim(14,22) + 
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.text.x=element_blank(), axis.text.y=element_blank(),
                   axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                   legend.position="none") + labs(x="X", y="Y")

dots <- binsreg(y, x, nbins=10)$data.plot[[1]]$data.dots
fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19)
ggsave("figures/addDots.pdf", width=pdf.width, height=pdf.height)

# dots and lm fit
dotsAndlm <- binsreg(y, x, nbins=10, polyreg = 1)
dots <- dotsAndlm$data.plot[[1]]$data.dots; dots$Sname <- "binscatter"
lin  <- dotsAndlm$data.plot[[1]]$data.poly; lin$Sname <- "linear fit"
fig <- ggplot() +  theme_bw() + ylim(14,22) + 
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



###############################################
##    Change variance but keep local means   ##
###############################################

# add variance
fig <- ggplot() +  theme_bw()
fig <- fig + geom_point(data=data, aes(x=x, y=y), shape=16, col="lightgrey") +  
             ylim(14,22) + 
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.text.x=element_blank(), axis.text.y=element_blank(),
                   axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                   legend.position="none") + labs(x="X", y="Y")

dots <- binsreg(y, x, nbins=10)$data.plot[[1]]$data.dots
fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19)
ggsave("figures/addingVariance1.pdf", width=pdf.width, height=pdf.height)

#######################
bin.membership <- findInterval(x, knots, rightmost.closed = TRUE)
bin.means      <- tapply(y, bin.membership, mean)
new.y          <- y + 1 * (y - bin.means[bin.membership])

fig <- ggplot() +  theme_bw()
fig <- fig + geom_point(data=data.frame(x=x,y=new.y), aes(x=x, y=y), shape=16, col="lightgrey") +  
             ylim(14,22) + 
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.text.x=element_blank(), axis.text.y=element_blank(),
                   axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                   legend.position="none") + labs(x="X", y="Y")

dots <- binsreg(new.y, x, nbins=10)$data.plot[[1]]$data.dots
fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19)
ggsave("figures/addingVariance2.pdf", width=pdf.width, height=pdf.height)


# Heteroskedasticity
# # Case 1: larger at tails, low HSK
new.y <- y  +   1.5*((bin.membership - 2.5)^2/50)*(y - bin.means[bin.membership])

fig <- ggplot() +  theme_bw()
fig <- fig + geom_point(data=data.frame(x=x,y=new.y), aes(x=x, y=y), shape=16, col="lightgrey") +  
             ylim(13,25)+theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              axis.text.x=element_blank(), axis.text.y=element_blank(),
                              axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                              legend.position="none") + labs(x="X", y="Y")
dots <- binsreg(new.y, x, nbins=10)$data.plot[[1]]$data.dots
fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19)
ggsave("figures/heteroskedasticity1.pdf", width=pdf.width, height=pdf.height)

# Case 2: large in the middle, high HSK
new.y <- y  +   10*((abs(bin.membership - 3.5) - 4.5)^2/50)*(y - bin.means[bin.membership])
fig <- ggplot() +  theme_bw()
fig <- fig + geom_point(data=data.frame(x=x,y=new.y), aes(x=x, y=y), shape=16, col="lightgrey") +  
             ylim(13,25)+theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              axis.text.x=element_blank(), axis.text.y=element_blank(),
                              axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                              legend.position="none") + labs(x="X", y="Y")
dots <- binsreg(new.y, x, nbins=10)$data.plot[[1]]$data.dots
fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19)
ggsave("figures/heteroskedasticity2.pdf", width=pdf.width, height=pdf.height)


##############################
##      regressogram        ##
##############################
fig <- ggplot() +  theme_bw()
fig <- fig + geom_point(data=data, aes(x=x, y=y), shape=16, col="lightgrey") +  ylim(14,22) + 
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.text.x=element_blank(), axis.text.y=element_blank(),
                   axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                   legend.position="none") + labs(x="X", y="Y")
binsAndline <- binsreg(y, x, nbins = 10, line = c(0,0))
dots <- binsAndline$data.plot[[1]]$data.dots
line <- binsAndline$data.plot[[1]]$data.line
fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5)
ggsave("figures/regressogram_and_dots.pdf", width=pdf.width, height=pdf.height)


#########################################
##     Covariate Adjustment            ##
##     inside or outside?              ##  
#########################################

# DGP with a control variable added

# Case 1: x ind of w, w of mean = 0!
fig <- ggplot() +  theme_bw() + ylim(15,22) + 
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         axis.text.x=element_blank(), axis.text.y=element_blank(),
                         axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                         legend.position = c(0.9,0.9), 
                         legend.background = element_rect(fill = "transparent")) + 
                   labs(x="X", y="Y")
data.more <- data; data.more$Sname <- "true fn."
fig <- fig + geom_line(data=data.more, aes(x=x, y=true, colour=Sname), size=0.5, linetype=1)

# Correct way
model <- binsreg(y, x, w=w, nbins=10, line=c(0,0))
dots <- model$data.plot[[1]]$data.dots; dots$Sname <- "cov. adj."; line <- model$data.plot[[1]]$data.line

fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=3, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5)

# Incorrect way
res.y <- lm(y~w)$residuals + mean(y)  # mimic the current binscatter command in STATA
res.x <- lm(x~w)$residuals + mean(x)

model <- binsreg(res.y, res.x, w=w, nbins=10, line=c(0,0))
dots  <- model$data.plot[[1]]$data.dots; dots$Sname <- "resid."; line <- model$data.plot[[1]]$data.line

fig   <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=3, shape=15) +
               geom_line(data=line, aes(x=x, y=fit), col="red", size=0.5, linetype=2)


fig <- fig + scale_color_manual(name="", values = c("blue", "red", "darkgrey"),
                                guide=guide_legend(override.aes = list(
                                linetype=c(1,2,1), shape=c(19, 15, NA))))
ggsave("figures/CovAdjust_xIndw.pdf", width=pdf.width, height=pdf.height)


# Case 2: x dep on w, w of mean = 0!
set.seed(1234)
data1 <- dgp(N=N, sig=sig, fun=mu, control = T, corr = T)
y1 <- data1$y; x1 <- data1$x; w1 <- data1$w
data.true1 <- cbind(x1, data1$true, NA, NA); colnames(data.true1) <- c("X", "tau.cl","tau.bc", "se.rb")
true1 <- list(Estimate=data.true1, opt=list(s=1, s.i=1, deriv=0))

# check
# cor(x1, w1)
# Case 1: x ind of w, w of mean = 0!
fig <- ggplot() +  theme_bw() + ylim(15,22) + 
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         axis.text.x=element_blank(), axis.text.y=element_blank(),
                         axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                         legend.position = c(0.9,0.9), 
                         legend.background = element_rect(fill = "transparent")) + 
                   labs(x="X", y="Y")
data.more <- data1; data.more$Sname <- "true fn."
fig <- fig + geom_line(data=data.more, aes(x=x, y=true, colour=Sname), size=0.5, linetype=1)

# Correct way
model <- binsreg(y1, x1, w=w1, nbins=10, line=c(0,0))
dots <- model$data.plot[[1]]$data.dots; dots$Sname <- "cov. adj."; line <- model$data.plot[[1]]$data.line

fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=3, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5)

# Incorrect way
res.y <- lm(y1~w1)$residuals + mean(y1)  # mimic the current binscatter command in STATA
res.x <- lm(x1~w1)$residuals + mean(x1)

model <- binsreg(res.y, res.x, w=NULL, nbins=10, line=c(0,0))
dots  <- model$data.plot[[1]]$data.dots; dots$Sname <- "resid."; line <- model$data.plot[[1]]$data.line

fig   <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=3, shape=15) +
               geom_line(data=line, aes(x=x, y=fit), col="red", size=0.5, linetype=2)


fig <- fig + scale_color_manual(name="", values = c("blue", "red", "darkgrey"),
                                guide=guide_legend(override.aes = list(
                                linetype=c(1,2,1), shape=c(19, 15, NA)))) +
  annotate("segment", x = 0.45, xend = max(res.x), y = 15, yend = 15, colour = "red",
           size=0.1, arrow=arrow(length=unit(0.25, "cm")))+
  annotate("segment", x = 0.25, xend = min(res.x), y = 15, yend = 15, colour = "red",
           size=0.1, arrow=arrow(length=unit(0.25, "cm")))+
  annotate("text", x = c(0.35), y = c(15), label = c("supp. narrowed"), 
           color="red", size=3.5) + 
  annotate("segment", x = min(res.x), xend = min(res.x), y = 14.8, yend = 15.2, colour = "red", size=0.1)+
  annotate("segment", x = max(res.x), xend = max(res.x), y = 14.8, yend = 15.2, colour = "red", size=0.1)
ggsave("figures/CovAdjust_xDepw.pdf", width=pdf.width, height=pdf.height)



#######################################
##      Choose smoothness            ##
#######################################
fig <- ggplot() + theme_bw() +
                  theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        axis.text.x=element_blank(), axis.text.y=element_blank(),
                        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                        legend.position = "none") + labs(x="X", y="Y")
dotsAndline <- binsreg(y, x, nbins = 10, line = c(1,0))
dots <- dotsAndline$data.plot[[1]]$data.dots
line <- dotsAndline$data.plot[[1]]$data.line
fig <- fig + geom_line(data=data, aes(x=x, y=true), colour="darkgrey") +
             geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5)
ggsave("figures/p1-s0.pdf", width=pdf.width, height=pdf.height)

fig <- ggplot() + theme_bw() +
                  theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        axis.text.x=element_blank(), axis.text.y=element_blank(),
                        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                        legend.position = "none") + labs(x="X", y="Y")
dotsAndline <- binsreg(y, x, nbins = 10, line = c(1,1))
dots <- dotsAndline$data.plot[[1]]$data.dots
line <- dotsAndline$data.plot[[1]]$data.line
fig <- fig + geom_line(data=data, aes(x=x, y=true), colour="darkgrey")+
             geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5) 
  
ggsave("figures/p1-s1.pdf", width=pdf.width, height=pdf.height)

fig <- ggplot() + theme_bw() +
                  theme(panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        axis.text.x=element_blank(), axis.text.y=element_blank(),
                        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                        legend.position = "none") + labs(x="X", y="Y")
dotsAndline <- binsreg(y, x, nbins = 10, line = c(2,0))
dots <- dotsAndline$data.plot[[1]]$data.dots
line <- dotsAndline$data.plot[[1]]$data.line
fig <- fig + geom_line(data=data, aes(x=x, y=true), colour="darkgrey")+
             geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5)
  
ggsave("figures/p2-s0.pdf", width=pdf.width, height=pdf.height)

fig <- ggplot() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
        legend.position = "none") + labs(x="X", y="Y")
dotsAndline <- binsreg(y, x, nbins = 10, line = c(2,1))
dots <- dotsAndline$data.plot[[1]]$data.dots
line <- dotsAndline$data.plot[[1]]$data.line
fig <- fig + geom_line(data=data, aes(x=x, y=true), colour="darkgrey")+
             geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5) 
  
ggsave("figures/p2-s1.pdf", width=pdf.width, height=pdf.height)



################################
##     Different J            ##
################################

# small J
fig <- binsreg(y, x, nbins = 10, line=c(0,0), bycolors=c("blue"))$bins_plot
fig <- fig + geom_line(data=data, aes(x=x, y=true), colour="darkgrey") + 
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.text.x=element_blank(), axis.text.y=element_blank(),
                   axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                   legend.position = "none", legend.background = element_rect(fill = "transparent"))
ggsave("figures/regressogram_Jsmall.pdf", width=pdf.width, height=pdf.height)

J.opt <- binsregselect(y, x)$nbinsdpi
fig   <- binsreg(y, x, nbins=J.opt, line=c(0,0), bycolors=c("blue"))$bins_plot
fig <- fig +  geom_line(data=data, aes(x=x, y=true), colour="darkgrey") + 
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.text.x=element_blank(), axis.text.y=element_blank(),
                    axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                    legend.position = "none", legend.background = element_rect(fill = "transparent"))
ggsave("figures/regressogram_Jopt.pdf", width=pdf.width, height=pdf.height)



#########################################
##          Inference:                 ##
##      pointwise and Uniform          ##
#########################################

bin.membership <- findInterval(x, knots, rightmost.closed = TRUE)
bin.means <- tapply(y, bin.membership, mean)

# HSK case 1
new.y <- y  +   1.5*((bin.membership - 2.5)^2/50)*(y - bin.means[bin.membership])

fig <- ggplot() +  theme_bw()
fig <- fig + geom_point(data=data.frame(x=x, y=new.y), aes(x=x, y=y), shape=16, col="lightgrey") + 
             ylim(13,25) + 
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.text.x=element_blank(), axis.text.y=element_blank(),
                   axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                   legend.position="none") + labs(x="X", y="Y")
model <- binsreg(new.y, x, line = c(0,0), ci=c(1,1), cb=c(1,1))
dots <- model$data.plot[[1]]$data.dots
line <- model$data.plot[[1]]$data.line
ci <- model$data.plot[[1]]$data.ci
cb <- model$data.plot[[1]]$data.cb
fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5) +
             geom_errorbar(data=ci, aes(x=x, ymin=ci.l, ymax=ci.r), col="blue", size=0.5, linetype=1) +
             geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), fill="blue", alpha=0.2)
ggsave("figures/inference_hsk1_p0s0.pdf", width=pdf.width, height=pdf.height)

fig <- ggplot() +  theme_bw()
fig <- fig + geom_point(data=data.frame(x=x, y=new.y), aes(x=x, y=y), shape=16, col="lightgrey") +
             ylim(13,25) + 
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.text.x=element_blank(), axis.text.y=element_blank(),
                   axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                   legend.position="none") + labs(x="X", y="Y")
model <- binsreg(new.y, x, dots=c(2,2), line = c(2,2), ci=c(3,3), cb=c(3,3))
dots <- model$data.plot[[1]]$data.dots
line <- model$data.plot[[1]]$data.line
ci <- model$data.plot[[1]]$data.ci
cb <- model$data.plot[[1]]$data.cb
fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5) +
             geom_errorbar(data=ci, aes(x=x, ymin=ci.l, ymax=ci.r), col="blue", size=0.5, linetype=1) +
             geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), fill="blue", alpha=0.2)
ggsave("figures/inference_hsk1_p2s2.pdf", width=pdf.width, height=pdf.height)


# HSK case 2
new.y <- y  +   10*((abs(bin.membership - 3.5) - 4.5)^2/50)*(y - bin.means[bin.membership])

fig <- ggplot() +  theme_bw()
fig <- fig + geom_point(data=data.frame(x=x, y=new.y), aes(x=x, y=y), shape=16, col="lightgrey") +
  ylim(13,25) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
        legend.position="none") + labs(x="X", y="Y")
model <- binsreg(new.y, x, line = c(0,0), ci=c(1,1), cb=c(1,1))
dots <- model$data.plot[[1]]$data.dots
line <- model$data.plot[[1]]$data.line
ci <- model$data.plot[[1]]$data.ci
cb <- model$data.plot[[1]]$data.cb
fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5) +
             geom_errorbar(data=ci, aes(x=x, ymin=ci.l, ymax=ci.r), col="blue", size=0.5, linetype=1) +
             geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), fill="blue", alpha=0.2)
ggsave("figures/inference_hsk2_p0s0.pdf", width=pdf.width, height=pdf.height)

fig <- ggplot() +  theme_bw()
fig <- fig + geom_point(data=data.frame(x=x, y=new.y), aes(x=x, y=y), shape=16, col="lightgrey") +
  ylim(13,25) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
        legend.position="none") + labs(x="X", y="Y")
model <- binsreg(new.y, x, dots=c(2,2), line = c(2,2), ci=c(3,3), cb=c(3,3))
dots <- model$data.plot[[1]]$data.dots
line <- model$data.plot[[1]]$data.line
ci <- model$data.plot[[1]]$data.ci
cb <- model$data.plot[[1]]$data.cb
fig <- fig + geom_point(data=dots, aes(x=x, y=fit), col="blue", size=2, shape=19) +
             geom_line(data=line, aes(x=x, y=fit), col="blue", size=0.5) +
             geom_errorbar(data=ci, aes(x=x, ymin=ci.l, ymax=ci.r), col="blue", size=0.5, linetype=1) +
             geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), fill="blue", alpha=0.2)
ggsave("figures/inference_hsk2_p2s2.pdf", width=pdf.width, height=pdf.height)


########################################
##       Hypothesis testing:          ##
##     graphical illustration         ##
########################################

# Linear?
# dots and lm fit
model <- binsreg(y, x, line=c(0,0), cb=c(1,1), polyreg = 1)
dots <- model$data.plot[[1]]$data.dots; dots$Sname <- "binscatter"
line <- model$data.plot[[1]]$data.line
cb <- model$data.plot[[1]]$data.cb
lfit  <- model$data.plot[[1]]$data.poly; lfit$Sname <- "linear fit"
fig <- ggplot() +  theme_bw() + ylim(15,22) + 
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         axis.text.x=element_blank(), axis.text.y=element_blank(),
                         axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                         legend.position = c(0.9,0.95), 
                         legend.background = element_rect(fill = "transparent")) + 
                   labs(x="X", y="Y")
fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=19)
fig <- fig + geom_line(data=line, aes(x=x, y=fit), size=0.1, col="blue")
fig <- fig + geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2, fill="blue")
fig <- fig + geom_line(data=lfit, aes(x=x, y=fit, colour=Sname), size=0.1)
fig <- fig + scale_color_manual(name="", values = c("blue", "black"),
                                guide=guide_legend(override.aes = list(
                                linetype=c(1,1), shape=c(19, NA))))
ggsave("figures/band-linearity.pdf", width=pdf.width, height=pdf.height)

# Compare two groups
set.seed(1234)
data <- dgp(N=N, sig=sig, fun=mu, control = T)
y <- data$y; x <- data$x; w <- data$w
data1 <- dgp(N=N, sig=sig, fun=mu1, control = T)
y1 <- data1$y; x1 <- data1$x; w1 <- data1$w

fig <- ggplot() +  theme_bw() + ylim(15,22) + 
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         axis.text.x=element_blank(), axis.text.y=element_blank(),
                         axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                         legend.position = c(0.9,0.95), 
                         legend.background = element_rect(fill = "transparent")) + 
                   labs(x="X", y="Y")
model <- binsreg(y, x, line=c(0,0), cb=c(1,1))
dots <- model$data.plot[[1]]$data.dots; dots$Sname <- "Group 1"
line <- model$data.plot[[1]]$data.line; line$Sname <- "Group 1"
cb <- model$data.plot[[1]]$data.cb

fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=19)
fig <- fig + geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=1)
fig <- fig + geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2, fill="blue")

model <- binsreg(y1, x1, line=c(0,0), cb=c(1,1))
dots <- model$data.plot[[1]]$data.dots; dots$Sname <- "Group 2"
line <- model$data.plot[[1]]$data.line; line$Sname <- "Group 2"
cb <- model$data.plot[[1]]$data.cb

fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=15)
fig <- fig + geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=2)
fig <- fig + geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2, fill="green4")

fig <- fig + scale_color_manual(name="", values = c("blue", "green4"),
                                guide=guide_legend(override.aes = list(
                                linetype=c(1,2), shape=c(19, 15))))
ggsave("figures/band-groupDiff.pdf", width=pdf.width, height=pdf.height)


##################################
##       Hypothesis testing:    ##
##        Formal test           ##
##################################
result.test <- matrix(NA, 6, 6)
set.seed(1234)
data <- dgp(N=N, sig=sig, fun=mu, control = T)
y <- data$y; x <- data$x; w <- data$w

# Parametric form
# prepare x grids
subset <- (x <= 0.3)       # half sample

# constant?
est <- binsregtest(y, x, w=NULL, bins = c(0,0), testmodelpoly=0, subset=subset)
result.test[1,1:3] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)
est <- binsregtest(y, x, w=NULL, bins = c(0,0), testmodelpoly=0)
result.test[1,4:6] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)

# linear?
est <- binsregtest(y, x, w=NULL, bins = c(0,0), testmodelpoly=1, subset=subset)
result.test[2,1:3] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)
est <- binsregtest(y, x, w=NULL, bins = c(0,0), testmodelpoly=1)
result.test[2,4:6] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)

# Quadratic?
est <- binsregtest(y, x, w=NULL, bins = c(0,0), testmodelpoly=2, subset=subset)
result.test[3,1:3] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)
est <- binsregtest(y, x, w=NULL, bins = c(0,0), testmodelpoly=2)
result.test[3,4:6] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)

# Shape restriction
# negativity: <=0?
est <- binsregtest(y, x, w=NULL, deriv=0, testshapel=0, subset=subset)
result.test[4, 1:3] <- c(est$testshapeL$stat.shapeL, est$testshapeL$pval, est$opt$nbins)
est <- binsregtest(y, x, w=NULL, deriv=0, testshapel=0)
result.test[4, 4:6] <- c(est$testshapeL$stat.shapeL, est$testshapeL$pval, est$opt$nbins)

# Decreasing: first-order deriv <= 0 ?
est <- binsregtest(y, x, w=NULL, bins=c(1,1), deriv=1, testshapel=0, subset=subset)
result.test[5, 1:3] <- c(est$testshapeL$stat.shapeL, est$testshapeL$pval, est$opt$nbins)
est <- binsregtest(y, x, w=NULL, bins=c(1,1), deriv=1, testshapel=0)
result.test[5, 4:6] <- c(est$testshapeL$stat.shapeL, est$testshapeL$pval, est$opt$nbins)

# Concavity: second-order deriv <= 0 ?
est <- binsregtest(y, x, w=NULL, bins=c(2,2), deriv=2, testshapel=0, subset=subset)
result.test[6, 1:3] <- c(est$testshapeL$stat.shapeL, est$testshapeL$pval, est$opt$nbins)
est <- binsregtest(y, x, w=NULL, bins=c(2,2), deriv=2, testshapel=0)
result.test[6, 4:6] <- c(est$testshapeL$stat.shapeL, est$testshapeL$pval, est$opt$nbins)

result.test <- round(result.test, 3)

# make table
n.cgroup <- c(3, 3)
cgroup   <- c("Half Support ($n=482$)", "Full Support ($n=1000$)")
colheads <- rep(c("Test Statistic", "P-value", "$J$"), 2)
rowname <- c("Constant", "Linear", "Quadratic", 
             "Negativity", "Decreasingness", "Concavity")
n.rgroup <- c(3, 3)
rgroup <- c("Parametric Specification", "Shape Restrictions")

latex(result.test, file=paste("figures/testing", ".txt", sep = ""), 
      append=FALSE, table.env=FALSE, center="none", title="",
      n.cgroup=n.cgroup, cgroup=cgroup, colheads=colheads,
      n.rgroup=n.rgroup, rgroup=rgroup, rowname=rowname)

########################
# Graphical illustration
est.bin <- binsreg(y, x, line = c(0,0), cb=c(1,1), polyreg = 1, subset=subset)
dots <- est.bin$data.plot[[1]]$data.dots; dots$Sname <- "binscatter"
line <- est.bin$data.plot[[1]]$data.line; line$Sname <- "binscatter"
cb <- est.bin$data.plot[[1]]$data.cb
lfit <- est.bin$data.plot[[1]]$data.poly; lfit$Sname <- "linear"
est.bin <- binsreg(y, x, polyreg = 2, subset=subset)
qfit <- est.bin$data.plot[[1]]$data.poly; qfit$Sname <- "quadratic"
est.bin <- binsreg(y, x, polyreg = 0, subset=subset)
const <- est.bin$data.plot[[1]]$data.poly; const$Sname <- "constant"

fig <- ggplot() +  theme_bw() + ylim(14,24) + 
                   theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(),
                         axis.text.x=element_blank(), axis.text.y=element_blank(),
                         axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
                         legend.position = c(0.9,0.9), 
                         legend.background = element_rect(fill = "transparent")) + 
                   labs(x="X", y="Y")
fig <- fig + geom_line(data=const, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=2)
fig <- fig + geom_point(data=dots, aes(x=x, y=fit, colour=Sname), size=2, shape=19)
fig <- fig + geom_line(data=line, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=1)
fig <- fig + geom_ribbon(data=cb, aes(x=x, ymin=cb.l, ymax=cb.r), alpha=0.2, fill="blue")
fig <- fig + geom_line(data=lfit, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=1)
fig <- fig + geom_line(data=qfit, aes(x=x, y=fit, colour=Sname), size=0.1, linetype=4)

fig <- fig + scale_color_manual(name="", values = c("blue", "grey", "black", "black"),
                                guide=guide_legend(override.aes = list(
                                linetype=c(1,2,1,4), shape=c(19, NA, NA, NA))))
ggsave("figures/testing-subsample.pdf", width=pdf.width, height=pdf.height)

# full sample
est.bin <- binsreg(y, x, line = c(0,0), cb=c(1,1), polyreg = 1)
dots <- est.bin$data.plot[[1]]$data.dots; dots$Sname <- "binscatter"
line <- est.bin$data.plot[[1]]$data.line; line$Sname <- "binscatter"
cb <- est.bin$data.plot[[1]]$data.cb
lfit <- est.bin$data.plot[[1]]$data.poly; lfit$Sname <- "linear"
est.bin <- binsreg(y, x, polyreg = 2)
qfit <- est.bin$data.plot[[1]]$data.poly; qfit$Sname <- "quadratic"
est.bin <- binsreg(y, x, polyreg = 0)
const <- est.bin$data.plot[[1]]$data.poly; const$Sname <- "constant"

fig <- ggplot() +  theme_bw() + ylim(14,24) + 
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

ggsave("figures/testing-fullsample.pdf", width=pdf.width, height=pdf.height)



####################################################################
#### Section 7 #####################
rm(list=ls(all=TRUE))
library(binsreg); library(ggplot2)
data <- read.csv("binsreg_illustration_data.csv")
pdf.width <- 6
pdf.height <- 4.5
y <- data$giniIndex; x <- data$medianHouseholdIncome/1000;
w <- data[c("percentBachelorsEdu", "medianAge", "uninsuredRate", "percentHsEdu", "ueRate")]

plot <- binsreg(y,x,line = c(3,3), cb=c(3,3), bycolors=c("blue"))$bins_plot
plot + theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())
ggsave("figures/binsreg_nocontrol.pdf", width=pdf.width, height=pdf.height)
plot <- binsreg(y,x,w=w,line = c(3,3), cb=c(3,3), bycolors=c("blue"))$bins_plot
plot + theme(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())
ggsave("figures/binsreg_control.pdf", width=pdf.width, height=pdf.height)

# Testing
result.test <- matrix(NA, 6, 3)
# Parametric form
# constant?
est <- binsregtest(y, x, w=w, bins = c(0,0), testmodelpoly=0)
result.test[1,1:3] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)

# linear?
est <- binsregtest(y, x, w=w, bins = c(0,0), testmodelpoly=1)
result.test[2,1:3] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)

# Quadratic?
est <- binsregtest(y, x, w=w, bins = c(0,0), testmodelpoly=2)
result.test[3,1:3] <- c(est$testpoly$stat.poly, est$testpoly$pval.poly, est$opt$nbins)

# Shape restriction
# Positivity: >=0?
est <- binsregtest(y, x, w=w, deriv=0, testshaper=0)
result.test[4, 1:3] <- c(est$testshapeR$stat.shapeR, est$testshapeR$pval, est$opt$nbins)

# Decreasing: first-order deriv <= 0 ?
est <- binsregtest(y, x, w=w, bins=c(1,1), deriv=1, testshapel=0)
result.test[5, 1:3] <- c(est$testshapeL$stat.shapeL, est$testshapeL$pval, est$opt$nbins)

# Convexity: second-order deriv >= 0 ?
est <- binsregtest(y, x, w=w, bins=c(2,2), deriv=2, testshaper=0)
result.test[6, 1:3] <- c(est$testshapeR$stat.shapeR, est$testshapeR$pval, est$opt$nbins)

result.test <- round(result.test, 3)

# make table
n.cgroup <- c(3)
cgroup   <- NULL
colheads <- rep(c("Test Statistic", "P-value", "$J$"), 1)
rowname <- c("Constant", "Linear", "Quadratic", 
             "Positivity", "Decreasingness", "Convexity")
n.rgroup <- c(3, 3)
rgroup <- c("Parametric Specification", "Shape Restrictions")

latex(result.test, file=paste("figures/binsreg_testing", ".txt", sep = ""), 
      append=FALSE, table.env=FALSE, center="none", title="",
      n.cgroup=n.cgroup, cgroup=cgroup, colheads=colheads,
      n.rgroup=n.rgroup, rgroup=rgroup, rowname=rowname)





