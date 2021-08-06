################################################################################
# Binscatter: illustration file -- Supplemental Appendix
# Authors: M. D. Cattaneo, R. Crump, M. Farrell and Y. Feng
# Last update: 05-AUG-2021
################################################################################
rm(list=ls(all=TRUE))
library("binsreg")
library('ggplot2')

data <- read.csv("CCFF_2021_Binscatter--SA.csv")

#############################################################
# Make Gini Coefficient plots by income
#############################################################
giniIndex <- data$giniIndex
medianHouseholdIncome <- data$medianHouseholdIncome
percentBachelorsEdu <- data$percentBachelorsEdu
medianAge <- data$medianAge
uninsuredRate <- data$uninsuredRate
percentHsEdu <- data$percentHsEdu
ueRate <- data$ueRate

wGini <- cbind(percentBachelorsEdu, percentHsEdu, medianAge, ueRate, uninsuredRate)

giniNoControls<- binsreg(giniIndex, medianHouseholdIncome)
giniNoControls$bins_plot + labs(title ="2017 Gini Index by Median Household Income (No Controls)", y = "Gini Index", x = "Median Household Income")
ggsave("figures/giniNoControls.pdf")

giniNoControls_cb11 <-  binsreg(giniIndex, medianHouseholdIncome, ci=c(1,1), cb=c(1,1))
giniNoControls_cb11$bins_plot + labs(title ="2017 Gini Index by Median Household Income (No Controls)", y = "Gini Index", x = "Median Household Income")
ggsave("figures/giniNoControls_cb11.pdf")

giniNoControls_cb33 <-  binsreg(giniIndex, medianHouseholdIncome, cb=c(3,3), line=c(3,3))
giniNoControls_cb33$bins_plot + labs(title ="2017 Gini Index by Median Household Income (No Controls)", y = "Gini Index", x = "Median Household Income")
ggsave("figures/giniNoControls_cb33.pdf")

giniWithControls <- binsreg(giniIndex, medianHouseholdIncome, w=wGini)
giniWithControls$bins_plot + labs(title ="2017 Gini Index by Median Household Income (With Controls)", y = "Gini Index", x = "Median Household Income")
ggsave("figures/giniWithControls.pdf")

giniWithControls_cb11 <- binsreg(giniIndex, medianHouseholdIncome, w=wGini, ci=c(1,1), cb=c(1,1))
giniWithControls_cb11$bins_plot + labs(title ="2017 Gini Index by Median Household Income (With Controls)", y = "Gini Index", x = "Median Household Income")
ggsave("figures/giniWithControls_cb11.pdf")
        
giniWithControls_cb33 <- binsreg(giniIndex, medianHouseholdIncome, w=wGini, cb=c(3,3), line=c(3,3))
giniWithControls_cb33$bins_plot + labs(title ="2017 Gini Index by Median Household Income (With Controls)", y = "Gini Index", x = "Median Household Income")
ggsave("figures/giniWithControls_cb33.pdf")
                     
#############################################################
# Internet Access by income
#############################################################
percentNoWeb <- data$percentNoWeb
percentWeb <- 100 - percentNoWeb
under40 <- (medianAge < 40)

wWeb <- cbind(percentBachelorsEdu, percentHsEdu, medianAge, ueRate, uninsuredRate)
wWebNoAge <- cbind(percentBachelorsEdu, percentHsEdu, ueRate, uninsuredRate)

webNoControls<- binsreg(percentWeb, medianHouseholdIncome)
webNoControls$bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (No Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
ggsave("figures/webNoControls.pdf")

webNoControlsByAge <- binsreg(percentWeb, medianHouseholdIncome, by=under40)
webNoControlsByAge$bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (No Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
ggsave("figures/webNoControls_byAge.pdf")

webNoControlsByAge_cb33 <- binsreg(percentWeb, medianHouseholdIncome, by=under40, cb=c(3,3), line=c(3,3), plotxrange=c(25000,150000))
webNoControlsByAge_cb33$bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (No Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
ggsave("figures/webNoControls_byAge_cb33.pdf")

webWithControls <- binsreg(percentWeb, medianHouseholdIncome, w=wWeb)
webWithControls$bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (With Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
ggsave("figures/webWithControls.pdf")

webWithControlsByAge <- binsreg(percentWeb, medianHouseholdIncome, w=wWebNoAge, by=under40)
webWithControlsByAge$bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (With Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
ggsave("figures/webWithControls_byAge.pdf")

webWithControlsByAge_cb33 <- binsreg(percentWeb, medianHouseholdIncome, w=wWebNoAge, by=under40, cb=c(3,3), line=c(3,3), plotxrange=c(25000,150000))
webWithControlsByAge_cb33 $bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (With Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
ggsave("figures/webWithControls_byAge_cb33.pdf")

#############################################################
# Uninsured Rate by income
###########################################################
perCapitaIncome <- data$perCapitaIncome
uninsuredRate <- data$uninsuredRate
wUninsured <- cbind(percentBachelorsEdu, percentHsEdu, medianAge, ueRate)

uninsuredNoControls<- binsreg(uninsuredRate, perCapitaIncome)
uninsuredNoControls$bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (No Controls)", y = "Uninsured Rate", x = "Per Capita Income")
ggsave("figures/uninsuredNoControls.pdf")

uninsuredNoControls_ci33 <-  binsreg(uninsuredRate, perCapitaIncome, ci=c(3,3), polyreg=3, plotxrange=c(0,100000))
uninsuredNoControls_ci33$bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (No Controls)", y = "Uninsured Rate", x = "Per Capita Income")
ggsave("figures/uninsuredNoControls_ci33.pdf")

uninsuredNoControls_cb33 <-  binsreg(uninsuredRate, perCapitaIncome, cb=c(3,3), line=c(3,3), plotxrange=c(0,100000))
uninsuredNoControls_cb33$bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (No Controls)", y = "Uninsured Rate", x = "Per Capita Income")
ggsave("figures/uninsuredNoControls_cb33.pdf")

uninsuredWithControls <- binsreg(uninsuredRate, perCapitaIncome, w=wUninsured)
uninsuredWithControls$bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (With Controls)", y = "Uninsured Rate", x = "Per Capita Income")
ggsave("figures/uninsuredWithControls.pdf")

uninsuredWithControls_ci33 <- binsreg(uninsuredRate, perCapitaIncome, w=wUninsured, ci=c(3,3), polyreg=3, plotxrange=c(0,100000))
uninsuredWithControls_ci33$bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (With Controls)", y = "Uninsured Rate", x = "Per Capita Income")
ggsave("figures/uninsuredWithControls_ci33.pdf")

uninsuredWithControls_cb33 <- binsreg(uninsuredRate, perCapitaIncome, w=wUninsured, cb=c(3,3), line=c(3,3), plotxrange=c(0,100000))
uninsuredWithControls_cb33$bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (With Controls)", y = "Uninsured Rate", x = "Per Capita Income")
ggsave("figures/uninsuredWithControls_cb33.pdf")
            
graphics.off()
