################################################################################
# Binscatter: illustration file -- Supplemental Appendix
# Authors: M. D. Cattaneo, R. Crump, M. Farrell and Y. Feng
# Last update: 20-MAR-2019
################################################################################
rm(list=ls(all=TRUE))
library("binsreg")
library('ggplot2')

data <- read.csv("CCFF_2019_Binscatter--SA.csv")




#############################################################
# Make Gini Coefficient plots by income
#############################################################
giniIndex <- data$giniIndex
percentHsEdu <- data$percentHsEdu
percentBachelorsEdu <- data$percentBachelorsEdu
medianHouseholdIncome <- data$medianHouseholdIncome
ueRate <- data$ueRate
uninsuredRate <- data$uninsuredRate
medianAge <- data$medianAge
percentNoWeb <- data$percentNoWeb
percentWithWeb <- data$percentWithWeb
meanHouseholdSize <- data$meanHouseholdSize
meanHouseholdIncome <- data$meanHouseholdIncome
perCapitaIncome <- data$perCapitaIncome
medianHouseholdIncomeGrowth <- data$medianHouseholdIncomeGrowth
meanHouseholdIncomeGrowth <- data$meanHouseholdIncomeGrowth
perCapitaIncomeGrowth <- data$perCapitaIncomeGrowth
percentWeb <- 100 - percentNoWeb
w <- cbind(percentBachelorsEdu, percentHsEdu, medianAge, ueRate, uninsuredRate)

giniNoControls<- binsreg(giniIndex, medianHouseholdIncome)
giniNoControls$bins_plot + labs(title ="2017 Gini Index by Median Household Income (No Controls)", y = "Gini Index", x = "Median Household Income")
ggsave("giniNoControls.pdf")

giniNoControls_cb00 <-  binsreg(giniIndex, medianHouseholdIncome, cb=c(0,0))
giniNoControls_cb00$bins_plot + labs(title ="2017 Gini Index by Median Household Income (No Controls)", y = "Gini Index", x = "Median Household Income")
ggsave("giniNoControls_cb00.pdf")


giniNoControls_cb33 <-  binsreg(giniIndex, medianHouseholdIncome, cb=c(3,3), line=c(3,3))
giniNoControls_cb33$bins_plot + labs(title ="2017 Gini Index by Median Household Income (No Controls)", y = "Gini Index", x = "Median Household Income")
ggsave("giniNoControls_cb33.pdf")




giniWithControls <- binsreg(giniIndex, medianHouseholdIncome, w=w)
giniWithControls$bins_plot + labs(title ="2017 Gini Index by Median Household Income (With Controls)", y = "Gini Index", x = "Median Household Income")
ggsave("giniWithControls.pdf")


giniWithControls_cb00 <- binsreg(giniIndex, medianHouseholdIncome, w=w, cb=c(0,0))
giniWithControls_cb00$bins_plot + labs(title ="2017 Gini Index by Median Household Income (With Controls)", y = "Gini Index", x = "Median Household Income")
ggsave("giniWithControls_cb00.pdf")
        
giniWithControls_cb33 <- binsreg(giniIndex, medianHouseholdIncome, w=w, cb=c(3,3), line=c(3,3))
giniWithControls_cb33$bins_plot + labs(title ="2017 Gini Index by Median Household Income (With Controls)", y = "Gini Index", x = "Median Household Income")
ggsave("giniWithControls_cb33.pdf")
                

                     
 #############################################################
 # Internet Access by income
 #############################################################

giniIndex <- data$giniIndex
percentHsEdu <- data$percentHsEdu
percentBachelorsEdu <- data$percentBachelorsEdu
medianHouseholdIncome <- data$medianHouseholdIncome
ueRate <- data$ueRate
uninsuredRate <- data$uninsuredRate
medianAge <- data$medianAge
percentNoWeb <- data$percentNoWeb
percentWithWeb <- data$percentWithWeb
meanHouseholdSize <- data$meanHouseholdSize
meanHouseholdIncome <- data$meanHouseholdIncome
perCapitaIncome <- data$perCapitaIncome
medianHouseholdIncomeGrowth <- data$medianHouseholdIncomeGrowth
meanHouseholdIncomeGrowth <- data$meanHouseholdIncomeGrowth
perCapitaIncomeGrowth <- data$perCapitaIncomeGrowth

w <- cbind(percentBachelorsEdu, percentHsEdu, medianAge, ueRate, uninsuredRate)


webNoControls<- binsreg(percentWeb, medianHouseholdIncome)
webNoControls$bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (No Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
ggsave("webNoControls.pdf")

webNoControls_cb00 <-  binsreg(percentWeb, medianHouseholdIncome, cb=c(0,0))
webNoControls_cb00$bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (No Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
ggsave("webNoControls_cb00.pdf")


webNoControls_cb33 <-  binsreg(percentWeb, medianHouseholdIncome, cb=c(3,3), line=c(3,3))
webNoControls_cb33$bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (No Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
ggsave("webNoControls_cb33.pdf")




webWithControls <- binsreg(percentWeb, medianHouseholdIncome, w=w)
webWithControls$bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (With Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
ggsave("webWithControls.pdf")


webWithControls_cb00 <- binsreg(percentWeb, medianHouseholdIncome, w=w, cb=c(0,0))
webWithControls_cb00$bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (With Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
ggsave("webWithControls_cb00.pdf")

webWithControls_cb33 <- binsreg(percentWeb, medianHouseholdIncome, w=w, cb=c(3,3), line=c(3,3))
webWithControls_cb33$bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (With Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
ggsave("webWithControls_cb33.pdf")

        


#############################################################
# Uninsured Rate by income
###########################################################
            
giniIndex <- data$giniIndex
percentHsEdu <- data$percentHsEdu
percentBachelorsEdu <- data$percentBachelorsEdu
medianHouseholdIncome <- data$medianHouseholdIncome
ueRate <- data$ueRate
uninsuredRate <- data$uninsuredRate
medianAge <- data$medianAge
percentNoWeb <- data$percentNoWeb
percentWithWeb <- data$percentWithWeb
meanHouseholdSize <- data$meanHouseholdSize
meanHouseholdIncome <- data$meanHouseholdIncome
perCapitaIncome <- data$perCapitaIncome
medianHouseholdIncomeGrowth <- data$medianHouseholdIncomeGrowth
meanHouseholdIncomeGrowth <- data$meanHouseholdIncomeGrowth
perCapitaIncomeGrowth <- data$perCapitaIncomeGrowth


w <- cbind(percentBachelorsEdu, percentHsEdu, medianAge, ueRate)
            
            
uninsuredNoControls<- binsreg(uninsuredRate, perCapitaIncome)
uninsuredNoControls$bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (No Controls)", y = "Uninsured Rate", x = "Per Capita Income")
ggsave("uninsuredNoControls.pdf")

uninsuredNoControls_cb00 <-  binsreg(uninsuredRate, perCapitaIncome, cb=c(0,0))
uninsuredNoControls_cb00$bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (No Controls)", y = "Uninsured Rate", x = "Per Capita Income")
ggsave("uninsuredNoControls_cb00.pdf")


uninsuredNoControls_cb33 <-  binsreg(uninsuredRate, perCapitaIncome, cb=c(3,3), line=c(3,3))
uninsuredNoControls_cb33$bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (No Controls)", y = "Uninsured Rate", x = "Per Capita Income")
ggsave("uninsuredNoControls_cb33.pdf")




uninsuredWithControls <- binsreg(uninsuredRate, perCapitaIncome, w=w)
uninsuredWithControls$bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (With Controls)", y = "Uninsured Rate", x = "Per Capita Income")
ggsave("uninsuredWithControls.pdf")


uninsuredWithControls_cb00 <- binsreg(uninsuredRate, perCapitaIncome, w=w, cb=c(0,0))
uninsuredWithControls_cb00$bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (With Controls)", y = "Uninsured Rate", x = "Per Capita Income")
ggsave("uninsuredWithControls_cb00.pdf")

uninsuredWithControls_cb33 <- binsreg(uninsuredRate, perCapitaIncome, w=w, cb=c(3,3), line=c(3,3))
uninsuredWithControls_cb33$bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (With Controls)", y = "Uninsured Rate", x = "Per Capita Income")
ggsave("uninsuredWithControls_cb33.pdf")
            
       
