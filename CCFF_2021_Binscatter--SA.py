################################################################################
# Binscatter: illustration file -- Supplemental Appendix
# Authors: M. D. Cattaneo, R. Crump, M. Farrell and Y. Feng
# Last update: 05-AUG-2021
################################################################################

import pandas as pd
from plotnine import *
from binsreg import *

data = pd.read_csv("CCFF_2021_Binscatter--SA.csv")

#############################################################
# Make Gini Coefficient plots by income
#############################################################
giniIndex = data.giniIndex
medianHouseholdIncome = data.medianHouseholdIncome
percentBachelorsEdu = data.percentBachelorsEdu
medianAge = data.medianAge
uninsuredRate = data.uninsuredRate
percentHsEdu = data.percentHsEdu
ueRate = data.ueRate

col_index = ['giniIndex','medianHouseholdIncome','percentBachelorsEdu','medianAge',
            'uninsuredRate','percentHsEdu','ueRate']
wGini = data[col_index]

giniNoControls = binsreg(giniIndex, medianHouseholdIncome)
fig = giniNoControls.bins_plot + labs(title ="2017 Gini Index by Median Household Income (No Controls)", y = "Gini Index", x = "Median Household Income")
fig.save("figures/giniNoControls.pdf")

giniNoControls_cb11 =  binsreg(giniIndex, medianHouseholdIncome, ci=(1,1), cb=(1,1))
fig = giniNoControls_cb11.bins_plot + labs(title ="2017 Gini Index by Median Household Income (No Controls)", y = "Gini Index", x = "Median Household Income")
fig.save("figures/giniNoControls_cb11.pdf")

giniNoControls_cb33 =  binsreg(giniIndex, medianHouseholdIncome, cb=(3,3), line=(3,3))
fig = giniNoControls_cb33.bins_plot + labs(title ="2017 Gini Index by Median Household Income (No Controls)", y = "Gini Index", x = "Median Household Income")
fig.save("figures/giniNoControls_cb33.pdf")

giniWithControls = binsreg(giniIndex, medianHouseholdIncome, w=wGini)
fig = giniWithControls.bins_plot + labs(title ="2017 Gini Index by Median Household Income (With Controls)", y = "Gini Index", x = "Median Household Income")
fig.save("figures/giniWithControls.pdf")

giniWithControls_cb11 = binsreg(giniIndex, medianHouseholdIncome, w=wGini, ci=(1,1), cb=(1,1))
fig = giniWithControls_cb11.bins_plot + labs(title ="2017 Gini Index by Median Household Income (With Controls)", y = "Gini Index", x = "Median Household Income")
fig.save("figures/giniWithControls_cb11.pdf")
        
giniWithControls_cb33 = binsreg(giniIndex, medianHouseholdIncome, w=wGini, cb=(3,3), line=(3,3))
fig = giniWithControls_cb33.bins_plot + labs(title ="2017 Gini Index by Median Household Income (With Controls)", y = "Gini Index", x = "Median Household Income")
fig.save("figures/giniWithControls_cb33.pdf")
                     
#############################################################
# Internet Access by income
#############################################################
percentNoWeb = data.percentNoWeb
percentWeb = 100 - percentNoWeb
under40 = (medianAge < 40)

col_index = ['percentBachelorsEdu','medianAge',
            'uninsuredRate','percentHsEdu','ueRate']
wWeb = data[col_index]

col_index = ['percentBachelorsEdu','uninsuredRate','percentHsEdu','ueRate']
wWebNoAge = data[col_index]

webNoControls= binsreg(percentWeb, medianHouseholdIncome)
fig = webNoControls.bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (No Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
fig.save("figures/webNoControls.pdf")

webNoControlsByAge = binsreg(percentWeb, medianHouseholdIncome, by=under40)
fig = webNoControlsByAge.bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (No Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
fig.save("figures/webNoControls_byAge.pdf")

webNoControlsByAge_cb33 = binsreg(percentWeb, medianHouseholdIncome, by=under40, cb=(3,3), line=(3,3), plotxrange=(25000,150000))
fig = webNoControlsByAge_cb33.bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (No Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
fig.save("figures/webNoControls_byAge_cb33.pdf")

webWithControls = binsreg(percentWeb, medianHouseholdIncome, w=wWeb)
fig = webWithControls.bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (With Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
fig.save("figures/webWithControls.pdf")

webWithControlsByAge = binsreg(percentWeb, medianHouseholdIncome, w=wWebNoAge, by=under40)
fig = webWithControlsByAge.bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (With Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
fig.save("figures/webWithControls_byAge.pdf")

webWithControlsByAge_cb33 = binsreg(percentWeb, medianHouseholdIncome, w=wWebNoAge, by=under40, cb=(3,3), line=(3,3), plotxrange=(25000,150000))
fig = webWithControlsByAge_cb33 .bins_plot + labs(title ="2017 Percent With Internet Acess by Median Household Income (With Controls)", y = "Percent With Internet Acess", x = "Median Household Income")
fig.save("figures/webWithControls_byAge_cb33.pdf")

#############################################################
# Uninsured Rate by income
###########################################################
perCapitaIncome = data.perCapitaIncome
uninsuredRate = data.uninsuredRate
wUninsured = data[['percentBachelorsEdu','percentHsEdu','medianAge','ueRate']]

uninsuredNoControls= binsreg(uninsuredRate, perCapitaIncome)
fig = uninsuredNoControls.bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (No Controls)", y = "Uninsured Rate", x = "Per Capita Income")
fig.save("figures/uninsuredNoControls.pdf")

uninsuredNoControls_ci33 =  binsreg(uninsuredRate, perCapitaIncome, ci=(3,3), polyreg=3, plotxrange=(0,100000))
fig = uninsuredNoControls_ci33.bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (No Controls)", y = "Uninsured Rate", x = "Per Capita Income")
fig.save("figures/uninsuredNoControls_ci33.pdf")

uninsuredNoControls_cb33 =  binsreg(uninsuredRate, perCapitaIncome, cb=(3,3), line=(3,3), plotxrange=(0,100000))
fig = uninsuredNoControls_cb33.bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (No Controls)", y = "Uninsured Rate", x = "Per Capita Income")
fig.save("figures/uninsuredNoControls_cb33.pdf")

uninsuredWithControls = binsreg(uninsuredRate, perCapitaIncome, w=wUninsured)
fig = uninsuredWithControls.bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (With Controls)", y = "Uninsured Rate", x = "Per Capita Income")
fig.save("figures/uninsuredWithControls.pdf")

uninsuredWithControls_ci33 = binsreg(uninsuredRate, perCapitaIncome, w=wUninsured, ci=(3,3), polyreg=3, plotxrange=(0,100000))
fig = uninsuredWithControls_ci33.bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (With Controls)", y = "Uninsured Rate", x = "Per Capita Income")
fig.save("figures/uninsuredWithControls_ci33.pdf")

uninsuredWithControls_cb33 = binsreg(uninsuredRate, perCapitaIncome, w=wUninsured, cb=(3,3), line=(3,3), plotxrange=(0,100000))
fig = uninsuredWithControls_cb33.bins_plot + labs(title ="2017 Uninsured Rate by Per Capita Income (With Controls)", y = "Uninsured Rate", x = "Per Capita Income")
fig.save("figures/uninsuredWithControls_cb33.pdf")
            