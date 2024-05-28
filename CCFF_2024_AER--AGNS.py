################################################################################
# Replication "On Binscatter"
# Cattaneo, Crump, Farrell and Feng (2024)
# Date: 05-JAN-2024
# AGNS Application
################################################################################
import pandas as pd
import numpy as np
from binsreg import *
import sklearn
import matplotlib.pyplot as plt
import statsmodels.api as sm 
from sklearn_pandas import DataFrameMapper

data = pd.io.stata.read_stata('CCFF_2024_AER--AGNS.dta').dropna()
data = data.reset_index(drop=True)
data['constant'] = 1

################################################################################
# Figure 1(a) Scatter plot
################################################################################
plt.scatter(data['mtr90_lag3'], data['lnpat'])
plt.xlabel('Combined Marginal Tax Rate for 90th Percentile') 
plt.ylabel('Log Patents')

plt.xlim(-.8,0)
plt.ylim(0,10)

################################################################################
# Figure 1(b) Demonstration of scatter to binscatter
################################################################################
res = binsreg(data[['lnpat']], data[['mtr90_lag3']], nbins=10, line=[0,0], noplot=True)
lines = res.data_plot[0].line
verts = pd.concat((lines.drop_duplicates(subset="bin"),lines.iloc[-2:-1,:]))
dots = res.data_plot[0].dots

plt.scatter(data['mtr90_lag3'], data['lnpat'], alpha=.1, c="gray")
plt.scatter(dots['x'], dots['fit'], s=10, c="blue")
plt.vlines(verts['x'], ymin=2, ymax=10,linestyles='dashed')
plt.ylim(4, 8)
plt.xlim(-.9, 0)

################################################################################
# Figure 1(c) Demonstration of conventional binscatter plot
################################################################################
res = binsreg(data['lnpat'], data['mtr90_lag3'], nbins=10, polyreg=1)

################################################################################
# Figure 1(d) Demonstration of piecewise constant conditional mean estimate
################################################################################
res = binsreg(data['lnpat'], data['mtr90_lag3'], nbins=10, line=[0,0], dots=(0,0), polyreg=1, plotxrange=[-.9,0])

################################################################################
# Figure 2 Conditioning on covariates
################################################################################
cov = data[['top_corp_lag3','lreal_gdp_pc','lpopulation_density','rd_credit_lag3','constant']].to_numpy()
state_flags = DataFrameMapper([('statenum', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]
state_flags = state_flags - np.average(state_flags,weights=data['pop1940'],axis=0)
year_flags = DataFrameMapper([('year', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]
year_flags = year_flags - np.average(year_flags,weights=data['pop1940'],axis=0)
X = np.concatenate((cov, state_flags, year_flags),axis=1)

# Residualized Binscatter
reg = sm.regression.linear_model.WLS(data['lnpat'], X, data['pop1940']).fit()
resid_lnpat = reg.resid + np.mean(data['lnpat'])
reg = sm.regression.linear_model.WLS(data['mtr90_lag3'], X, data['pop1940']).fit()
resid_mtr90_lag3 = reg.resid + np.mean(data['mtr90_lag3'])

res = binsreg(resid_lnpat, resid_mtr90_lag3, weights=data['pop1940'], nbins=50, polyreg=1)

x = list(range(-525,-340))
x = pd.DataFrame([v/1000 for v in x], columns=['x'])
x['dots'] = np.zeros(len(x))

dots = res.data_plot[0].dots
bins = res.data_plot[0].data_bin
poly = res.data_plot[0].poly
bin_last = bins['bin_id'].iloc[-1]

bin_counter = 0
bin_right = float(bins[bins['bin_id']==bin_counter+1]['right.endpoint'].iloc[0]) # discrep
bin_dot = float(dots[dots['bin']==bin_counter].fit.iloc[0])
for index, row in x.iterrows():
    if row['x'] > bin_right:
        bin_counter += 1
        if bin_counter < bin_last:
            bin_right = float(bins[bins['bin_id']==(bin_counter+1)]['right.endpoint'].iloc[0])
            bin_dot = float(dots[dots['bin']==bin_counter].fit.iloc[0])
    
    x.iloc[index, 1] = bin_dot

plt.plot(x['x'],x['dots'])
plt.scatter(dots['x'], dots['fit'], s=10, c="blue")
plt.plot(poly['x'],poly['fit'])

# Correct handling of covariates (w/ regression line)
res = binsreg(data['lnpat'], data['mtr90_lag3'], X, weights=data['pop1940'], polyreg=1, randcut=1, nbins=50)

# Correct handling of covariates (w/out regression line)
res = binsreg(data['lnpat'], data['mtr90_lag3'], X, weights=data['pop1940'], randcut=1, nbins=50, plotxrange=[-.8,0])

################################################################################
# Figure 3 Confidence bands
################################################################################
# Direct binsreg implementation of confidence band
res = binsreg(data['lnpat'], data['mtr90_lag3'], X, weights=data['pop1940'], cb=[1,1], randcut=1, cluster=data[['fiveyrblockbystate','year']], plotxrange=[-.8,0])

res = binsreg(data['lnpat'], data['mtr90_lag3'], X, weights=data['pop1940'], cb=[1,1], randcut=1, cluster=data[['fiveyrblockbystate','year']], polyreg=1, plotxrange=[-.8,0])

################################################################################
# Figure 4 Choice of J
################################################################################
# 5 bins
res = binsreg(data['lnpat'], data['mtr90_lag3'], X, weights=data['pop1940'], nbins=5, randcut=1, cluster=data['fiveyrblockbystate'], polyreg=1, plotxrange=[-.8,0])
# Optimal bins
res = binsreg(data['lnpat'], data['mtr90_lag3'], X, weights=data['pop1940'], randcut=1, cluster=data['fiveyrblockbystate'], polyreg=1, plotxrange=[-.8,0])
# 50 bins
res = binsreg(data['lnpat'], data['mtr90_lag3'], X, weights=data['pop1940'], nbins=50, randcut=1, cluster=data['fiveyrblockbystate'], polyreg=1, plotxrange=[-.8,0])

################################################################################
# Figure 5 Confidence bands
################################################################################
res = binsreg(data['lnpat'], data['mtr90_lag3'], X, weights=data['pop1940'], nbins=5, randcut=1, cluster=data['fiveyrblockbystate'], cb=[0,0], plotxrange=[-.8,0])

################################################################################
# Figure SA-1 Evaluation Point
################################################################################
res = binsreg(data['lnpat'], data['mtr90_lag3'], data[['top_corp_lag3','lreal_gdp_pc','lpopulation_density','rd_credit_lag3']], weights=data['pop1940'], at="median", plotxrange=[-.8,0])
res = binsreg(data['lnpat'], data['mtr90_lag3'], data[['top_corp_lag3','lreal_gdp_pc','lpopulation_density','rd_credit_lag3']], weights=data['pop1940'], at=0, plotxrange=[-.8,0])