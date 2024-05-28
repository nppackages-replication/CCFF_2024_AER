################################################################################
# Replication "On Binscatter"
# Cattaneo, Crump, Farrell and Feng (2024)
# Date: 05-JAN-2024
# M Application
################################################################################
import pandas as pd
import numpy as np
from binsreg import *
import sklearn
import matplotlib.pyplot as plt
import statsmodels.api as sm 
from sklearn_pandas import DataFrameMapper

data_og = pd.io.stata.read_stata('../data/CCFF_2023_M_1.dta')
data_og['constant'] = 1

################################################################################
# Figure 6(a) Scatter plot
################################################################################
plt.scatter(data_og['x'], data_og['y'])
plt.xlabel("log Cluster Size")
plt.ylabel("log Number of Patents per Inventor per Year")

################################################################################
# Figure 6(b)-6(d) Scatter plot
################################################################################
data = data_og
data = data[['constant','bea_code','zd2','year','y','x']].dropna()

cov = data[['constant']].to_numpy()
bea_flags = DataFrameMapper([('bea_code', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]
zd2_flags = DataFrameMapper([('zd2', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]
year_flags = DataFrameMapper([('year', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]
X = np.concatenate((cov, bea_flags, year_flags, zd2_flags),axis=1)

reg = sm.regression.linear_model.OLS(data['y'], X).fit()
resid_y = reg.resid + np.mean(data['y'])
reg = sm.regression.linear_model.OLS(data['x'], X).fit()
resid_x = reg.resid + np.mean(data['x'])

res = binsreg(resid_y,resid_x,nbins=40, polyreg=1, plotxrange=[-5.5,-2.5], plotyrange=[-.35,-.15])

x = list(range(-5500,-2500))
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
plt.xlim([-5.5,-2.5])
plt.ylim([-.35,-.15])

dots2 = res.data_plot[0].dots
res = binsreg(data['y'], data['x'], X, nbins=40, noplot=True)
dots1 = res.data_plot[0].dots
plt.scatter(dots1['x'],dots1['fit'])
plt.scatter(dots2['x'],dots2['fit'])

################################################################################
# Figure 6(e) Optimal Choice of J/Confidence Band
################################################################################
data_og = pd.io.stata.read_stata('../data/CCFF_2023_M_2.dta')
data_og['constant'] = 1

data = data_og[['constant','bea_code','zd2','year','y','x','cluster1']].dropna()

cov = data[['constant']].to_numpy()
bea_flags = DataFrameMapper([('bea_code', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]
zd2_flags = DataFrameMapper([('zd2', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]
year_flags = DataFrameMapper([('year', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]
X = np.concatenate((cov, bea_flags, year_flags, zd2_flags),axis=1)

res = binsreg(data['y'], data['x'], X, cluster=data['cluster1'])

################################################################################
# Figure 6(f) Optimal Choice of J/Confidence Band (full specification)
################################################################################

data = data_og[['constant','class','cluster_bea_class','cluster1','cluster_class_year','cluster_zd_year','inventor_id','cluster_bea_year','org_new','y','x']].dropna()

c1_flags = DataFrameMapper([('class', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]
c2_flags = DataFrameMapper([('cluster_bea_class', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]
c3_flags = DataFrameMapper([('cluster1', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]
c4_flags = DataFrameMapper([('cluster_class_year', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]
c5_flags = DataFrameMapper([('cluster_zd_year', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]
c6_flags = DataFrameMapper([('inventor_id', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]
c7_flags = DataFrameMapper([('cluster_bea_year', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]
c8_flags = DataFrameMapper([('org_new', sklearn.preprocessing.LabelBinarizer())]).fit_transform(data)[:,1:]

X = np.concatenate((X,c1_flags,c2_flags,c3_flags,c4_flags,c5_flags,c6_flags,c7_flags,c8_flags),axis=1)

res = binsreg(data['y'], data['x'], X, cluster=data['cluster1'], randcut=1, cb=(1,1))