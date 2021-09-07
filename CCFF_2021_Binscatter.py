################################################################################
## Replication file: "On Binscatter"
## Authors: M. D. Cattaneo, R. Crump, M. Farrell and Y. Feng
## Script written by Ricardo Masini
## Last update: September 6, 2021
################################################################################

import pandas as pd
import numpy as np
import statsmodels.api as sm
from plotnine import *
from binsreg import *

np.random.seed(1234)

## Read data
data = pd.read_csv('CCFF_2021_Binscatter.csv')

## Rename columns
data = data.rename(columns={"y.true": "y_true", "y.wx": "y_wx", "w.x":"w_x"})

## Main dataset (t=1)
data1 = data[data["t"]==1]
x = data1.x
w = data1.w
y_true = data1['y_true']
y = data1.y
## Used to illustrate cov adj. (w indep of x)
w_x = data1['w_x']
y_wx = data1['y_wx']

## Second group dataset (t=2)
data2 = data[data["t"]==2]
x2 = data2.x
w2 = data2.w
y2_true = data2['y_true']
y2 = data2.y

## Specify knots
nbins = 10
knots = np.quantile(x,np.linspace(0,1, nbins+1))

pdf_width = 6
pdf_height = 4.5

################################################################################
## Figure 1: Canonical Binscatter
################################################################################
fig = ggplot() + theme_bw()
fig += geom_point(mapping=aes(x='x', y='y'), data=data1, color="lightgrey") 
fig += geom_vline(xintercept=knots, colour="black", linetype = "dashed", size = 0.2)
fig += ylim(12,22)
fig += theme(panel_grid_major = element_blank(),
            panel_grid_minor = element_blank(),
            axis_text_x = element_blank(), 
            axis_text_y = element_blank(),
            axis_ticks_major_x = element_blank(),
            axis_ticks_major_y = element_blank(),
            legend_position = "none")
fig += labs(x="X", y="Y")
out = binsreg(y, x, nbins=10, noplot=True)
dots  = out.data_plot[0].dots
fig += geom_point(mapping=aes(x='x',y='fit'),data=dots, color="blue", size=2)
fig.save("figures/addDots.pdf", width=pdf_width, height=pdf_height)

## dots and lm fit
dotsAndlm = binsreg(y, x, nbins=10, polyreg = 1, noplot=True)
dots = dotsAndlm.data_plot[0].dots
dots.insert(0,'Sname','binscatter')
lin = dotsAndlm.data_plot[0].poly
lin.insert(0,'Sname','linear fit')
fig = ggplot() +  theme_bw()
fig += theme(panel_grid_major = element_blank(),
            panel_grid_minor = element_blank(),
            axis_text_x = element_blank(), 
            axis_text_y = element_blank(),
            axis_ticks_major_x = element_blank(),
            axis_ticks_major_y = element_blank(),
            legend_position = (0.82,0.82),
            legend_title = element_blank(),
            legend_background = element_rect(fill = 'None'),
            legend_key = element_blank())
fig += labs(x="X", y="Y")
fig += geom_point(mapping=aes(x='x', y='fit', colour='Sname'),data=dots, size=2)
fig += geom_line(mapping=aes(x='x', y='fit', colour='Sname'),data=lin,size=0.1)
fig += ylim(12,22)
fig += scale_color_manual(values=("blue", "black"),
                          guide=guide_legend(override_aes = {"linetype":('None','solid'),"shape":('o',None)})
                          )
fig.save("figures/dotsAndLine.pdf", width=pdf_width, height=pdf_height)

################################################################################
## Figure 2: Extended Binscatter
################################################################################

## p=1, s=0
fig =  (ggplot() + ylim(12,22) +
       geom_point(data=data1, mapping=aes(x='x', y='y'), shape='o', color="lightgrey", size=1) + 
       theme_bw() +
       theme(panel_grid_major = element_blank(),
             panel_grid_minor = element_blank(),
             axis_text_x=element_blank(), axis_text_y=element_blank(),
             axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
             legend_position = "none") + labs(x="X", y="Y"))
dotsAndline = binsreg(y, x, w, at=0, nbins = 10, dots=(1,0), line = (1,0), linegrid=300, noplot=True)
dots = dotsAndline.data_plot[0].dots
line = dotsAndline.data_plot[0].line
fig = (fig + geom_line(data=data1, mapping=aes(x='x', y='y_true'), colour="darkgrey") +
             geom_point(data=dots, mapping=aes(x='x', y='fit'), color="blue", size=2, shape='o') +
             geom_line(data=line, mapping=aes(x='x', y='fit'), color="blue", size=0.5))
fig.save("figures/p1-s0.pdf", width=pdf_width, height=pdf_height)

## p=s=2
fig = (ggplot() + ylim(12,22) +
       geom_point(data=data1, mapping=aes(x='x', y='y'), shape='o', color="lightgrey", size=1) + 
       theme_bw() +
       theme(panel_grid_major = element_blank(),
             panel_grid_minor = element_blank(),
             axis_text_x=element_blank(), axis_text_y=element_blank(),
             axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
             legend_position = "none") + labs(x="X", y="Y"))
dotsAndline = binsreg(y, x, w, at=0, nbins = 10, dots=(2,2), line = (2,2), linegrid=300, noplot=True)
dots = dotsAndline.data_plot[0].dots
line = dotsAndline.data_plot[0].line
fig = (fig + geom_line(data=data1, mapping=aes(x='x', y='y_true'), colour="darkgrey") +
             geom_point(data=dots, mapping=aes(x='x', y='fit'), color="blue", size=2, shape='o') +
             geom_line(data=line, mapping=aes(x='x', y='fit'), color="blue", size=0.5))
fig.save("figures/p2-s2.pdf", width=pdf_width, height=pdf_height)


################################################################################
## Figure 3: Covariate Adjustment
################################################################################

## Case 1: x dependent on w
fig = (ggplot() +  
       theme_bw() + ylim(15,22) + 
       theme(panel_grid_major = element_blank(),
             panel_grid_minor = element_blank(),
             axis_text_x=element_blank(), axis_text_y=element_blank(),
             axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
             legend_position = (0.7,0.78), 
             legend_background = element_rect(fill = 'None'),
             legend_title = element_blank(),
             legend_key = element_blank()) + 
       labs(x="X", y="Y"))
data_more = data1.copy() 
data_more.insert(0,'Sname','true function')
fig += geom_line(data=data_more, mapping=aes(x='x', y='y_true', colour='Sname'), size=0.5, linetype='solid')

## Correct way
model = binsreg(y, x, w=w, data=data_more, at=0, nbins=10, line=(0,0), noplot=True)
dots  = model.data_plot[0].dots
dots.insert(0,'Sname','semi-linear canonical binscatter')
line = model.data_plot[0].line

fig = (fig + geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=3, shape='o') +
             geom_line(data=line, mapping=aes(x='x', y='fit'), color="blue", size=0.5))

## Incorrect way
res_y = sm.OLS(y,sm.add_constant(w)).fit().resid + np.mean(y)  # mimic the current binscatter command in STATA
res_x = sm.OLS(x,sm.add_constant(w)).fit().resid + np.mean(x)

model = binsreg(res_y, res_x, nbins=10, line=(0,0), noplot=True)
dots  = model.data_plot[0].dots
dots.insert(0,'Sname','residualized canonical binscatter')
line = model.data_plot[0].line

fig = (fig + geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=3, shape='s') +
               geom_line(data=line, mapping=aes(x='x', y='fit'), color="red", size=0.5, linetype='dashed'))

fig = (fig + scale_color_manual(values = ("darkgrey", "blue", "red"),
                                guide=guide_legend(override_aes = {'linetype' : ('solid','solid','dashed'),'shape': ('None', 'o', 's')})) +
        annotate("segment", x = 0.45, xend = np.max(res_x), y = 15, yend = 15, colour = "red", size=0.1, arrow=arrow(length=0.15)) +
        annotate("segment", x = 0.25, xend = np.min(res_x), y = 15, yend = 15, colour = "red", size=0.1, arrow=arrow(length=0.15)) +
        annotate("text", x = 0.35, y = 15, label = "supp. narrowed", color="red", size=9) + 
        annotate("segment", x = np.min(res_x), xend = np.min(res_x), y = 14.8, yend = 15.2, colour = "red", size=0.1) +
        annotate("segment", x = np.max(res_x), xend = np.max(res_x), y = 14.8, yend = 15.2, colour = "red", size=0.1))

fig.save("figures/CovAdjust_xDepw.pdf", width=pdf_width, height=pdf_height)


## Case 2: x ind of w, w of mean = 0!
fig = (ggplot() +  
       theme_bw() + ylim(15,22) + 
       theme(panel_grid_major = element_blank(),
             panel_grid_minor = element_blank(),
             axis_text_x=element_blank(), axis_text_y=element_blank(),
             axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
             legend_position = (0.7,0.78), 
             legend_background = element_rect(fill = 'None'),
             legend_title = element_blank(),
             legend_key = element_blank()) + 
       labs(x="X", y="Y"))
data_more = data1.copy() 
data_more.insert(0,'Sname','true function')
fig += geom_line(data=data_more, mapping=aes(x='x', y='y_true', colour='Sname'), size=0.5, linetype='solid')

## Correct way
model = binsreg(y_wx, x, w=w_x, data=data_more, at=0, nbins=10, line=(0,0), noplot=True)
dots  = model.data_plot[0].dots
dots.insert(0,'Sname','semi-linear canonical binscatter')
line = model.data_plot[0].line

fig = (fig + geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=3, shape='o') +
             geom_line(data=line, mapping=aes(x='x', y='fit'), color="blue", size=0.5))

## Incorrect way
res_y = sm.OLS(y_wx,sm.add_constant(w_x)).fit().resid + np.mean(y_wx)  # mimic the current binscatter command in STATA
res_x = sm.OLS(x,sm.add_constant(w_x)).fit().resid + np.mean(x)

model = binsreg(res_y, res_x, nbins=10, line=(0,0), noplot=True)
dots  = model.data_plot[0].dots
dots.insert(0,'Sname','residualized canonical binscatter')
line = model.data_plot[0].line

fig = (fig + geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=3, shape='s') +
               geom_line(data=line, mapping=aes(x='x', y='fit'), color="red", size=0.5, linetype='dashed'))

fig = (fig + scale_color_manual(values = ("darkgrey", "blue", "red"),
                                guide=guide_legend(override_aes = {'linetype' : ('solid','solid','dashed'),'shape': ('None', 'o', 's')}))) 
     
fig.save("figures/CovAdjust_xIndw.pdf", width=pdf_width, height=pdf_height)


################################################################################
## Figure 4: IMSE-optimal J
################################################################################

## small J
fig = (ggplot() + theme_bw() + ylim(12, 22) +
                  geom_point(data=data1, mapping=aes(x='x', y='y'), shape = 'o', color="lightgrey", size=1) +
                  geom_line(data=data1, mapping=aes(x='x', y='y_true'), colour="darkgrey") + 
                  theme(panel_grid_major = element_blank(),
                        panel_grid_minor = element_blank(),
                        axis_text_x=element_blank(), axis_text_y=element_blank(),
                        axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
                        legend_position = "none", legend_background = element_rect(fill = 'None')) +
                  labs(x="X", y="Y"))
dotsAndline = binsreg(y, x, w, at=0, nbins = 10, line=(0,0), noplot=True)
dots = dotsAndline.data_plot[0].dots
line = dotsAndline.data_plot[0].line

fig = (fig + geom_point(data=dots, mapping=aes(x='x', y='fit'), color="blue", size=2, shape='o') +
             geom_line(data=line, mapping=aes(x='x', y='fit'), color="blue", size=0.5))
fig.save("figures/regressogram_Jsmall.pdf", width=pdf_width, height=pdf_height)

## IMSE optimal J
J_opt = binsregselect(y, x, w).nbinsdpi
fig = (ggplot() + theme_bw() + ylim(12, 22) +
                  geom_point(data=data1, mapping=aes(x='x', y='y'), shape='o', color="lightgrey", size=1) +
                  geom_line(data=data1, mapping=aes(x='x', y='y_true'), colour="darkgrey") + 
                  theme(panel_grid_major = element_blank(),
                        panel_grid_minor = element_blank(),
                        axis_text_x=element_blank(), axis_text_y=element_blank(),
                        axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
                        legend_position = "none", legend_background = element_rect(fill = 'None')) +
                  labs(x="X", y="Y"))
dotsAndline = binsreg(y, x, w, at=0, nbins = J_opt, line=(0,0), noplot=True)
dots = dotsAndline.data_plot[0].dots
line = dotsAndline.data_plot[0].line

fig = (fig + geom_point(data=dots, mapping=aes(x='x', y='fit'), color="blue", size=2, shape='o') +
             geom_line(data=line, mapping=aes(x='x', y='fit'), color="blue", size=0.5))
fig.save("figures/regressogram_Jopt.pdf", width=pdf_width, height=pdf_height)


################################################################################
##  Figure 5: Uncertainty Quantification
################################################################################

fig = ggplot() +  theme_bw() + ylim(12,23)
fig = (fig + geom_point(data=data1, mapping=aes(x='x', y='y'), shape='o', color="lightgrey") + 
             theme(panel_grid_major = element_blank(),
                   panel_grid_minor = element_blank(),
                   axis_text_x=element_blank(), axis_text_y=element_blank(),
                   axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
                   legend_position="none") + labs(x="X", y="Y"))
model = binsreg(y, x, w, at=0, line = (0,0), ci=(1,1), cb=(1,1), noplot=True)
dots = model.data_plot[0].dots
line = model.data_plot[0].line
ci = model.data_plot[0].ci
cb = model.data_plot[0].cb
fig = (fig + geom_point(data=dots, mapping=aes(x='x', y='fit'), color="blue", size=2, shape='o') +
             geom_line(data=line, mapping=aes(x='x', y='fit'), color="blue", size=0.5) +
             geom_errorbar(data=ci, mapping=aes(x='x', ymin='ci_l', ymax='ci_r'), color="blue", size=0.5, width = 0.02, linetype='solid') +
             geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), fill="blue", alpha=0.2))
fig.save("figures/inference_hsk1_p0s0.pdf", width=pdf_width, height=pdf_height)

fig = ggplot() +  theme_bw() + ylim(12,23)
fig = (fig + geom_point(data=data1, mapping=aes(x='x', y='y'), shape='o', color="lightgrey") +
             theme(panel_grid_major = element_blank(),
                   panel_grid_minor = element_blank(),
                   axis_text_x=element_blank(), axis_text_y=element_blank(),
                   axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
                   legend_position="none") + labs(x="X", y="Y"))
model = binsreg(y, x, w, at=0, dots=(2,2), line = (2,2), ci=(3,3), cb=(3,3),noplot=True)
dots = model.data_plot[0].dots
line = model.data_plot[0].line
ci = model.data_plot[0].ci
cb = model.data_plot[0].cb
fig = (fig + geom_point(data=dots, mapping=aes(x='x', y='fit'), color="blue", size=2, shape='o') +
             geom_line(data=line, mapping=aes(x='x', y='fit'), color="blue", size=0.5) +
             geom_errorbar(data=ci, mapping=aes(x='x', ymin='ci_l', ymax='ci_r'), color="blue", size=0.5, linetype='solid', width=0.02) +
             geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), fill="blue", alpha=0.2))
fig.save("figures/inference_hsk1_p2s2.pdf", width=pdf_width, height=pdf_height)


################################################################################
## Figure 6: Hypothesis Testing -- Parametric Specification
################################################################################

## p=s=0
est_bin = binsreg(y, x, w, at=0, dots=(0,0), line = (0,0), cb=(1,1), polyreg = 1, noplot=True)
dots    = est_bin.data_plot[0].dots
dots.insert(0,'Sname','binscatter')
line    = est_bin.data_plot[0].line
line.insert(0,'Sname','binscatter')
cb      = est_bin.data_plot[0].cb
lfit    = est_bin.data_plot[0].poly
lfit.insert(0,'Sname','linear')
est_bin = binsreg(y, x, w, at=0, polyreg = 2, noplot=True)
qfit    = est_bin.data_plot[0].poly
qfit.insert(0,'Sname','quadratic')
est_bin = binsreg(y, x, w, at=0, polyreg = 0, noplot=True)
const   = est_bin.data_plot[0].poly
const.insert(0,'Sname','constant')

fig = (ggplot() + theme_bw() + ylim(12,23) + 
       theme(panel_grid_major = element_blank(),
             panel_grid_minor = element_blank(),
             axis_text_x=element_blank(), axis_text_y=element_blank(),
             axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
             legend_position = (0.8,0.75), 
             legend_background = element_rect(fill = 'None'),
             legend_title = element_blank(),
             legend_key = element_blank()) + 
       labs(x="X", y="Y"))

fig += geom_point(data=data1, mapping=aes(x='x', y='y'), shape='o', color="lightgrey", size=1)
fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2)
fig += geom_line(data=const, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1, linetype='dashed')
fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2)
fig += geom_line(data=line, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1)
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), alpha=0.2, fill="blue")
fig += geom_line(data=lfit, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1)
fig += geom_line(data=qfit, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1, linetype='dotted')

fig += scale_color_manual(values = ("blue", "grey", "black", "black"),
                                guide=guide_legend(override_aes = {"linetype":('solid','dashed','solid','dotted'), "shape":('o', 'None', 'None', 'None')}))
fig.save("figures/testspec_p0s0.pdf", width=pdf_width, height=pdf_height)


## p=s=2
est_bin = binsreg(y, x, w, at=0, dots=(2,2), line = (2,2), cb=(3,3), polyreg = 1, noplot=True)
dots    = est_bin.data_plot[0].dots
dots.insert(0,'Sname','binscatter')
line    = est_bin.data_plot[0].line
line.insert(0,'Sname','binscatter')
cb      = est_bin.data_plot[0].cb
lfit    = est_bin.data_plot[0].poly
lfit.insert(0,'Sname','linear')
est_bin = binsreg(y, x, w, at=0, polyreg = 2, noplot=True)
qfit    = est_bin.data_plot[0].poly
qfit.insert(0,'Sname','quadratic')
est_bin = binsreg(y, x, w, at=0, polyreg = 0, noplot=True)
const   = est_bin.data_plot[0].poly
const.insert(0,'Sname','constant')

fig = (ggplot() + theme_bw() + ylim(12,23) + 
       theme(panel_grid_major = element_blank(),
             panel_grid_minor = element_blank(),
             axis_text_x=element_blank(), axis_text_y=element_blank(),
             axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
             legend_position = (0.8,0.75), 
             legend_background = element_rect(fill = 'None'),
             legend_title = element_blank(),
             legend_key = element_blank()) + 
       labs(x="X", y="Y"))

fig += geom_point(data=data1, mapping=aes(x='x', y='y'), shape='o', color="lightgrey", size=1)
fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2)
fig += geom_line(data=const, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1, linetype='dashed')
fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2)
fig += geom_line(data=line, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1)
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), alpha=0.2, fill="blue")
fig += geom_line(data=lfit, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1)
fig += geom_line(data=qfit, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1, linetype='dotted')

fig += scale_color_manual(values = ("blue", "grey", "black", "black"),
                                guide=guide_legend(override_aes = {"linetype":('solid','dashed','solid','dotted'), "shape":('o', 'None', 'None', 'None')}))
fig.save("figures/testspec_p2s2.pdf", width=pdf_width, height=pdf_height)


################################################################################
## Table 1: Hypothesis Testing -- Parametric Specification
################################################################################
result_test = np.empty((6,6))
result_test[:] = np.nan

## p=s=0
## constant?
est = binstest(y, x, w=w, at=0, bins = (0,0), testmodel=(1,1), testmodelpoly=0)
result_test[0,:3] = np.concatenate((est.testpoly.stat, est.testpoly.pval, [est.options.nbins]))
est = binstest(y, x, w=w, at=0, bins = (0,0), testmodel=(1,1), testmodelpoly=0, lp=2)
result_test[0,3:6] = np.concatenate((est.testpoly.stat, est.testpoly.pval, [est.options.nbins]))

## linear?
est = binstest(y, x, w=w, at=0, bins = (0,0), testmodel=(1,1), testmodelpoly=1)
result_test[1,:3] = np.concatenate((est.testpoly.stat, est.testpoly.pval, [est.options.nbins]))
est = binstest(y, x, w=w, at=0, bins = (0,0), testmodel=(1,1), testmodelpoly=1, lp=2)
result_test[1,3:6] = np.concatenate((est.testpoly.stat, est.testpoly.pval, [est.options.nbins]))

## Quadratic?
est = binstest(y, x, w=w, at=0, bins = (0,0), testmodel=(1,1), testmodelpoly=2)
result_test[2,:3] = np.concatenate((est.testpoly.stat, est.testpoly.pval, [est.options.nbins]))
est = binstest(y, x, w=w, at=0, bins = (0,0), testmodel=(1,1), testmodelpoly=2, lp=2)
result_test[2,3:6] = np.concatenate((est.testpoly.stat, est.testpoly.pval, [est.options.nbins]))

## p=s=2
## constant?
est = binstest(y, x, w=w, at=0, bins = (2,2), testmodel=(1,1), testmodelpoly=0)
result_test[3,:3] = np.concatenate((est.testpoly.stat, est.testpoly.pval, [est.options.nbins]))
est = binstest(y, x, w=w, at=0, bins = (2,2), testmodel=(1,1), testmodelpoly=0, lp=2)
result_test[3,3:6] = np.concatenate((est.testpoly.stat, est.testpoly.pval, [est.options.nbins]))

## linear?
est = binstest(y, x, w=w, at=0, bins = (2,2), testmodel=(1,1), testmodelpoly=1)
result_test[4,:3] = np.concatenate((est.testpoly.stat, est.testpoly.pval, [est.options.nbins]))
est = binstest(y, x, w=w, at=0, bins = (2,2), testmodel=(1,1), testmodelpoly=1, lp=2)
result_test[4,3:6] = np.concatenate((est.testpoly.stat, est.testpoly.pval, [est.options.nbins]))

## Quadratic?
est = binstest(y, x, w=w, at=0, bins = (2,2), testmodel=(1,1), testmodelpoly=2)
result_test[5,:3] = np.concatenate((est.testpoly.stat, est.testpoly.pval, [est.options.nbins]))
est = binstest(y, x, w=w, at=0, bins = (2,2), testmodel=(1,1), testmodelpoly=2, lp=2)
result_test[5,3:6] = np.concatenate((est.testpoly.stat, est.testpoly.pval, [est.options.nbins]))

tab1 = result_test.copy()

# spec.test <- cbind(format(round(result.test[,c(1:2,4:5)], 2), nsmall=2), result.test[,6])

# n.cgroup <- c(2,2,1)
# cgroup   <- c("Sup norm", "$L_2$ norm", "")
# colheads <- c(rep(c("Test Statistic", "P-value"), 2), "$\\hat{J}_{\\texttt{IMSE}}$")
# rowname <- c("Constant", "Linear", "Quadratic") 
# #             "Negativity", "Decreasingness", "Concavity")
# n.rgroup <- c(3,3)
# rgroup <- c("Canonical: $p=s=0$", "Extended: $p=s=2$")

# latex(spec.test, file=paste("figures/testing-spec", ".txt", sep = ""), 
#       append=FALSE, table.env=FALSE, center="none", title="",
#       n.cgroup=n.cgroup, cgroup=cgroup, colheads=colheads,
#       n.rgroup=n.rgroup, rgroup=rgroup, rowname=rowname, rowlabel="Binscatter")


################################################################################
## Table 2: Hypothesis Testing -- Shape Restriction
################################################################################
result_test = np.empty((6,3))
result_test[:] = np.nan

## p=s=0
## negativity: <=0?
est = binstest(y, x, w=w, at=0, deriv=0, testshapel=0, bins=(0,0), testshape=(1,1))
result_test[0,:3] = np.concatenate((est.testshapeL.stat, est.testshapeL.pval, [est.options.nbins]))

## p=s=1
## negativity: <=0 ?
est = binstest(y, x, w=w, at=0, deriv=0, testshapel=0, bins=(1,1), testshape=(2,2))
result_test[1,0:3] = np.concatenate((est.testshapeL.stat, est.testshapeL.pval, [est.options.nbins]))
## Decreasing: first-order deriv <= 0 ?
est = binstest(y, x, w=w, at=0, deriv=1, testshapel=0, bins=(1,1), testshape=(2,2))
result_test[2,0:3] = np.concatenate((est.testshapeL.stat, est.testshapeL.pval, [est.options.nbins]))

## p=s=2
## negativity: <=0?
est = binstest(y, x, w=w, at=0, deriv=0, testshapel=0, bins=(2,2), testshape=(3,3))
result_test[3,0:3] = np.concatenate((est.testshapeL.stat, est.testshapeL.pval, [est.options.nbins]))
## Decreasing: first-order deriv <= 0 ?
est = binstest(y, x, w=w, at=0, deriv=1, testshapel=0, bins=(2,2), testshape=(3,3))
result_test[4,0:3] = np.concatenate((est.testshapeL.stat, est.testshapeL.pval, [est.options.nbins]))
## Concavity: second-order deriv <= 0 ?
est = binstest(y, x, w=w, at=0, deriv=2, testshapel=0, bins=(2,2), testshape=(3,3))
result_test[5,0:3] = np.concatenate((est.testshapeL.stat, est.testshapeL.pval, [est.options.nbins]))

tab2 = result_test.copy()

# shape.test = cbind(format(round(result_test[,1:2], 2), nsmall=2), result_test[,3])
# n.cgroup = (3)
# cgroup = NULL
# colheads = rep(("Test Statistic", "P-value", ".\\hat{J}_{\\texttt{IMSE}}."), 1)
# rowname = ("Negativity", ("Negativity", "Decreasingness"), ("Negativity", "Decreasingness", "Concavity"))
# n.rgroup = (1,2,3)
# rgroup = ("Canonical: .p=s=0.", "Extended: .p=s=1.", "Extended: .p=s=2.")

# latex(shape.test, file=paste("figures/testing-shape", ".txt", sep = ""), 
#       append=FALSE, table.env=FALSE, center="none", title="",
#       n.cgroup=n.cgroup, cgroup=cgroup, colheads=colheads,
#       n.rgroup=n.rgroup, rgroup=rgroup, rowname=rowname, rowlabel="\\bf{Binscatter}")


################################################################################
## Figure 7: Two-group Comparison
################################################################################

## Graphical illustration
fig =  (ggplot() +  theme_bw() + ylim(13,22) + 
        geom_vline(xintercept=0.22, colour="black", linetype="dashed", size=0.2) +
        geom_vline(xintercept=0.25, colour="black", linetype="dashed", size=0.2) + 
        geom_rect(mapping=aes(xmin=0.22, xmax=0.25, ymin=-np.inf, ymax=np.inf), linetype="dashed", fill="lightgrey", alpha=0.3) +
        theme(panel_grid_major = element_blank(),
              panel_grid_minor = element_blank(),
              axis_text_x=element_blank(), axis_text_y=element_blank(),
              axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
              legend_position = (0.8,0.8),
              legend_title = element_blank(),
              legend_key = element_blank(), 
              legend_background = element_rect(fill = 'None')) + 
        labs(x="X", y="Y"))
model = binsreg(y, x, w, at=0, line=(0,0), cb=(1,1), noplot=True)
dots = model.data_plot[0].dots
dots.insert(0,'Sname', "Group 0")
line = model.data_plot[0].line
line.insert(0,'Sname',"Group 0")
cb = model.data_plot[0].cb

fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2, shape='o')
fig += geom_line(data=line, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1, linetype='solid')
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), alpha=0.2, fill="blue")

model = binsreg(y2, x2, w2, at=0, line=(0,0), cb=(1,1), noplot=True)
dots = model.data_plot[0].dots
dots.insert(0,'Sname', "Group 1")
line = model.data_plot[0].line
line.insert(0,'Sname',"Group 1")
cb = model.data_plot[0].cb

fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2, shape='s')
fig += geom_line(data=line, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1, linetype='dashed')
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), alpha=0.2, fill="green")

fig += scale_color_manual(name="", values = ("blue", "green"),
                                guide=guide_legend(override_aes = {'linetype':('solid','dashed'), 'shape':('o','s')}))
fig.save("figures/groupDiff_p0s0.pdf", width=pdf_width, height=pdf_height)

# p = 2 and s = 2
fig =  (ggplot() +  theme_bw() + ylim(13,22) + 
        geom_vline(xintercept=0.22, colour="black", linetype="dashed", size=0.2) +
        geom_vline(xintercept=0.25, colour="black", linetype="dashed", size=0.2) + 
        geom_rect(mapping=aes(xmin=0.22, xmax=0.25, ymin=-np.inf, ymax=np.inf), linetype="dashed", fill="lightgrey", alpha=0.3) +
        theme(panel_grid_major = element_blank(),
              panel_grid_minor = element_blank(),
              axis_text_x=element_blank(), axis_text_y=element_blank(),
              axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
              legend_position = (0.8,0.8),
              legend_title = element_blank(),
              legend_key = element_blank(), 
              legend_background = element_rect(fill = 'None')) + 
        labs(x="X", y="Y"))
model = binsreg(y, x, w, at=0, dots=(2,2), line=(2,2), cb=(3,3), noplot=True)
dots = model.data_plot[0].dots
dots.insert(0,'Sname', "Group 0")
line = model.data_plot[0].line
line.insert(0,'Sname',"Group 0")
cb = model.data_plot[0].cb

fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2, shape='o')
fig += geom_line(data=line, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1, linetype='solid')
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), alpha=0.2, fill="blue")

model = binsreg(y2, x2, w2, at=0, dots=(2,2), line=(2,2), cb=(3,3), noplot=True)
dots = model.data_plot[0].dots
dots.insert(0,'Sname', "Group 1")
line = model.data_plot[0].line
line.insert(0,'Sname',"Group 1")
cb = model.data_plot[0].cb

fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2, shape='s')
fig += geom_line(data=line, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1, linetype='dashed')
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), alpha=0.2, fill="green")

fig += scale_color_manual(name="", values = ("blue", "green"),
                                guide=guide_legend(override_aes = {'linetype':('solid','dashed'), 'shape':('o','s')}))
fig.save("figures/groupDiff_p2s2.pdf", width=pdf_width, height=pdf_height)


################################################################################
## Table 3: Two-group Comparison
################################################################################
pwc_test = np.empty((4,4))
pwc_test[:]=np.nan
index = (data.x>=0.22) & (data.x <=0.25)
data.insert(data.shape[1],'subset',index)

## p=s=0
est = binspwc('y', 'x', w='w', at=0, by='t', data=data, bins=(0,0), pwc=(1,1), subset='subset')
pwc_test[0,:4] = np.concatenate(([est.tstat[0][0]], est.pval[0], est.options.nbins_by))
est = binspwc('y', 'x', w='w', at=0, by='t', data=data, bins=(0,0), pwc=(1,1))
pwc_test[1,:4] = np.concatenate(([est.tstat[0][0]], est.pval[0], est.options.nbins_by))

## p=s=2
est = binspwc('y', 'x', w='w', at=0, by='t', data=data, bins=(2,2), pwc=(3,3), subset='subset')
pwc_test[2,:4] = np.concatenate(([est.tstat[0][0]], est.pval[0], est.options.nbins_by))
est = binspwc('y', 'x', w='w', at=0, by='t', data=data, bins=(2,2), pwc=(3,3))
pwc_test[3,:4] = np.concatenate(([est.tstat[0][0]], est.pval[0], est.options.nbins_by))

tab3 = pwc_test.copy()

# pwc_test = cbind(format(round(pwc_test[,1:2], 2), nsmall=2), pwc_test[,3:4])

# ## make table
# n.cgroup = (4)
# cgroup = NULL
# colheads = rep(("Test Statistic", "P-value", ".\\hat{J}_{\\texttt{IMSE},0}.", ".\\hat{J}_{\\texttt{IMSE},1}."), 1)
# rowname  = rep((".0.22\\leq x\\leq 0.25.", "full sample"),2)
# n.rgroup = (2,2)
# rgroup   = ("Canonical: .p=s=0.", "Extended: .p=s=2.")

# latex(pwc_test, file=paste("figures/testing-2sample", ".txt", sep = ""), 
#       append=FALSE, table.env=FALSE, center="none", title="",
#       n.cgroup=n.cgroup, cgroup=cgroup, colheads=colheads,
#       n.rgroup=n.rgroup, rgroup=rgroup, rowname=rowname, rowlabel="\\bf{Binscatter}")



################################################################################
## Figure 8: Generalized Non-linear Binscatter
################################################################################

############################################################
## Row 1: Quantile Regression
############################################################

## p=s=0
fig = ggplot() + theme_bw() + ylim(12,23)
fig += geom_point(data=data1, mapping=aes(x='x', y='y'), shape='o', color="lightgrey")
fig += geom_vline(xintercept=0.12, colour="black", linetype="dashed", size=0.2)
fig += geom_vline(xintercept=0.5, colour="black", linetype="dashed", size=0.2)
fig += geom_rect(mapping=aes(xmin=0.12, xmax=0.5, ymin=-np.inf, ymax=np.inf), linetype="dashed", fill="lightgrey", alpha=0.3)
fig += theme(panel_grid_major = element_blank(),
            panel_grid_minor = element_blank(),
            axis_text_x=element_blank(), axis_text_y=element_blank(),
            axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
            legend_position=(0.8,0.8),
            legend_background = element_rect(fill = 'None'),
            legend_title=element_blank(),
            legend_key=element_blank())
fig += labs(x="X", y="Y")

model = binsqreg(y, x, w, at=0, quantile=0.25, line = (0,0), ci=(1,1), cb=(1,1),noplot=True)
dots = model.data_plot[0].dots
dots.insert(0,'Sname', "0.25 quantile")
line = model.data_plot[0].line
line.insert(0,'Sname', "0.25 quantile")
ci = model.data_plot[0].ci
cb = model.data_plot[0].cb
fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2, shape='o')
fig += geom_line(data=line, mapping=aes(x='x', y='fit', colour='Sname'), size=0.5, linetype='solid')
fig += geom_errorbar(data=ci, mapping=aes(x='x', ymin='ci_l', ymax='ci_r'), color="blue", size=0.5, width = 0.02, linetype='solid')
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), fill="blue", alpha=0.2)

model = binsqreg(y, x, w, at=0, quantile=0.75, line = (0,0), ci=(1,1), cb=(1,1),noplot=True)
dots = model.data_plot[0].dots
dots.insert(0,'Sname', "0.75 quantile")
line = model.data_plot[0].line
line.insert(0,'Sname', "0.75 quantile")
ci = model.data_plot[0].ci
cb = model.data_plot[0].cb
fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2, shape='o')
fig += geom_line(data=line, mapping=aes(x='x', y='fit', colour='Sname'), size=0.5, linetype='dashed')
fig += geom_errorbar(data=ci, mapping=aes(x='x', ymin='ci_l', ymax='ci_r'), color="green", size=0.5, width = 0.02, linetype='dashed')
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), fill="green", alpha=0.2)
fig += scale_color_manual(values = ("blue", "green"),
                          guide=guide_legend(override_aes = {'linetype':('solid','dashed'), 'shape':('o', 's')}))
fig.save("figures/QR_p0s0.pdf", width=pdf_width, height=pdf_height)

## p=s=2
fig = ggplot() + theme_bw() + ylim(12,23)
fig += geom_point(data=data1, mapping=aes(x='x', y='y'), shape='o', color="lightgrey")
fig += geom_vline(xintercept=0.12, colour="black", linetype="dashed", size=0.2)
fig += geom_vline(xintercept=0.5, colour="black", linetype="dashed", size=0.2)
fig += geom_rect(mapping=aes(xmin=0.12, xmax=0.5, ymin=-np.inf, ymax=np.inf), linetype="dashed", fill="lightgrey", alpha=0.3)
fig += theme(panel_grid_major = element_blank(),
            panel_grid_minor = element_blank(),
            axis_text_x=element_blank(), axis_text_y=element_blank(),
            axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
            legend_position=(0.8,0.8),
            legend_background = element_rect(fill = 'None'),
            legend_title=element_blank(),
            legend_key=element_blank())
fig += labs(x="X", y="Y")

model = binsqreg(y, x, w, at=0, quantile=0.25, dots=(2,2), line = (2,2), ci=(3,3), cb=(3,3),noplot=True)
dots = model.data_plot[0].dots
dots.insert(0,'Sname', "0.25 quantile")
line = model.data_plot[0].line
line.insert(0,'Sname', "0.25 quantile")
ci = model.data_plot[0].ci
cb = model.data_plot[0].cb
fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2, shape='o')
fig += geom_line(data=line, mapping=aes(x='x', y='fit', colour='Sname'), size=0.5, linetype='solid')
fig += geom_errorbar(data=ci, mapping=aes(x='x', ymin='ci_l', ymax='ci_r'), color="blue", size=0.5, width = 0.02, linetype='solid')
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), fill="blue", alpha=0.2)

model = binsqreg(y, x, w, at=0, quantile=0.75, dots=(2,2), line = (2,2), ci=(3,3), cb=(3,3),noplot=True)
dots = model.data_plot[0].dots
dots.insert(0,'Sname', "0.75 quantile")
line = model.data_plot[0].line
line.insert(0,'Sname', "0.75 quantile")
ci = model.data_plot[0].ci
cb = model.data_plot[0].cb
fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2, shape='o')
fig += geom_line(data=line, mapping=aes(x='x', y='fit', colour='Sname'), size=0.5, linetype='dashed')
fig += geom_errorbar(data=ci, mapping=aes(x='x', ymin='ci_l', ymax='ci_r'), color="green", size=0.5, width = 0.02, linetype='dashed')
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), fill="green", alpha=0.2)
fig += scale_color_manual(values = ("blue", "green"),
                          guide=guide_legend(override_aes = {'linetype':('solid','dashed'), 'shape':('o', 's')}))
fig.save("figures/QR_p2s2.pdf", width=pdf_width, height=pdf_height)


############################################################
## Row 2: Logistic Regression
############################################################

## Define a binary response
d = 1*(y<=np.quantile(y,1/3))
subset = (x>=0.12) & (x<=0.5)

## p=s=0
fig = ggplot() + theme_bw()
fig += theme(panel_grid_major = element_blank(),
            panel_grid_minor = element_blank(),
            axis_text_x=element_blank(), axis_text_y=element_blank(),
            axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank())
fig += labs(x="X", y="Y")
model = binsglm(d, x, w, at=0, dist='Binomial', line = (0,0), ci=(1,1), cb=(1,1), subset=subset, noplot = True)
dots  = model.data_plot[0].dots
line  = model.data_plot[0].line
ci    = model.data_plot[0].ci
cb    = model.data_plot[0].cb
fig += geom_point(data=dots, mapping=aes(x='x', y='fit'), color="blue", size=2, shape='o')
fig += geom_line(data=line, mapping=aes(x='x', y='fit'), color="blue", size=0.5, linetype='solid')
fig += geom_errorbar(data=ci, mapping=aes(x='x', ymin='ci_l', ymax='ci_r'), color="blue", size=0.5, linetype='solid', width=0.01)
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), fill="blue", alpha=0.2)
fig.save("figures/Logit_p0s0.pdf", width=pdf_width, height=pdf_height)

## p=s=2
fig = ggplot() + theme_bw()
fig += theme(panel_grid_major = element_blank(),
            panel_grid_minor = element_blank(),
            axis_text_x=element_blank(), axis_text_y=element_blank(),
            axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank())
fig += labs(x="X", y="Y")
model = binsglm(d, x, w, at=0, dist='Binomial', dots=(2,2), line=(2,2), ci=(3,3), cb=(3,3), subset=subset, noplot = True)
dots  = model.data_plot[0].dots
line  = model.data_plot[0].line
ci    = model.data_plot[0].ci
cb    = model.data_plot[0].cb
fig += geom_point(data=dots, mapping=aes(x='x', y='fit'), color="blue", size=2, shape='o')
fig += geom_line(data=line, mapping=aes(x='x', y='fit'), color="blue", size=0.5, linetype='solid')
fig += geom_errorbar(data=ci, mapping=aes(x='x', ymin='ci_l', ymax='ci_r'), color="blue", size=0.5, linetype='solid', width=0.01)
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), fill="blue", alpha=0.2)
fig.save("figures/Logit_p2s2.pdf", width=pdf_width, height=pdf_height)


############################################################
## Row 3: Quantile Regression -- Two-group comparison
############################################################

# p = s = 0
fig =  ggplot() +  theme_bw() + ylim(12,23)
fig += theme(panel_grid_major = element_blank(),
              panel_grid_minor = element_blank(),
              axis_text_x=element_blank(), axis_text_y=element_blank(),
              axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
              legend_position = (0.8,0.8), 
              legend_background = element_rect(fill = 'None'),
              legend_title = element_blank(),
              legend_key = element_blank())
fig +=labs(x="X", y="Y")

model = binsqreg(y, x, w, at=0, quantile=0.75, line=(0,0), cb=(1,1), noplot=True)
dots = model.data_plot[0].dots
dots.insert(0,'Sname',"Group 0")
line = model.data_plot[0].line
line.insert(0,'Sname',"Group 0")
cb = model.data_plot[0].cb

fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2, shape='o')
fig += geom_line(data=line, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1, linetype='solid')
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), alpha=0.2, fill="blue")

model = binsqreg(y2, x2, w2, at=0, quantile=0.75, line=(0,0), cb=(1,1), noplot=True)
dots = model.data_plot[0].dots
dots.insert(0,'Sname',"Group 1")
line = model.data_plot[0].line
line.insert(0,'Sname',"Group 1")
cb = model.data_plot[0].cb

fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2, shape='s')
fig += geom_line(data=line, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1, linetype='dashed')
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), alpha=0.2, fill="green")

fig += scale_color_manual(values = ("blue", "green"),
                          guide=guide_legend(override_aes = {'linetype':('solid','dashed'), 'shape':('o', 's')}))
fig.save("figures/QR_groupDiff_p0s0.pdf", width=pdf_width, height=pdf_height)

# p = s = 2
fig =  ggplot() +  theme_bw() + ylim(12,23)
fig += theme(panel_grid_major = element_blank(),
              panel_grid_minor = element_blank(),
              axis_text_x=element_blank(), axis_text_y=element_blank(),
              axis_ticks_major_x=element_blank(), axis_ticks_major_y=element_blank(),
              legend_position = (0.8,0.8), 
              legend_background = element_rect(fill = 'None'),
              legend_title = element_blank(),
              legend_key = element_blank())
fig +=labs(x="X", y="Y")

model = binsqreg(y, x, w, at=0, quantile=0.75, dots=(2,2), line=(2,2), cb=(2,2), noplot=True)
dots = model.data_plot[0].dots
dots.insert(0,'Sname',"Group 0")
line = model.data_plot[0].line
line.insert(0,'Sname',"Group 0")
cb = model.data_plot[0].cb

fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2, shape='o')
fig += geom_line(data=line, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1, linetype='solid')
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), alpha=0.2, fill="blue")

model = binsqreg(y2, x2, w2, at=0, quantile=0.75, dots=(2,2), line=(2,2), cb=(2,2), noplot=True)
dots = model.data_plot[0].dots
dots.insert(0,'Sname',"Group 1")
line = model.data_plot[0].line
line.insert(0,'Sname',"Group 1")
cb = model.data_plot[0].cb

fig += geom_point(data=dots, mapping=aes(x='x', y='fit', colour='Sname'), size=2, shape='s')
fig += geom_line(data=line, mapping=aes(x='x', y='fit', colour='Sname'), size=0.1, linetype='dashed')
fig += geom_ribbon(data=cb, mapping=aes(x='x', ymin='cb_l', ymax='cb_r'), alpha=0.2, fill="green")

fig += scale_color_manual(values = ("blue", "green"),
                          guide=guide_legend(override_aes = {'linetype':('solid','dashed'), 'shape':('o', 's')}))
fig.save("figures/QR_groupDiff_p2s2.pdf", width=pdf_width, height=pdf_height)
