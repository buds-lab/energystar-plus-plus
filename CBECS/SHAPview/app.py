import pandas as pd
import numpy as np

from xgboost import XGBRegressor
import shap

import matplotlib
matplotlib.use("Agg", force = True)

import matplotlib.pyplot as plt

x = pd.read_csv('office_all_x.csv')
y = pd.read_csv('office_all_y.csv')


## exclude all interaction terms
cols = [col for col in x.columns if ':' not in col]
x = x.filter(cols)


x = x.replace({'IsBank': {'Yes': True, 'No': False}})
x.drop(['SOURCE_EUI', 'SourceEnergy', 'FINALWT',  'SourceEnergyLog'], axis=1, inplace=True)
x = x.round(1)

#x.SqFt = np.log(x.SqFt)
#x.ComputersCnt = np.log(x.ComputersCnt)

  
xgb1 = XGBRegressor(max_depth = 2)
# Add silent=True to avoid printing out updates with each cycle
xgb1.fit(x, y, verbose=False)    

shap.initjs()

treeExplainer = shap.TreeExplainer(xgb1)
shap_values = treeExplainer.shap_values(x)
shap_values = np.round(shap_values, 2)


def get_force_plot(ix):
    plt1 = shap.force_plot(treeExplainer.expected_value, 
                       shap_values[ix,:], 
                       x.iloc[ix,:], 
                       matplotlib=False, 
                       #plot_cmap='PkYg',
                       text_rotation=40, show=False,  figsize=(18, 3))
    return(plt1)
    
def get_force_plot_all():
    plt2 = shap.force_plot(treeExplainer.expected_value, 
                           shap_values, 
                           x,
                           matplotlib=False,
                           show=False)

    return(plt2)

def get_summary_plot():
    plt3 = shap.summary_plot(shap_values, x, show=False)
    return(plt3)    