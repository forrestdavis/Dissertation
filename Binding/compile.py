import os
import sys
import pandas as pd

#### Set up the system interface
sys.path.append(os.path.join("..", "ModelsAPI"))
from src.experiments.TSE import TSE

def flatten_dataframe(dataframe, fname_base):

    columns = list(filter(lambda x: '_surp' in x, dataframe.columns.to_list()))
    base_columns = list(filter(lambda x: '_surp' not in x, dataframe.columns.to_list()))

    lstms = list(filter(lambda x: 'lstm' in x.lower(), columns))
    non_lstms = list(filter(lambda x: 'lstm' not in x.lower(), columns))

    #Filter down data for melt
    plot_data = dataframe[base_columns+lstms+non_lstms]
    #Get row-wise average of lstms
    plot_data['lstm avg_surp'] = dataframe[lstms].mean(axis=1)
    #pivot around model names
    plot_data = pd.melt(plot_data, 
            id_vars=base_columns, var_name='model', value_name='surp')

    #rename model names to something sensible
    plot_data['model'] = list(map(lambda x: x.split('/')[-1].replace('_surp', ''), plot_data['model'].to_list()))

    print(f"Saving compiled data to {fname_base}_combined_flat.csv")
    plot_data.to_csv(f"{fname_base}_combined_flat.csv", index=False)

    return plot_data

if __name__ == "__main__":

    ################
    # Combine results
    ################

    for exp_name in ["Chow_1", "Nicol", 'vanGompel_1', 'Kush_Dillon_1', 'Kush_Dillon_2']:

        fnames = []
        if exp_name == "Chow_1":
            fname_base = 'results/Chow_Exp1'

        elif exp_name == "Nicol":
            fname_base = 'results/Nicol_Exp'

        elif exp_name == 'vanGompel_1':
            fname_base = 'results/vanGompel_Liversedge_Exp1'

        elif exp_name == 'Kush_Dillon_1':
            fname_base = 'results/Kush_Dillon_Exp1'

        elif exp_name == 'Kush_Dillon_2':
            fname_base = 'results/Kush_Dillon_Exp2'

        exp = TSE(exp_name)

        #Add bert
        fnames.append(fname_base+'_bert.csv')
        #Add gpt2/tfxl
        fnames.append(fname_base+'_auto.csv')
        #Add lstm
        fnames.append(fname_base+'_lstm.csv')

        #Compile results
        exp.combine_precompiled_experiments(fnames)
        dataframe = flatten_dataframe(exp.dataframe, fname_base)
