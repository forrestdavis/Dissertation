import os
import sys
import configparser
import yaml
import pandas as pd

#### Set up the system interface
sys.path.append(os.path.join("..", "ModelsAPI"))
from src.experiments.TSE import TSE

def flatten_dataframe(dataframe, fname_base, expName):

    columns = list(filter(lambda x: '_prob' in x, dataframe.columns.to_list()))
    base_columns = list(filter(lambda x: '_prob' not in x, dataframe.columns.to_list()))

    lstms = list(filter(lambda x: 'lstm' in x.lower(), columns))
    non_lstms = list(filter(lambda x: 'lstm' not in x.lower(), columns))

    #Filter down data for melt
    plot_data = dataframe[base_columns+lstms+non_lstms]
    #Get row-wise average of lstms
    plot_data['lstm avg_prob'] = dataframe[lstms].mean(axis=1)
    #pivot around model names
    plot_data = pd.melt(plot_data,
            id_vars=base_columns, var_name='model', value_name='prob')

    #rename model names to something sensible
    plot_data['model'] = list(map(lambda x: x.split('/')[-1].replace('_prob', ''), plot_data['model'].to_list()))

    if expName != 'Gilboy':

        #Filter out ambiguous stuff
        plot_data = plot_data[plot_data['AttachType']!='ambi']

        print(f"Saving compiled data to {fname_base}_combined_flat.csv")
        plot_data.to_csv(f"{fname_base}_combined_flat.csv", index=False)

        return plot_data

    ####
    #For Gilboy experiments need to get N2 Proporition per stimuli group
    ####
    #filter out ambiguous attachment
    ambi_data = plot_data[(plot_data['AttachType'] == 'ambi') & (plot_data['verbNum'] == 'sg')]
    #Get by item by model scores of N2 preference
    exp_types = plot_data['expType'].unique().tolist()

    contrasts = set([])

    modelN2Prop = []

    for exp_type in exp_types:
        exp_data = plot_data[plot_data['expType'] == exp_type]
        max_item = max(exp_data['item'].tolist())
        for item in range(1, max_item+1):
            item_data = exp_data[exp_data['item'] == item]

            #Cases where an item had to be dropped from
            #the experiment
            if len(item_data) == 0:
                continue

            for model_name in item_data['model'].unique().tolist():
                model_data = item_data[item_data['model'] == model_name]

                low = model_data[model_data['AttachType'] == 'low']
                high = model_data[model_data['AttachType'] == 'high']

                contrasts.add(model_data['contrast'].tolist()[0])

                props = (low['prob'].values - high['prob'].values)

                low_num = len(props[props >= 0])
                high_num = len(props[props < 0])
                total = len(props)
                modelN2Prop.append(low_num/total)

    #ambi_data is one instance of each item per model rather than 
    #the full set of number manipulations which lead to the score
    ambi_data = ambi_data[ambi_data['contrast'].isin(contrasts)]
    #add model preferences for N2
    ambi_data['modelN2Prop'] = modelN2Prop

    #Plot it
    expType = ambi_data['expType'].tolist()
    cats = []
    for e in expType:
        cats.append(e[0])
    ambi_data['expCat'] = cats

    base_columns = list(filter(lambda x: 'N2Prop' not in x, ambi_data.columns.tolist()))
    ambi_data = pd.melt(ambi_data, id_vars = base_columns, 
            var_name='N2PropSource', value_name='N2Prop')

    N2PropSource = ambi_data['N2PropSource'].tolist()
    renamedSource = []
    for source in N2PropSource:
        if 'model' in source:
            renamedSource.append('model')
        else:
            renamedSource.append('human')

    ambi_data['N2PropSource'] = renamedSource

    plot_data = ambi_data

    print(f"Saving compiled data to {fname_base}_combined_flat.csv")
    plot_data.to_csv(f"{fname_base}_combined_flat.csv", index=False)

    return plot_data

if __name__ == "__main__":

    ###################
    # Compile Results #
    ###################

    for lang in ['en', 'es']:
        for exp_name in ['FCM', 'Carreiras_Clifton', 'Gilboy', 'Rohde']:
            fnames = []

            if exp_name == "FCM":
                fname_base = 'results/FCM'

            elif exp_name == "Carreiras_Clifton":
                if lang == 'en':
                    continue
                fname_base = 'results/Carreiras_Clifton_1993'

            elif exp_name == "Gilboy":
                fname_base = 'results/Gilboy_etal_1995'

            elif exp_name == 'Rohde':
                if lang == 'es':
                    continue
                fname_base = 'results/Rohde_etal_2011'

            if lang == 'es':
                fname_base += "_Spanish"
            elif lang == 'en':
                fname_base += "_English"


            exp = TSE(exp_name)

            #Add bert
            fnames.append(fname_base+'_bert.csv')
            #Add GPT2
            fnames.append(fname_base+'_auto.csv')
            #Add LSTMs
            fnames.append(fname_base+'_lstm.csv')

            #Compile results
            exp.combine_precompiled_experiments(fnames)
            #Flatten and save
            dataframe = flatten_dataframe(exp.dataframe, fname_base, exp_name)
