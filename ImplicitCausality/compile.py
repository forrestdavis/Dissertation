import os
import sys
import configparser
import yaml
import pandas as pd

#### Set up the system interface
sys.path.append(os.path.join("..", "ModelsAPI"))
from src.experiments.TSE import TSE

def flatten_dataframe(dataframe, fname_base):

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

    print(f"Saving compiled data to {fname_base}_combined_flat.csv")
    plot_data.to_csv(f"{fname_base}_combined_flat.csv", index=False)

    return plot_data


def get_targeted_difference_results(dataframe):

    subj_data = dataframe[dataframe['antecedent']=='subj']
    obj_data = dataframe[dataframe['antecedent']=='obj']

    target_columns = list(filter(lambda x: '_prob' in x, dataframe.columns.to_list()))
    base_columns = list(filter(lambda x: '_prob' not in x and 'antecedent' not in x, dataframe.columns.to_list()))

    return_data = subj_data[base_columns].copy()

    for target_column in target_columns:
        #This should be improved, I'm tired...
        obj = obj_data[target_column].to_list()
        subj = subj_data[target_column].to_list()
        diff = []
        for s,o in zip(subj, obj):
            diff.append((s-o)*100)
        return_data[target_column.replace('_prob', '_diff')] = diff

    dataframe = return_data
    dataframe = dataframe.groupby(['verb'], as_index=False).mean()
    return dataframe

def plot_ferstl_difference_results(dataframe, expname):

    dataframe = get_targeted_difference_results(dataframe)

    import seaborn as sns
    import matplotlib.pyplot as plt

    columns = list(filter(lambda x: '_diff' in x, dataframe.columns.to_list()))
    base_columns = list(filter(lambda x: '_diff' not in x, dataframe.columns.to_list()))

    lstms = list(filter(lambda x: 'lstm' in x.lower(), columns))
    non_lstms = list(filter(lambda x: 'lstm' not in x.lower(), columns))
    #filter out the other gpt-2 models
    non_lstms = list(filter(lambda x: not('gpt2' in x and 'xl' not in x), non_lstms))

    #Filter down data for melt
    plot_data = dataframe[base_columns+non_lstms]
    #Get row-wise average of lstms
    plot_data['lstm avg_diff'] = dataframe[lstms].mean(axis=1)
    #pivot around model names
    plot_data = pd.melt(plot_data, 
            id_vars=base_columns, var_name='model', value_name='diff')
    #rename model names to something sensible
    plot_data['model'] = list(map(lambda x: x.split('/')[-1].replace('_diff', ''), plot_data['model'].to_list()))

    filtered_models = plot_data['model'].unique().tolist()
    filtered_models = list(filter(lambda x: not ('probert' in x.lower() or 'proroberta' in x.lower()), filtered_models))
    plot_data = plot_data[plot_data['model'].isin(filtered_models)]

    #Rename Models
    mnames = plot_data['model'].tolist()
    for idx, mname in enumerate(mnames):
        if mname == 'lstm avg':
            mnames[idx]="LSTM avg"
        elif mname == 'transfo-xl-wt103':
            mnames[idx] = "TransformerXL"
        elif mname == 'gpt2-xl':
            mnames[idx] = "GPT-2 XL"
        elif mname == 'bert-base-uncased':
            mnames[idx] = "BERT"
        elif mname == 'roberta-base':
            mnames[idx] = "RoBERTa"

    plot_data['model'] = mnames

    #Helper
    import scipy as sp
    def annotate(data, **kws):
        r, p = sp.stats.pearsonr(data['diff'], data['bias'])
        ax = plt.gca()
        ax.text(.05, .9, 'r={:.2f}, p={:.2g}'.format(r, p),
                transform=ax.transAxes)

    #PLOT
    sns.set(style='darkgrid', font_scale=1.5)

    g = sns.lmplot(x='diff', y='bias', col='model', data=plot_data, 
            col_wrap=3, scatter_kws={'color': 'm'}, line_kws={"color":"gold"})

    g.map_dataframe(annotate)

    g.set(xlim=(-100, 100))
    g.set(ylim=(-100, 120))

    g.set_axis_labels("Neural Model IC Bias", "Human IC Bias")
    g.set_titles("{col_name}")

    g.fig.subplots_adjust(top=0.9)

    if exp_name == 'Ferstl':
        g.fig.suptitle("Model versus Human IC Verb Bias")
        plt.savefig('figures/English-IC-differences.png')

    elif exp_name == 'Passive':
        g.fig.suptitle("Model versus Human IC Verb Bias with Passives")
        plt.savefig('figures/English-IC-differences-Passive.png')

    #plt.show()
    plt.clf()

def plot_results(dataframe, exp_name):

    if exp_name == 'Ferstl':
        plot_ferstl_difference_results(dataframe, exp_name)

    elif exp_name == 'Passive':
        plot_ferstl_difference_results(dataframe, exp_name)

if __name__ == "__main__":

    ###################
    # Compile Results #
    ###################

    for exp_name in ['Ferstl', 'Passive']:
        fnames = []

        if exp_name == 'Ferstl':
            fname_base = 'results/English_IC'

        elif exp_name == 'Passive':
            fname_base = 'results/English_IC_passive'

        exp = TSE(exp_name)

        #Add bert
        fnames.append(fname_base+'_bert.csv')
        #Add gpt2 tfxl
        fnames.append(fname_base+'_auto.csv')
        #add lstms
        fnames.append(fname_base+'_lstm.csv')
                
        #Compile results
        exp.combine_precompiled_experiments(fnames)

        #plot results
        plot_results(exp.dataframe, exp_name)

        if exp_name == 'Ferstl':
            plot_data = flatten_dataframe(exp.dataframe, fname_base)
