import pandas as pd
import dill
from collections import defaultdict
from ..models.models import load_models
from .Experiment import Experiment

class TSE(Experiment):

    def __init__(self, fname):
        super().__init__()
        self.name = fname
        self.dataframe = None
        self.special_strings = {}

    def load_dataframe(self):
        self.dataframe = pd.read_csv(self.name)

    def to_dataframe(self):
        """Makes pandas dataframe from experiment.
        """
        return self.dataframe

    def load_special_list(self):
        """Loads the verbs from Newman et al. (2021) 
        which are contained in BERT/RoBERTa/LSTMs/GPT2/TFXL.
        """
        english_fname = 'stimuli/combined_verb_list_vocab_checked.csv'
        spanish_fname = 'stimuli/spanish_verbs_vocab_checked.csv'
        spanish_adj_fname = 'stimuli/spanish_adjectives_vocab_checked.csv'

        en_verb_data = pd.read_csv(english_fname)
        es_verb_data = pd.read_csv(spanish_fname)
        es_adj_data = pd.read_csv(spanish_adj_fname)
        #filter out of vocab verb pairs
        en_verb_data = en_verb_data[en_verb_data['inVocabs'] == 1]
        es_verb_data = es_verb_data[es_verb_data['inVocabs'] == 1]
        es_adj_data = es_adj_data[es_adj_data['inVocabs'] == 1]

        #Holds special string by list of strings 
        #   (e.g., '$SG' -> ['runs', 'eats', ...]
        self.special_strings = {'$SG': [],
                                '$PL': [], 
                                '$ES_SG': [],
                                '$ES_PL': [], 
                                '$ES_Adj_m': [], 
                                '$ES_Adj_f': []}
                                
        self.special_strings['$SG'].extend(en_verb_data['sing'].to_list())
        self.special_strings['$ES_SG'].extend(es_verb_data['sing'].to_list())
        self.special_strings['$PL'].extend(en_verb_data['plur'].to_list())
        self.special_strings['$ES_PL'].extend(es_verb_data['plur'].to_list())
        self.special_strings['$ES_Adj_m'].extend(es_adj_data['m_sg'].to_list())
        self.special_strings['$ES_Adj_f'].extend(es_adj_data['f_sg'].to_list())

    def expand_strings(self, targets):
        """Returns targets with any special strings 
        expanded into their lists.

        Returns: 
            List[List[str] | str]: List of target strings and list of strings 
        """

        expanded_targets = []
        for target in targets:
            if target in self.special_strings:
                expanded_targets.append(self.special_strings[target])
            else:
                expanded_targets.append(target)

        return expanded_targets
        

    def load_experiment(self):
        """Loads the respective experiment and sets
        to self._stimuli

        Returns:
            list: collection of Sentence instances
        """
        self.load_dataframe()

    def combine_precompiled_experiments(self, fnames):

        for x, fname in enumerate(fnames):
            if x == 0:
                self.dataframe = pd.read_csv(fname)
            else:
                exisiting_columns = set(self.dataframe.columns.to_list())
                data = pd.read_csv(fname)
                current_columns = data.columns.to_list()
                for column in current_columns:
                    if column not in exisiting_columns:
                        self.dataframe[column] = data[column]

    def save_flattened(self, fname):

        columns = list(filter(lambda x: '_prob' in x, self.dataframe.columns.to_list()))
        base_columns = list(filter(lambda x: '_prob' not in x, self.dataframe.columns.to_list()))

        lstms = list(filter(lambda x: 'lstm' in x.lower(), columns))

        return_data = self.dataframe.copy()

        #Get row-wise average of lstms
        if len(lstms) != 0:
            return_data['lstm_avg_prob'] = return_data[lstms].mean(axis=1)

        #pivot around model names
        return_data = pd.melt(return_data, 
                id_vars=base_columns, var_name='model', value_name='prob')
        #rename model names to something sensible
        return_data['model'] = list(map(lambda x: x.split('/')[-1].replace('_prob', ''), return_data['model'].to_list()))

        return_data.to_csv(fname, index=False)

    def plot_adj_results(self, language='Spanish'):

        import seaborn as sns
        import matplotlib.pyplot as plt

        columns = list(filter(lambda x: '_prob' in x, self.dataframe.columns.to_list()))
        base_columns = list(filter(lambda x: '_prob' not in x, self.dataframe.columns.to_list()))

        filtered_columns = list(filter(lambda x: 'BASE' not in x, columns))

        #Filter down data for melt
        plot_data = self.dataframe[base_columns+filtered_columns]

        #pivot around model names
        plot_data = pd.melt(plot_data, 
                id_vars=base_columns, var_name='model', value_name='prob')

        #rename model names to something sensible
        plot_data['model'] = list(map(lambda x: x.split('/')[-1].replace('_prob', ''), plot_data['model'].to_list()))

        #rename antecedent labels
        ants = plot_data['antecedent'].to_list()
        for i in range(len(ants)):
            if ants[i] == 'subj':
                ants[i] = 'Subject Antecedent'
            else:
                ants[i] = 'Object Antecedent'
        plot_data['antecedent'] = ants

        #rename PRO
        models = plot_data['model'].to_list()
        for i in range(len(models)):
            if 'PRO' in models[i]:
                models[i] = models[i].split('_')[0]+' without ProDrop'
        plot_data['model'] = models

        #PLOT
        sns.set(style='darkgrid', font_scale=1.3)

        order = ["subj", "obj"]
        palette=["m", "gold"]
        g = sns.catplot(x="IC", y="prob", hue='antecedent', col="model", 
                col_wrap=3,
                notch=True, palette=palette,
                kind="box", order=order, data=plot_data, 
                legend=False)

        g.set(ylim=(0, 1))

        g.set_axis_labels("", "Adj Probability")
        g.set_xticklabels(["Subject-Bias", "Object-Bias"])
        g.set_titles("{col_name}")

        g.fig.subplots_adjust(top=0.9)
        g.fig.suptitle(f"{language} Adjective Agreement Probability by IC Verb Bias")

        plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)

        plt.savefig(f"figures/IC-{language}-adj-probabilities.png")

        plt.show()

    def plot_difference_results(self):

        import seaborn as sns
        import matplotlib.pyplot as plt

        columns = list(filter(lambda x: '_diff' in x, self.dataframe.columns.to_list()))
        base_columns = list(filter(lambda x: '_diff' not in x, self.dataframe.columns.to_list()))

        lstms = list(filter(lambda x: 'lstm' in x.lower(), columns))
        non_lstms = list(filter(lambda x: 'lstm' not in x.lower(), columns))
        #filter out the other gpt-2 models
        non_lstms = list(filter(lambda x: not('gpt2' in x and 'xl' not in x), non_lstms))

        #Filter down data for melt
        plot_data = self.dataframe[base_columns+non_lstms]
        #Get row-wise average of lstms
        plot_data['lstm avg_diff'] = self.dataframe[lstms].mean(axis=1)
        #pivot around model names
        plot_data = pd.melt(plot_data, 
                id_vars=base_columns, var_name='model', value_name='diff')
        #rename model names to something sensible
        plot_data['model'] = list(map(lambda x: x.split('/')[-1].replace('_diff', ''), plot_data['model'].to_list()))
        #rename antecedent labels

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
        g.fig.suptitle("Model versus Human IC Verb Bias")


        plt.savefig('figures/IC-mismatch-differences.png')

    def plot_results(self):

        import seaborn as sns
        import matplotlib.pyplot as plt

        columns = list(filter(lambda x: '_prob' in x, self.dataframe.columns.to_list()))
        base_columns = list(filter(lambda x: '_prob' not in x, self.dataframe.columns.to_list()))

        lstms = list(filter(lambda x: 'lstm' in x.lower(), columns))
        non_lstms = list(filter(lambda x: 'lstm' not in x.lower(), columns))
        #filter out the other gpt-2 models
        non_lstms = list(filter(lambda x: not('gpt2' in x and 'xl' not in x), non_lstms))

        #Filter down data for melt
        plot_data = self.dataframe[base_columns+non_lstms]
        #Get row-wise average of lstms
        plot_data['lstm avg_prob'] = self.dataframe[lstms].mean(axis=1)
        #pivot around model names
        plot_data = pd.melt(plot_data, 
                id_vars=base_columns, var_name='model', value_name='prob')
        #rename model names to something sensible
        plot_data['model'] = list(map(lambda x: x.split('/')[-1].replace('_prob', ''), plot_data['model'].to_list()))
        #rename antecedent labels
        ants = plot_data['antecedent'].to_list()
        for i in range(len(ants)):
            if ants[i] == 'subj':
                ants[i] = 'Subject Antecedent'
            else:
                ants[i] = 'Object Antecedent'
        plot_data['antecedent'] = ants

        #PLOT
        sns.set(style='darkgrid', font_scale=2)

        order = ["subj", "obj"]
        palette=["m", "gold"]
        g = sns.catplot(x="IC", y="prob", hue='antecedent', col="model", 
                col_wrap=3,
                notch=True, palette=palette,
                kind="box", order=order, data=plot_data, 
                legend=False)

        #for i, patch in enumerate(g.ax.patches):
        #    patch.set_hatch(hatch)

        #for i in range(len(g.axes['boxes'])):
        #    g.axes['boxes'][i].set(hatch='x')


        g.set_axis_labels("", "Pronoun Probability")
        g.set_xticklabels(["Subject-Bias", "Object-Bias"])
        g.set_titles("{col_name}")

        g.fig.subplots_adjust(top=0.87)
        g.fig.suptitle("Pronoun Probability by IC Verb Bias with Passives")

        plt.legend(bbox_to_anchor=(1.05, 1), loc=2, borderaxespad=0.)

        plt.savefig('figures/IC-passive-probabilities.png')

        plt.show()

    def get_targeted_results(self, model, batch_size=40, lowercase=True, return_type = 'prob'):

        if self.dataframe is None:
            self.load_experiment()

        contexts = self.dataframe['sent'].to_list()
        targets = self.dataframe['target'].to_list()

        target_measure = []
        for idx in range(0, len(targets), batch_size):
            context_batch = contexts[idx:idx+batch_size]
            if lowercase:
                context_batch = list(map(lambda x: x[0].lower()+x[1:], context_batch))
            target_batch = targets[idx:idx+batch_size]
            #Expand out special tokens 
            target_batch = self.expand_strings(target_batch)
            if return_type == 'prob':
                target_measure.extend(model.get_targeted_word_probabilities(context_batch, target_batch))
            elif return_type == 'surp':
                target_measure.extend(model.get_targeted_word_surprisals(context_batch, target_batch))
            else:
                import sys
                sys.stderr.write(f"return type {return_type} not recognized")
                sys.exit(1)

        assert len(contexts) == len(target_measure)
        if return_type == 'prob':
            self.dataframe[str(model)+'_prob'] = target_measure
        elif return_type == 'surp':
            self.dataframe[str(model)+'_surp'] = target_measure

    def get_targeted_difference_results(self):

        subj_data = self.dataframe[self.dataframe['antecedent']=='subj']
        obj_data = self.dataframe[self.dataframe['antecedent']=='obj']

        target_columns = list(filter(lambda x: '_prob' in x, self.dataframe.columns.to_list()))
        base_columns = list(filter(lambda x: '_prob' not in x and 'antecedent' not in x, self.dataframe.columns.to_list()))

        return_data = subj_data[base_columns].copy()

        for target_column in target_columns:
            #This should be improved, I'm tired...
            obj = obj_data[target_column].to_list()
            subj = subj_data[target_column].to_list()
            diff = []
            for s,o in zip(subj, obj):
                diff.append((s-o)*100)
            return_data[target_column.replace('_prob', '_diff')] = diff

        self.dataframe = return_data
        self.dataframe = self.dataframe.groupby(['verb'], as_index=False).mean()

