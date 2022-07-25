#### Set up the system interface
from src.models import models
from src.experiments.TSE import TSE
from src.experiments.Interact import Interact
import os
import sys

import configparser
import yaml

try:
    from progress.bar import Bar
    PROGRESS = True
except ModuleNotFoundError:
    PROGRESS = False

def return_verb_list(fname):

    import pandas as pd
    data = pd.read_csv(fname)
    return data

def check_verb_list(fname, LMs):

    verb_data = return_verb_list(fname)
    print(verb_data)

    inAllVocabs = []
    for _, row in verb_data.iterrows():
        sg = row['sing']
        pl = row['plur']
        inVocabs = 1
        for LM in LMs:
            if (not LM.word_in_vocab(sg)) or (not LM.word_in_vocab(pl)):
                inVocabs = 0
                break
        inAllVocabs.append(inVocabs)

    verb_data['inVocabs'] = inAllVocabs
    verb_data.to_csv(fname.split('.csv')[0]+'_vocab_checked.csv', index=False)

if __name__ == "__main__":
    path_config = configparser.ConfigParser()
    path_config.read('path_config.cfg')

    if len(sys.argv) != 2:
        print('Did not pass in run_config file, using default: run_config.yaml')
        run_config_fname = 'run_config.yaml'
    else:
        run_config_fname = sys.argv[1]

    with open(run_config_fname, 'r') as f:
        run_config = yaml.load(f, Loader=yaml.FullLoader)

    #add paths to run_config
    run_config['nc_path'] = path_config['libraries']['neural-complexity']

    exp = Interact(run_config)
    exp.run_interact()

    #sent = 'the man who is tall are happy.'
    #print(exp.get_interactive_output(sent))

    '''
    LMs = models.load_models(run_config)
    #texts = ['the man stepped outside.', 'the man was outside.', 'the man are outside.', 'the man is outside.']
    texts = ['The Assistant Professor of Theoretical/Computational Linguistics will have expertise within the following: general theoretical linguistics (syntax, semantics, phonology, historical linguistics) and/or computational linguistics. Additional welcome areas of interest may include natural language processing, underrepresented/minority languages, language variation and bilingualism, world Englishes, writing studies, corpus linguistics, and/or computer-assisted language learning.']
    for LM in LMs:
        print(LM)

        LM.get_output(texts)
        
        sent_surps = LM.get_aligned_words_probabilities(texts)
        for sent_surp in sent_surps:
            for word_surp in sent_surp:
                print(word_surp.word, word_surp.prob)
            print()
    '''
    '''

    fname = 'stimuli/IC_RC_Stimuli.csv'
    outname = 'results/IC_RC_LSTMs.csv'

    exp = TSE(fname)
    exp.load_experiment()
    exp.load_verb_list()
    for LM in LMs:
        exp.get_targeted_results(LM)
    exp.save(outname)
    '''
    '''
    fnames = ['results/IC_RC_non_autoregressive.csv',
            'results/IC_RC_autoregressive.csv',
            'results/IC_RC_LSTMs.csv']

    exp = TSE('IC_RC')
    exp.combine_precompiled_experiments(fnames)
    #exp.save_flattened('results/IC_RC_combined_flat.csv')
    #exp.save('results/IC_RC_combined.csv')
    exp.plot_rc_results()
    '''

    '''
    fnames = ['results/IC_mismatch_IT_cont.csv']
    exp = TSE('IC_Adj')
    exp.combine_precompiled_experiments(fnames)
    #exp.save_flattened('results/IC_ES_Adjectives.csv')
    exp.plot_adj_results('Italian')
    '''

    '''
    LMs = models.load_models(run_config)

    exp = TSE('stimuli/IC_mismatch.csv')
    exp.load_experiment()
    for LM in LMs:
        exp.get_targeted_results(LM)
    exp.get_targeted_difference_results()
    exp.save('results/IC_mismatch_cont_LSTMs.csv')
    #exp.save('results/IC_mismatch_non_autoregressive.csv')
    '''

    '''
    fnames = ['results/IC_mismatch_cont_non_autoregressive.csv', 
            'results/IC_mismatch_cont_autoregressive.csv', 
            'results/IC_mismatch_cont_LSTMs.csv']
            
    exp = TSE('IC_mismatch')
    exp.combine_precompiled_experiments(fnames)
    exp.plot_difference_results()
    '''

    #fname = 'stimuli/IC_mismatch_BERT.csv'

    '''
    fnames = ['results/IC_mismatch_autoregressive.csv', 'results/IC_mismatch_LSTMs.csv', 'results/IC_mismatch_non_autoregressive.csv']
    exp = TSE('IC_mismatch')
    exp.combine_precompiled_experiments(fnames)
    #exp.save_flattened('results/IC_mismatch_combined_flat.csv')
    exp.plot_results()

    '''


    '''
    fname = 'stimuli/IC_passive.csv'
    exp = TSE(fname)
    exp.load_experiment()
    LMs = models.load_models(run_config)
    for LM in LMs:
        exp.get_targeted_results(LM)
        del(LM)

    print(exp.dataframe)
    #exp.save('results/IC_passive_LSTMs.csv')
    #exp.plot_results()

    '''

    '''
    fnames = ['results/IC_passive_autoregressive.csv',
    'results/IC_passive_LSTMs.csv',
    'results/IC_passive_non_autoregressive.csv']
    exp = TSE('IC_passive')
    exp.combine_precompiled_experiments(fnames)
    exp.get_targeted_difference_results()
    exp.plot_difference_results()

    #    exp.plot_results(str(LM))
    #exp.plot_results(str(LMs[1]))
    #exp.save('results/IC_mismatch_non_autoregressive.csv')
    '''

    '''
    fname = "/home/forrestdavis/Projects/ModelsAPI/stimuli/Nicol1988.csv"
    exp = Nicol(fname)

    ##########################
    #      Load Models       #
    ##########################
    if run_config['verbose']:
        print('Loading Models...')
    exp.get_models(run_config)

    exp.load_experiment()
    exp.set_stimuli_similarities(layer_number=2)
    exp.plot_data()
    #exp.save('results/nicol1988_results.csv')
    '''


    '''
    ##########################
    #      Load Models       #
    ##########################
    if run_config['verbose']:
        print('Loading Models...')
    LMs = models.load_models(run_config)
    context_type = run_config['context']
    '''

    '''
    ##########################
    #      Load Corpora      #
    ##########################
    Corpora = datasets.get_datasets(run_config)

    if run_config['load_binary']:
        if run_config['verbose']:
            print('Loading Corpora from binary files...')
        #Ensure we have 1 binary file per corpus
        assert len(run_config['corpora_binary_fnames']) == len(Corpora)
        for binary_fname, corpus in zip(run_config['corpora_binary_fnames'], Corpora):
            corpus.load_binary(binary_fname)
    else:
        if run_config['verbose']:
            print('Loading Corpora...')
        for corpus in Corpora:
            corpus.load_corpus()

    ##########################
    #      Save Corpus       #
    ##########################
    if run_config['save_binary']:
        if run_config['verbose']:
            print('Saving Corpora...')
        #Ensure we have 1 binary file per corpus
        assert len(run_config['corpora_binary_fnames']) == len(Corpora)
        for binary_fname, corpus in zip(run_config['corpora_binary_fnames'], Corpora):
            corpus.save_binary(binary_fname)

    ##########################
    #    Get Surprisals      #
    ##########################
    for LM in LMs:
        for corpus in Corpora:
            aligned_documents_words_surprisals = []

            if PROGRESS and run_config['verbose']:
                bar = Bar(f"Processing surprisals for {corpus} with {LM}", max=len(corpus))

            for document in corpus:
                doc_words = []
                if context_type == 'sent':
                    for sentence in document:
                        sent_words_surprisals = LM.get_aligned_words_surprisals(
                                sentence.text, 
                                include_punctuation=run_config['include_punct'])[0]
                        doc_words += sent_words_surprisals
                elif context_type == 'para':
                    doc_words_surprisals = LM.get_aligned_words_surprisals(
                            document.text, 
                            include_punctuation=run_config['include_punct'])[0]
                    doc_words += doc_words_surprisals
                else:
                    raise ValueError(f"Unrecognized context type: {context_type}.")
                aligned_documents_words_surprisals.append(doc_words)
                if PROGRESS and run_config['verbose']:
                    bar.next()

            corpus.update_words_with_surprisals(aligned_documents_words_surprisals)
            if PROGRESS and run_config['verbose']:
                bar.finish()

    ##########################
    #      Save Results      #
    ##########################
    if run_config['verbose']:
        print('Saving Results...')
    for result_fname, corpus in zip(run_config['results_fnames'], Corpora):
        corpus.save(result_fname)
    '''
