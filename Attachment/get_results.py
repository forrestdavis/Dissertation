import os
import sys
import configparser
import yaml

#### Set up the system interface
sys.path.append(os.path.join("..", "ModelsAPI"))
from src.models import models
from src.experiments.TSE import TSE

if __name__ == "__main__":

    path_config = configparser.ConfigParser()
    path_config.read('path_config.cfg')

    if len(sys.argv) != 2:
        print('Did not pass in run_config file')
        sys.exit(1)
    else:
        run_config_fname = sys.argv[1]

    with open(run_config_fname, 'r') as f:
        run_config = yaml.load(f, Loader=yaml.FullLoader)

    #add paths to run_config
    run_config['nc_path'] = path_config['libraries']['neural-complexity']


    ################
    # Get Results
    ################

    if run_config['run'] == 'results':

        #load models
        LMs = models.load_models(run_config)

        #Run experiments
        for exp_name in run_config['exp']:
            #Set experiment
            fname = ''

            if exp_name == 'FCM':

                en_fname = 'stimuli/FCM_English.csv'
                es_fname = 'stimuli/FCM_Spanish.csv'

                if run_config['lang'] == 'en':
                    fname = en_fname
                elif run_config['lang'] == 'es':
                    fname = es_fname

            elif exp_name == 'Carreiras_Clifton':

                if run_config['lang'] == 'es':
                    fname = 'stimuli/Carreiras_Clifton_1993_Spanish.csv'

            elif exp_name == 'Gilboy':

                en_fname = 'stimuli/Gilboy_etal_1995_English.csv'
                es_fname = 'stimuli/Gilboy_etal_1995_Spanish.csv'

                if run_config['lang'] == 'en':
                    fname = en_fname
                elif run_config['lang'] == 'es':
                    fname = es_fname

            elif exp_name == 'Rohde':
                
                if run_config['lang'] == 'en':
                    fname = 'stimuli/Rohde_etal_2011_English.csv'

            print(f"Running {exp_name}...")

            assert fname != ''
            exp = TSE(fname)

            #Load experiment and wildcards
            exp.load_experiment()
            exp.load_special_list()

            #Set sent to write type (e.g., the [MASK] is happy vs. the )
            if run_config['model_type'] == 'bert':
                exp.dataframe['sent'] = exp.dataframe['mask_sents']
            elif run_config['model_type'] in {'auto', 'lstm'}:
                exp.dataframe['sent'] = exp.dataframe['auto_sents']


            #Run models
            for LM in LMs:
                print(LM)
                exp.get_targeted_results(LM)

            #Save output
            outname = 'results/'+fname.split('/')[-1].split('.csv')[0]+'_'+run_config['model_type']+'.csv'
            print(f"Saving to {outname}....")
            exp.save(outname)
