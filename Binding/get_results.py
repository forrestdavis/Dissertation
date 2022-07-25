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
        print('Loading models...')
        LMs = models.load_models(run_config)

        #Run experiments
        for exp_name in run_config['exp']:
            #Set experiment
            fname = ''

            if exp_name == 'Chow_1':

                fname = 'stimuli/Chow_Exp1.csv'

            elif exp_name == 'Nicol':
                fname = 'stimuli/Nicol_Exp.csv'

            elif exp_name == 'vanGompel_1':
                fname = 'stimuli/vanGompel_Liversedge_Exp1.csv'

            elif exp_name == 'Kush_Dillon_1':
                fname = 'stimuli/Kush_Dillon_Exp1.csv'

            elif exp_name == 'Kush_Dillon_2':
                fname = 'stimuli/Kush_Dillon_Exp2.csv'


            print(f"Running {exp_name}...")

            assert fname != ''
            exp = TSE(fname)

            #Load experiment and wildcards
            exp.load_experiment()

            #Set sent to write type (e.g., the [MASK] is happy vs. the )
            if run_config['model_type'] == 'bert':
                exp.dataframe['sent'] = exp.dataframe['mask_sents']
            elif run_config['model_type'] in {'auto', 'lstm'}:
                exp.dataframe['sent'] = exp.dataframe['auto_sents']


            #Run models
            for LM in LMs:
                print(LM)
                exp.get_targeted_results(LM, return_type = 'surp')

            #Save output
            outname = 'results/'+fname.split('/')[-1].split('.csv')[0]+'_'+run_config['model_type']+'.csv'
            print(f"Saving to {outname}....")
            exp.save(outname)
