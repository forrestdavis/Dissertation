from src.models import models
from src.experiments.Interact import Interact

import configparser
import yaml
import sys

if __name__ == "__main__":

    path_config = configparser.ConfigParser()
    path_config.read('path_config.cfg')

    if len(sys.argv) != 2:
        print('Did not pass in config file, using default: interact_config.yaml')
        run_config_fname = 'interact_config.yaml'
    else:
        run_config_fname = sys.argv[1]

    with open(run_config_fname, 'r') as f:
        run_config = yaml.load(f, Loader=yaml.FullLoader)

    #add paths to run_config
    run_config['nc_path'] = path_config['libraries']['neural-complexity']

    exp = Interact(run_config)

    exp.run_interact()

