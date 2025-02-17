from utils.states.states_default import get_default_states
from utils.states.states_from_nc import states_from_nc
import os
import pandas as pd

def get_states_from_manager(options=None,network:pd.DataFrame=None)->pd.DataFrame:
    if 'initial_states' not in list(options.keys()):
        print('Error. No states option in yaml')
        quit()
    f = options['initial_states']
    if f=='default':
        df = get_default_states(network)
        return df
    if os.path.isfile(f)==False:
        print('Error. States file not found')
        quit()
    _, extension = os.path.splitext(f)
    if extension =='.pkl':
        try:
            df = pd.read_pickle(f)
            return df
        except AttributeError as e:
            print('states pickle file created with different version of pandas')
            print(e)
        quit()
    if extension == '.nc':
        if 'init_time' not in list(options.keys()):
            print('Error. No init time option in yaml')
            quit()
        init_time = options['init_time']
        df = states_from_nc(f,init_time,network)
        return df
    if extension == '.csv':
        pass   
