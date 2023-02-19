from scipy.integrate import solve_ivp
import numpy as np
import pandas as pd
import multiprocessing as mp
import time

def nonlinear_velocity(states:pd.DataFrame,
    params:pd.DataFrame,
    network:pd.DataFrame,DT:int):

    def fun(t,q,invtau,idx_up,lambda1):
        q_aux = pd.concat([
             pd.Series(0,index=[0]),
             pd.Series(q)
        ]).to_numpy() #it is important to convert this pd df into a nparray,otherwise i got broadcast errors
        q_upstream = np.array([np.sum(q_aux[x]) for x in idx_up])
        dq_dt = invtau*q_aux[1:]**lambda1*(-1*q_aux[1:] + q_upstream)
        return dq_dt
    
    q = states['discharge']
    q_aux = pd.concat([
            pd.Series(0,index=[0]),
            pd.Series(states['discharge'])
        ])
    invtau = np.divide(
                np.multiply(
                    params['river_velocity'],
                    np.power(params['drainage_area'],params['lambda2'])
                ),
                np.multiply(
                    np.subtract(1,params['lambda1'])
                    ,params['channel_length']
                )
    )
    idx_up = network['upstream_link']
    lambda1 =params['lambda1']
    t_end_sim = DT*60 #since the ODE flow inputs are in m3/s , t_end_sim is the number of seconds of routing process
    res = solve_ivp(fun,
            t_span=(0,t_end_sim),
            y0=q,
            args=(invtau,idx_up,lambda1),
        )
    n_eval = res.t.shape[0] 
    y_1 = res.y[:,n_eval-1]
    states['discharge'] = y_1


def linear_velocity(states:pd.DataFrame,
    velocity:np.float16,
    network:pd.DataFrame,DT:int):

#constrain: q needs to be an nd-array. the solver change the data structure
    def fun(t,q,velocity,channel_len_m,idx_up): #t in minutes, q in m3/h
        #print(type(q))
        q_aux = np.concatenate(([0],q))
        q_upstream = np.zeros(q.shape)
        q_upstream = np.array([np.sum(q_aux[x]) for x in idx_up]) #m3/h
        velocity *=60*60 #m/s to m/h
        dq_dt = (1/channel_len_m )* velocity * (-1*q_aux[1:] + q_upstream)
        return dq_dt

    idx_up = network['idx_upstream_link'].to_numpy()
    #t_end_sim = DT*60 #since the ODE flow inputs are in m3/s , t_end_sim is the number of seconds of routing process
    t_end_sim = DT / 60 # in hours
    q = np.array(states['discharge'])
    channel_len_m = np.array(network['channel_length'])
    start_time = time.time()
    res = solve_ivp(fun,
            t_span=(0,1),
            y0=q*60*60, #m3/s to m3/h
            args=(velocity,channel_len_m,idx_up),
        )
    print("--- %s seconds ---" % (time.time() - start_time))
    #takes 90 seconds run one hour
    n_eval = res.t.shape[0] 
    y_1 = res.y[:,n_eval-1]/3600. #m3/h to m3/s
    states['discharge'] = y_1

