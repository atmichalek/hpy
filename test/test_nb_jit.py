from numba import config, njit, threading_layer,prange
import numpy as np
import time 

# set the threading layer before any parallel target compilation
config.THREADING_LAYER = 'threadsafe'

@njit(parallel=True)
def foo(a, b):
    return a + b

@njit(parallel=False)
def foo2(a, b):
    return a + b


x = np.arange(10.)
y = x.copy()

# this will force the compilation of the function, select a threading layer
# and then execute in parallel
foo(x, y)

# Benchmarking
start_time = time.time()
parallel_results = foo(x, y)
parallel_time = time.time() - start_time

start_time = time.time()
non_parallel_results = foo2(x, y)
non_parallel_time = time.time() - start_time

print("Parallel Time:", parallel_time)
print("Non-Parallel Time:", non_parallel_time)

# demonstrate the threading layer chosen
print("Threading layer chosen: %s" % threading_layer())


#stack overflow example:
@njit
def pend(t, y, b, c):
    theta, omega = y
    dydt = np.array([omega, -b*omega - c*np.sin(theta)])
    return dydt
@njit(parallel=True)
def rungeStep(f, t, y0, tau, params):
    k1 = tau * f(t, y0, *params)
    k2 = tau * f(t + tau/2, y0 + k1 / 2, *params)
    k3 = tau * f(t + tau/2, y0 + k2 / 2, *params)
    k4 = tau * f(t + tau, y0 + k3, *params)
    return (k1 + 2 * k2 + 2 * k3 + k4) / 6
@njit(parallel=True)
def integrate(f, t0, y0, tEnd, h, params):
    ys = [list(y0)]
    t = [t0]
    while t0 <= tEnd:
        y0 += rungeStep(f, t0, y0, h, params)
        t0 += h
        ys.append(list(y0))
        t.append(t0)
    return np.array(t), np.array(ys).T

args = (0.25, 5.0)
y0 = np.array([np.pi - 0.1, 0.0])
t, y = integrate(pend, 0, y0, 10, 1e-1, args)


def create_solver(hlm_object):

    N = hlm_object.network.shape[0]
    channel_len_m = np.array(hlm_object.network['channel_length'])
    velocity = np.array(hlm_object.params['river_velocity']*3600) #m/h
    idx_up = hlm_object.network['idx_upstream_link'].to_numpy()
    #network indexes start at 1
    #index 0 is auxiliary to denote no  upstream
    #aux zero will be used for inputs
    #first element of f is zero
    f = [0]
    try:
        for i in np.arange(N)+1:
            coupling_sum = 0
            if (idx_up[i-1] !=0).any():
                #print(i)
                #print(idx_up[i-1])
                for j in idx_up[i-1]:
                    coupling_sum = coupling_sum + y(j)
            #print(coupling_sum)
            f.append(
                (velocity[i-1] / channel_len_m [i-1]) * (-y(i)+coupling_sum)
            )
    except AttributeError as e:
        print('Error creating solver from network topology for row %s'%i)
        print(hlm_object.network.iloc[i])
        print(e)
        quit()

    #if file exists, load solver from existing file
    if exists(hlm_object.pathsolver):
        ODE=jitcode(f,module_location=hlm_object.pathsolver)
    else:
        ODE = jitcode(f)
        ODE.set_integrator("dopri5")
        #ODE.save_compiled(hlm_object.pathsolver)

    ODE.compile_C(omp=(["-openmp"],["-openmp"]))
    ODE.set_integrator("dopri5")
    return ODE

def transfer2(hlm_object):
    t = time.time()
    N = hlm_object.network.shape[0]
    initial_state = np.zeros(shape=(N+1))
    initial_state[1:] = hlm_object.states['volume'].to_numpy()
    hlm_object.ODESOLVER.set_initial_value(initial_state,0.0)
    time1 = hlm_object.time_step_sec / 3600 #hours
    out = hlm_object.ODESOLVER.integrate(time1)[1:]#value 0 is auxiliary
    print(out)
    hlm_object.states['volume'] = out
    hlm_object.states['discharge'] = out / hlm_object.network['channel_length'] * hlm_object.params['river_velocity']
    #hlm_object.states['discharge'] = out / hlm_object.time_step_sec
    print('discharge routing in %f' % (time.time()-t))
    
    
def test2():
    instance = HLM()
    config_file = 'examples/cedarrapids1/cedar_example.yaml'
    instance.init_from_file(config_file)
    instance.advance()