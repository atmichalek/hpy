from numba import cuda, void, float32  # for parallel gpu computations
import numpy as np  # for random samples and array management
import time  # for timing
import math  # for sinus


@cuda.jit(void(float32[::1], float32[::1]))
def solve_ode(x, time):
    """
    Solve 2DoF ode on gpu, given the initial conditions. The result will be
    stored in the input array.


    Parameters
    ----------
    x : np.array
        Contains initial conditions for the simulations.
        The elements are arranged in pairs:
        [x1_sim1, x2_sim1, x1_sim2, x2_sim2, ...]
    time : np.array
        Three-element list with time details.

    Returns
    -------
    None.

    """
    # time variables
    t = time[0]
    t_end = time[1]
    dt = time[2]

    # index of thread on GPU
    pos = cuda.grid(1)
    # mappping index to access every
    # second element of the array
    pos = pos * 2

    # condidion to avoid threads
    # accessing indices out of array
    if pos < x.size:
        # execute until the time reaches t_end
        while t < t_end:
            # compute derivatives
            dxdt0 = x[pos + 1]
            dxdt1 = (
                np.float32(10.0) * math.sin(t)
                - np.float32(0.1) * x[pos + 1]
                - x[pos]**3
            )

            # update state vecotr
            x[pos] += dxdt0 * dt
            x[pos + 1] += dxdt1 * dt

            # update time
            t += dt


@cuda.jit(void(float32[::1], float32[::1]))
def solve_ode_opt(x, time):
    """
    Solve 2DoF ode on gpu, given the initial conditions. The result will be
    stored in the input array.


    Parameters
    ----------
    x : np.array
        Contains initial conditions for the simulations.
        The elements are arranged in pairs:
        [x1_sim1, x2_sim1, x1_sim2, x2_sim2, ...]
    time : np.array
        Three-element list with time details.

    Returns
    -------
    None.

    """
    # time variables
    t = time[0]
    t_end = time[1]
    dt = time[2]

    # index of thread on GPU
    pos = cuda.grid(1)
    # mappping index to access every
    # second element of the array
    pos = pos * 2

    # condidion to avoid threads
    # accessing indices out of array
    if pos < x.size:
        # execute until the time reaches t_end
        while t < t_end:
            # compute derivatives
            dxdt0 = x[pos + 1]
            xp = x[pos]
            xp3 = xp * xp * xp
            dxdt1 = (
                np.float32(10.0) * math.sin(t)
                - np.float32(0.1) * x[pos + 1]
                - xp3
            )

            # update state vecotr
            x[pos] += dxdt0 * dt
            x[pos + 1] += dxdt1 * dt

            # update time
            t += dt


# number of independent oscillators
# to simulate
trials = 1_000_000

# time variables
t0 = 0
t_end = 200
dt = 0.001
t = np.array([t0, t_end, dt], dtype="float32")

# generate random initial condiotions
init_states = np.random.random_sample(2 * trials).astype("float32")

# manage nr of threads (threads)
threads_per_block = 32
blocks_per_grid = (init_states.size + (threads_per_block - 1)) \
        // threads_per_block

d_init_states = cuda.to_device(init_states)

d_init_states_opt = cuda.to_device(init_states)

d_t = cuda.to_device(t)

# start timer
start = time.perf_counter()

# start parallel simulations
solve_ode[blocks_per_grid, threads_per_block](d_init_states, d_t)
cuda.synchronize()

# measure time elapsed
end = time.perf_counter()


# start timer
start_opt = time.perf_counter()

# start parallel simulations
solve_ode_opt[blocks_per_grid, threads_per_block](d_init_states_opt, d_t)
cuda.synchronize()

# measure time elapsed
end_opt = time.perf_counter()

# Copy results back to host
original_result = d_init_states.copy_to_host()
optimized_result = d_init_states_opt.copy_to_host()

# Sanity check

np.testing.assert_equal(original_result, optimized_result)

print(f"The result was computed in {end-start} s")
print(f"The optimized result was computed in {end_opt-start_opt} s")