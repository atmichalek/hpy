from scipy.integrate import solve_ivp
from matplotlib import pyplot as plt
import numpy as np
def test1():
    '''
    #v[0] = V11'(s) = -12*v12(s)**2 
    #v[1] v22'(s) = 12*v12(s)**2  
    #v[2] v12'(s) = 6*v11(s)*v12(s) - 6*v12(s)*v22(s) - 36*v12(s) 
    '''
    def fun(s,v):
        return [-12*v[2]**2 , 12*v[2]**2, 6*v[0]*v[2]-6*v[2]*v[1] - 36*v[2]]

    res = solve_ivp(fun,
        t_span=(0,0.1),
        y0=[2,3,4]
    )
    res['y'].shape
    plt.plot(res['t'],res['y'][0])
    plt.plot(res['t'],res['y'][1])
    plt.plot(res['t'],res['y'][2])
    plt.show()

def test2():
    #channel 1 and 2 drain to 3
    def fun(t,q):
        v0=0.3
        lambda1=0.33
        lambda2=0.1
        A_i=100     #km2
        L_i = 100   #m
        invtau = (v0*A_i**lambda2) /((1.0 - lambda1)*L_i)
        return [invtau*(q[0]**lambda1)*(-1*q[0]),
                invtau*(q[1]**lambda1)*(-1*q[1]),
                invtau*(q[2]**lambda1)*(-1*q[2]+q[0]+q[1])
        ]

    res = solve_ivp(fun,t_span=(0,100),y0=[100,110,100])
    plt.plot(res['t'],res['y'][0])
    plt.plot(res['t'],res['y'][1])
    plt.plot(res['t'],res['y'][2])
    plt.show()

def test3():
    #channel 1 and 2 drain to 3
    def fun(t,q,v0,lambda1,lambda2,A_i,L_i):
        invtau = (v0*A_i**lambda2) /((1.0 - lambda1)*L_i)
        return [invtau*(q[0]**lambda1)*(-1*q[0]),
                invtau*(q[1]**lambda1)*(-1*q[1]),
                invtau*(q[2]**lambda1)*(-1*q[2]+q[0]+q[1])
        ]
    args=(.3,.33,.1,100,200)
    res = solve_ivp(fun,t_span=(0,0.1),y0=[100,110,100],args=args)


def test3():
    #channel 1 and 2 drain to 3
    def fun(t,q,v0,lambda1,lambda2,A_i,L_i):
        #invtau = (v0*A_i**lambda2) /((1.0 - lambda1)*L_i)
        invtau = np.divide(np.multiply(v0,np.power(A_i,lambda2)),np.multiply(np.subtract(1,lambda1),L_i))
        print(q.shape)
        return np.multiply(np.power(np.multiply(invtau,q),lambda1),(-1*q))
"""         return [invtau*(q[0]**lambda1)*(-1*q[0]),
                invtau*(q[1]**lambda1)*(-1*q[1]),
                invtau*(q[2]**lambda1)*(-1*q[2]+q[0]+q[1])
        ]
 """        
    v0=np.array([.3, .4 , .5])
    lambda1=np.array([0.33,.33,.33])
    lambda2=np.array([.1,.1,.1])
    A_i=np.array([100,100,100])    #km2
    L_i = np.array([100,100,100])   #m
    #invtau = np.divide(np.multiply(v0,np.power(A_i,lambda2)),np.multiply(np.subtract(1,lambda1),L_i)) 
    args1 = (v0,lambda1,lambda2,A_i,L_i)
    args2=(.3,.33,.1,100,200)
    res = solve_ivp(fun,t_span=(0,0.1),y0=[100,110,100],args=args1,)
    res = solve_ivp(fun,t_span=(0,0.1),y0=[100,110,100],args=args2,)
    plt.plot(res['t'],res['y'][0])
    plt.plot(res['t'],res['y'][1])
    plt.plot(res['t'],res['y'][2])
    plt.show()


