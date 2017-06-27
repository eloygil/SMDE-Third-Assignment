##################################
#     SMDE - Queueing Theory     #
#    Long-Tailed distribution    #
#   Eloy Gil - eloy@ac.upc.edu   #
##################################

## Effects of service times following a long-tailed distribution.

a = 0.6556
b = 1
#a = 0.3135
#b = 5 

euler1 = gamma((a+1)/a)
euler2 = gamma((a+2)/a)

# Mean (E[x])
Ex = b*euler1

# Variance (Var[x])
VarX = b^2*(euler2 - euler1^2)
stdVarX = sqrt(VarX)

# Coefficient of variation (C[x])
Cx = stdVarX / Ex



## The effect of long tailed service times on G/G/1

# Rank of Erlang Distribution
k = 2 
e_stage = 32
e_tau = 64
#k = 4 
#e_stage = 19.75
#e_tau = 79

# E[x] = k*e_stage
mean_erlang = e_tau 

# Var[x], sigma² (arrivals)
variance_erlang = k / ((1/e_stage)^2)

# stdVar[x], sigma (arrival)
std_variance_erlang = sqrt(variance_erlang)

# lambda 
lambda = 1/mean_erlang

# Value of a & b for each Traffic Factor
traffic_factor <-  c(0.4, 0.7, 0.85, 0.925)
for (p in traffic_factor) {
    # b, scale parameter
    b = (p*mean_erlang) / euler1
    
    # mhu 
    mhu = 1/(b*euler1)
    
    # theta 
    theta = lambda / mhu
    
    # C(s, theta)
    C = theta
    
    # Var[x], sigma² (service)
    var_weibull = b^2*(euler2 - euler1^2)
    
    # Lq 
    Lq_m = C*p / (1-p)  
    
    Lq = Lq_m*((lambda^2*variance_erlang + mhu^2*var_weibull))/2
    
    # E[x] = Wq, average waiting time 
    #Wq = Lq / lambda
    Wq = (C*(lambda^2*variance_erlang + mhu^2*var_weibull)) / (2*mhu*(1-p))
    
    print(paste("Traffic Factor", p, " a = ", a, " b = ", b, "Var Weibull: ", var_weibull, "Mean: ", 1/mhu, " Wq: ", Wq))
}