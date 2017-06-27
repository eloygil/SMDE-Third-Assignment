##########################################
#         SMDE - Queueing Theory         #
#            Evaluating G/G/1            #
#   Albert Cerezo - acerezo@ac.upc.edu   #
#       Eloy Gil - eloy@ac.upc.edu       #
##########################################

## Effects of service times following a long-tailed distribution.

a = 0.6135
b = 4

# Euler 
euler1 = gamma((a+1)/a)
euler2 = gamma((a+2)/a)

Ex = b*euler1                     # Mean (E[x])
VarX = b^2*(euler2 - euler1^2)    # Variance (Var[x])
stdVarX = sqrt(VarX)              # std. dev.
Cx = stdVarX / Ex                 # Coefficient of variation (C[x])


## The effect of long tailed service times on G/G/1.

# Using the assigned Erlang Distribution
k = 4 
e_stage = 19.75
e_tau = 79
mean_erlang = e_tau               # E[x] = k * e_stage
var_erlang = k / ((1/e_stage)^2)  # Var[x], sigma^2 (arrivals)
std_var_erlang = sqrt(var_erlang) # stdVar[x], sigma (arrival)
lambda = 1 / mean_erlang          # lambda

# Value of a & b for each Traffic Factor
tf <-  c(0.4, 0.7, 0.85, 0.925)
for (p in tf) {
  b = (p*mean_erlang) / euler1  # b (scale)
  mhu = 1/(b*euler1)            # mhu 
  theta = lambda / mhu          # theta 
  C = theta                     # C(s, theta)
  var_weibull = b^2*(euler2 - euler1^2)  # Var[x], sigma^2 (service)
  # Lq 
  Lq_m = C*p / (1-p)            
  Lq = Lq_m * ((lambda^2 * var_erlang + mhu^2 * var_weibull))/2
  # E[x] = Wq, average waiting time
  Wq = (C * (lambda^2 * var_erlang + mhu^2 * var_weibull)) / (2 * mhu * (1-p))  
  print(paste("Traffic Factor", p, " b = ", b, " Wq: ", Wq))
}

simulate <- function(seed, a, b, k, e_st, N) {
  L = Lq = W = Wq = t = 0
  LTi = ti = NULL
  theta = -Inf
  WeibullNum = rweibull(N, a, b)
  erlangNum = rgamma(N, k, 1/e_st)
  set.seed(seed)

  for (i in 1:N) {
    ts = max(theta, t)    # 1
    x_i = WeibullNum[i]   # 2
    theta = ts + x_i      # 3

    wi = li = theta - t   # 5a
    L = L + li            #
    Lt = L / t
    
    LTi <- c(LTi, Lt)     # Saving to 
    ti <- c(ti, t)        # plot later
    
    W = W + wi
    
    lqi = wqi = ts - t    # 5b
    Lq = Lq + lqi         # 
    Wq = Wq + wqi         #
    
    if (i < N) {          # 4
      tau = erlangNum[i]  #
      t = t + tau         #
    }                     #
  }
  
  W = W / N
  Wq = Wq / N
  L = L / t
  Lq = Lq / t
  plot(ti, LTi, type="l", main="LT versus time", col="blue")    # Plot data LTi vs. i time
  returnlist = list(W, Wq, L, Lq, LTi, ti)                      # Return results
}

# Initialization 
W_v = Wq_v = L_v = Lq_v = NULL
seed = 157      # Starting seed
N = 100000      # Number of clients
NumTests = 10   # Number of simulations per scale
k = 4           # K
e_st = 19.75    # E(stage)
a = 0.6135      # Shape
b_list = c(21.6068526614056, 37.8119921574598, 45.9145619054869, 49.9658467795004)
b = b_list[4]   # Scale (b factor) obtained using our scr

print(paste("Simulation with b = ", b))
for (j in 1:NumTests) {
  seed = seed + j                             # Refreshing seed value
  result = simulate(seed, a, b, k, e_st, N)   # Starting simulation
  W = result[[1]]                             # Extracting simulation results
  Wq = result[[2]]
  L = result[[3]]
  Lq = result[[4]]
  LTi = result[[5]]
  ti = result[[6]]
  W_v = c(W_v, W)
  Wq_v = c(Wq_v, Wq)
  L_v = c(L_v, L)
  Lq_v = c(Lq_v, Lq)
}

# Printing simulation results
print(paste("W mean: ", mean(W_v), "| Std dev.: ", sd(W_v)))    # Avg time in the system and its std. dev.
print(paste("Wq mean: ", mean(Wq_v), "| Std dev.: ", sd(Wq_v))) # Avg time in the queue and its std. dev.
print(paste("L mean: ", mean(L_v), "| Std dev.: ", sd(L_v)))    # Avg system occupation and its std. dev.
print(paste("Lq mean: ", mean(Lq_v), "| Std dev.: ", sd(Lq_v))) # Avg queue occupation and its std. dev.