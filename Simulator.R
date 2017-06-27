##################################
#     SMDE - Queueing Theory     #
#        Evaluating G/G/1        #
#   Eloy Gil - eloy@ac.upc.edu   #
##################################

simulation <- function(number_seed, a, b) {
  
  set.seed(number_seed)
  L = W = Lq = Wq = t = 0
  theta = -Inf
  
  n = 100000
  weibull_numbers = rweibull(100000, a, b)
  erlang_numbers= rgamma(100000, 4, 1/19.75)
  
  LTi = NULL 
  ti = NULL
  
  for (i in 1:n) {
    
    # [1]
    ts = max(theta, t)
    
    # [2] Generate x_1 from the specified distribution
    xi = weibull_numbers[i]
    
    # [3] 
    theta = ts + xi
    
    
    # [5][a]
    wi = li = theta - t 
    L = L + li 
    
    Lt = L / (t)
    
    # Save data for graphic part 
    LTi <- c(LTi,Lt)
    ti <- c(ti, t)
    
    W = W + wi 
    
    # [5][b]
    lqi = wqi = ts - t 
    Lq = Lq + lqi
    Wq = Wq + wqi
    
    # [4]
    if (i < n) {
      tau = erlang_numbers[i] 
      t = t + tau    
      
    } 
  }
  
  # Printing Statistics 
  W = W/n 
  Wq = Wq/n
  #print (paste("Value of W:  " , W))
  #print (paste("Value of Wq:  ", Wq))
  
  L = L / (t)
  Lq = Lq / (t)
  #print (paste("Value of L:  " , L))
  #print (paste("Value of Lq:  ", Lq))
  
  
  
  # Print data LTi vs. i time 
  heading = " LT versus time "
  ti
  LTi
  plot(ti, LTi, type="n", main=heading) 
  
  returnlist = list(W, Wq, L, Lq, LTi, ti)
}



############ 
#   MAIN  #  
############
W_values = NULL
Wq_values = NULL
L_values = NULL
Lq_values = NULL

# Shape, value obtained from assigment list  
a = 0.3135

# Scale,  values obtained at long-tailed-distribution.R script
#b_values <-  c(5.7291727528036, 10.0260523174063, 12.1744920997076, 13.2487119908583)
b_values <-  c(9.54771579960225)
for (b in b_values ) {
  
  print (paste("Execution using b = ", b))
  
  for (j in 1:10) {
    # print (paste("Simulation ", j))
    number_seed = 692*j
    list_ret = simulation(number_seed, a, b)
    
    W = list_ret[[1]]
    Wq = list_ret[[2]]
    L = list_ret[[3]]
    Lq = list_ret[[4]]
    LTi = list_ret[[5]]
    LTi 
    ti = list_ret[[6]]
    
    ti
    W_values <- c(W_values, W)
    Wq_values <- c(Wq_values, Wq)
    L_values <- c(L_values, L)
    Lq_values <- c(Lq_values, Lq)
    
    # print ("-------------------")
  }
  
  # Confidence Interval for Lq and Wq
  mean_wq = mean(Wq_values)
  mean_lq = mean(Lq_values)
  print (paste("W Mean ", mean(W_values)))   # Temps mitjana del sistema  -> 11759 (segons)
  print (paste("Wq Mean ", mean(Wq_values))) # Temps mitjana de cua       -> 11687 (segons)
  print (paste("L Mean ", mean(L_values)))   # Ocupacio mitjana del sistema -> 148.69 (clients)
  print (paste("Lq Mean ", mean(Lq_values))) # Ocupacio mitjana de la cua  -> 147.77 (clients)
  print ("+++++++++++++++++++++")
}
