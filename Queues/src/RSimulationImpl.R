

computeGamma <- function(a, sum=1) {
  return (gamma((a+sum)/a))
}

computeB <- function(rho, a, expectation_tau=78) {
  
  g <- computeGamma(a)
  b <- (rho*expectation_tau)/g
  #cat("\n-------------------\n B: ", b, "\n-----------\n")
  return (b)
}


a_service = 0.5425

a_arrival = 2
b_arrival = 88

x = 10000   # Num of clients

#values = weibullCumDensity(a=a, b=b)

rhos <- c(0.4, 0.7, 0.8, 0.925)

n = 100000

generateWeibullInstance <- function(a, b, t) {
  
  upper_div = a * (t**(a-1)) * exp((t/b)**a)
  lower_div = b**a  
  
  val = upper_div/lower_div
  if(val == Inf){
    val = .Machine$integer.max
  }
  
  val = rweibull(n = 1, shape = a, scale = b)
  return (val)
}


# ---------------------------------------
#Algorithm execution
for(rho in rhos) {
  
  b_service = computeB(rho, a_service)  
  
  L = 0
  W = 0
  L_q = 0
  W_q = 0
  
  W_vec = c(integer(n))
  L_q_vec = c(integer(n))
  L_t_vec = c(integer(n))
  
  tau_vec = c(integer(n))
  t_vec = c(integer(n))      #Arrivals
  theta_vec = c(integer(n))  #Exits
  x_vec = c(integer(n))
  
  theta_prev =  -(Inf)  # Starts with theta_0
  
  for (i in 1:n) {
    
    t_i_s = max(theta_prev, t_vec[i])
    
    x_vec[i] = generateWeibullInstance(a_service, b_service, t_i_s)
    
    theta_vec[i] = t_i_s + x_vec[i]
    
    
    if(i < n) {
      
      tau_vec[i] = generateWeibullInstance(a_arrival, b_arrival, t_vec[i])
      t_vec[i+1] = t_vec[i] + tau_vec[i]
    }
    
    # ---- STATICS ----
    W_vec[i] = theta_vec[i] - t_vec[i]
    L = L + W_vec[i]
    L_t_vec[i] = (L / (t_vec[i] - t_vec[1])) # Avg waiting time until now
    W = W + W_vec[i]
    
    # b. For queue
    L_q_vec[i] = t_i_s - t_vec[i]
    L_q = L_q + L_q_vec[i]
    W_q = W_q + L_q_vec[i]
    
    if(FALSE) {
      # a. For system
      cat("\n Waiting time on system for client ", i, "=", wi)
      cat("\n Current queue system ", L)
      cat("\n Avg waiting time until now ", L_T_i)
      cat("\n Waiting time until now ", W)
      
      
      cat("\n Waiting time on queue for client ", i, "=", L_q_i)
      cat("\n Avg waiting time until now on QUEUE ", L_q)
      cat("\n Waiting time until now on QUEUE ", W_q)
    }
    
    theta_prev = theta_vec[i]
  }
  
  cat("\n\n-------------\n")
  cat("FINAL REPORT\n")
  cat("-------------")
  cat("\n b ", b_service)
  cat("\n W ", W/n)
  cat("\n Wq", W_q/n)
  cat("\n L", L/(t_vec[n] - t_vec[1]))
  cat("\n L_q", L_q/(t_vec[n] - t_vec[1]))
  
  #Histograms & plots execution
  par(mfrow = c(3, 2))
  statistics <- paste("Mean:", round(mean(L_q_vec),2),"Std. dev.:", round(sd(L_q_vec),2), sep=" ")
  hist(L_q_vec, main=statistics, xlab="sojourn time in queue", ylab="", col = "RED", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  statistics <- paste("Mean:", round(mean(W_vec),2),"Std. dev.:", round(sd(W_vec),2), sep=" ")
  hist(W_vec, main=statistics, xlab="sojourn time in W.S", ylab="", col = "RED", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  statistics <- paste("Mean:", round(mean(tau_vec),2),"Std. dev.:", round(sd(tau_vec),2), sep=" ")
  hist(tau_vec, main=statistics, xlab="Interarrival times", ylab="", col = "RED", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  statistics <- paste("Mean:", round(mean(x_vec),2),"Std. dev.:", round(sd(x_vec),2), sep=" ")
  hist(x_vec, main=statistics, xlab="sojourn service time", ylab="", col = "RED", cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  plot(t_vec,L_t_vec, col="RED", xlab = "time", ylab = "Lt")
  
}


