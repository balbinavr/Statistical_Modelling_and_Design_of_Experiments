
weibullCumDensity <- function(x_max=10000, a, b) {
  
  values = c()
  y_vals = c()
  for (x in 1:x_max) {
    
    cum_f = 1 - exp(-((x/b)**a))
    
    values = c(values, cum_f);
    y_vals = c(y_vals, x)
  }
  return (data.frame(x=y_vals,y=values))
}

weibullRandomGenerator <- function(a, b, numInstances, a_arrival, b_arrival) {
  
  values = c()
  
  for(x in 1:numInstances){
    t = rweibull(n = 1, shape = a_arrival, scale = b_arrival)
    upper_div = a * (t**(a-1)) * exp((t/b)**a)
    lower_div = b**a  
    
    values = c(values, upper_div/lower_div)
  }
  return (values)
}

computeGamma <- function(a, sum=1) {
  return (gamma((a+sum)/a))
}

computeExpectation_X <- function(b, a) {
  return (b*computeGamma(a=a))
}

computeB <- function(rho, a, expectation_tau=64) {
  
  g <- computeGamma(a)
  b <- (rho*expectation_tau)/g
  cat("\n-------------------\n B: ", b, "\n-----------\n")
  return (b)
}

#Computes weibull var
computeVar <- function(b, a) {
  
  pow_b = b**2
  first_gamma = (computeGamma(a=a, sum=2))
  second_gamma = (computeGamma(a=a, sum=1))
  pow_second_gamma = second_gamma ** 2
  
  if(FALSE){
    cat("Pow b:", pow_b, "\n")
    cat("first_gamma:", first_gamma, "\n")
    cat("second_gamma:", second_gamma, "\n")
    cat("Poq second_gamma:", pow_second_gamma, "\n")
  }
  return (pow_b * (first_gamma - pow_second_gamma))
}


computeExpectation_tau <-function(i1, i2){
  
  return (1/12 * ((i2 - i1)**2))
}

computeWq <- function(lambda, mu, var_tau, var_x, rho, s) {
  
  C_s_theta_upper = rho**s/(factorial(s)*(1-rho))
  C_s_theta_lower = 1 + C_s_theta_upper
  
  C_s_theta = C_s_theta_upper / C_s_theta_lower
  
  upper_div = C_s_theta * ((lambda**2 * var_tau) + (mu**2 * var_x))
  lower_div = 2 * s * mu * (1-rho)
  
  return (upper_div/lower_div)
}

computeLq <- function(Wq, lambda) {
  return (Wq * lambda)
}


generateRandomUnif <- function(n, a, b) {
  return (runif(n, a, b))
}

# a = shape
# b = scale
generateRandomWeibull <- function(n, a, b) {
  return(rweibull(n, a, b))
}

createCustomDataFrame <- function() {
  
  rm(df)
  #theta not added because it is always the same as rho (1 server)
  # E_x = expectation service
  # E_tau = expectation arrival
  # a,b from service
  df <- data.frame(
    rho=numeric(),
    a = numeric(),
    b = numeric(),
    E_tau = numeric(),
    E_x = numeric(),
    var_tau = numeric(),
    var_x = numeric(),
    std_dev_tau = numeric(),
    std_dev_x = numeric(),
    lambda = numeric(),
    mu = numeric(),
    Wq = numeric(),
    Lq = numeric()
  )
  
  return (df)
}

applyCustomNamesToDF <- function(df) {
  colnames(df) <- c(
    "rho",
    "a" ,
    "b",
    "E_tau",
    "E_x",
    "var_tau",
    "var_x",
    "std_dev_tau",
    "std_dev_x",
    "lambda",
    "mu",
    "Wq",
    "Lq")
  
  return (df)
}


# #######################################
# -------------- PARAMS -----------------
# #######################################

#a = 0.3263      # OLD param!
#exp_tau = 64    # OLD param!

a = 0.5425
exp_tau = 78

x = 10000   # Num of clients

#values = weibullCumDensity(a=a, b=b)

s = 1

i1 = 2
i2 = 88
rhos <- c(0.4, 0.7, 0.8, 0.925)

var_tau = computeVar(a = i1, b = i2)

df <- createCustomDataFrame()

# ##########################################
# #######   PREPARE TABLE   #############
# ##########################################
par(mfrow = c(2, 2), las = 1, font.main = 4, font.lab = 4, font.axis = 2, oma = c(0, 0, 1, 0))

for (rho in rhos){
  
  cat("rho = ", rho, ", ")
  b = computeB(rho=rho, a=a, expectation_tau=exp_tau)
  e_x = computeExpectation_X(b=b, a=a)
  var_x = computeVar(a=a, b=b)
  
  cat("E[x]:", e_x, "\n")
  cat("Var[x]:", var_x, "\n")
  cat("--------------\n")
  
  Wq = computeWq(lambda=1/exp_tau, mu=1/e_x, var_tau=var_tau, var_x=var_x, rho=rho, s=s)
  Lq = computeLq(Wq, 1/exp_tau)
  
  df = rbind(df, c(rho, a,  b, exp_tau, e_x, var_tau, var_x, sqrt(var_tau), sqrt(var_x), 1/exp_tau, 1/e_x, Wq, Lq))
  values = weibullCumDensity(a=a, b=b, x_max = x) ## Cumulative
  
  #hist(values$y)
  plot(values$x, values$y, col="PINK")
  #lines(values$x, values$y, col="RED")
}

df <- applyCustomNamesToDF(df)
View(df)


# ##########################################
# #######   Plots Allen Cuneen #############
# #######   Wq and Lq charts   #############
# ##########################################

par(mfrow = c(1, 2))
plot(c(0.4, 0.7, 0.8, 0.925), df$Wq, type = "o", col = "red", xlab = "rho", ylab = "Wq value",
     main = "Wq chart")

plot(c(0.4, 0.7, 0.8, 0.925), df$Lq, type = "o", col = "blue", xlab = "rho", ylab = "Lq value",
     main = "Lq chart")




# ##########################################
# #######   Plots Hist and stats #############
# #######  for n = 10 or n = 100k  #############
# ##########################################

i1 = 2
i2 = 88

a = 0.5425
rhos <- c(0.4, 0.7, 0.8, 0.925)
exp_tau = 78
k = 10000
#k = 100000
par(mfrow = c(2, 2), mar = rep(2, 4))
titles = c("Rho 0.4", "Rho 0.7", "Rho 0.8", "Rho 0.925" )
r = 1
simdf = data.frame()
for (rho in rhos) {
    b = computeB(rho=rho, a=a, expectation_tau=exp_tau)
    
    
    weib = weibullRandomGenerator(a, b, k, i1, i2)
    wdf = data.frame(values = weib, y = val)
    hist(wdf$values, wdf$y, col="RED", breaks = 50, main = titles[r], xlab = "values")
    
    simdf = rbind(simdf, c(rho, mean(weib), median(weib), sd(weib)))
    r = r+1
}



# ##########################################
# #######   Plot weibull rand. #############
# #######       generator      #############
# ##########################################   
    
k = 10000
rweib = generateRandomWeibull(k, a, b)
val = c(1:k)
rwdf = data.frame(values = rweib, y = val)

hist(rwdf$values, rwdf$y, col="RED", breaks = 50)


par(mfrow = c(1, 1))
plot(wdf$y, wdf$values, col = "red", xlab = "rho", ylab = "Wq value",
   main = "Wq chart")
