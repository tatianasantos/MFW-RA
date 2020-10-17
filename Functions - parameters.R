# Functions defined for the parameter estimation

# Reporting and payment patterns
# Development factors in the reporting and payment patterns
Pattern_DF <- function(DF){
  PId = rep(0,length(DF))
  PId[1] = DF[1]/prod(DF) #First element for the payment pattern is an exception to the rule
  for(d in 2:length(DF)){
    ProdDF = DF[1:d]
    ProdDF_1 = DF[1:d-1]
    PId[d] = (prod(ProdDF)-prod(ProdDF_1))/prod(DF)
  }
  return(PId)
}


# Reporting pattern based on the claims rates
Reporting_Pattern_CR <- function(Njd, Pj){
  J = nrow(Pj)
  CR = rep(0,J)
  for(d in 0:(J-1)){
    CumNjd = 0
    CumPj = 0
    for(j in 1:(J-d)){
      CumNjd = CumNjd + Njd[[j,d+1]]
      CumPj = CumPj + Pj[[j,1]]
    }
    if(CumPj<=0){CumPj=1}
    CR[d+1] =  CumNjd/CumPj
  }
  TOTCR = CR/sum(CR)
  return(TOTCR)
}


# Payment pattern based on the claims rates
Payment_Pattern_CR <- function(Xjdt, Njd){
  J = nrow(Xjdt)
  CR = rep(0,J)
  for(d in 0:(J-1)){
    CumXjdt = 0
    CumNjd = 0
    for(j in 1:(J-d)){
      CumXjdt = CumXjdt + Xjdt[[j,d+1]]
      CumNjd = CumNjd + Njd[[j,d+1]]
    }
    if(CumNjd<=0){CumNjd=1}
    CR[d+1] =  CumXjdt/CumNjd
  }
  TOTCR = CR/sum(CR)
  return(TOTCR)
}

# Estimate the moments of theta
# We have already estimated the delay probabilities based on the DF and the CR
# 1st approach Method of moments
Method_Moments <- function(CumNjd,Pj,PId){ #The Njd to input must be accumulated
  J = nrow(Pj)
  mom_theta = matrix(nrow = J, ncol = 3)
  SumNjd = 0 
  SumPjPId = 0
  SumNjd2 = 0
  SumPjPId2 = 0
  SumNjd3 = 0
  SumPjPId3 = 0
  for(j in 1:J){
    SumNjd = SumNjd + CumNjd[[j,(J-j+1)]]
    SumPjPId = SumPjPId + Pj[[j,1]]*sum(PId[1:(J-j+1)])
    SumNjd2 = SumNjd2 + CumNjd[[j,(J-j+1)]]*(CumNjd[[j,(J-j+1)]]-1)
    SumPjPId2 = SumPjPId2 + (Pj[[j,1]]*sum(PId[1:(J-j+1)]))^2
    SumNjd3 = SumNjd3 + CumNjd[[j,(J-j+1)]]*(CumNjd[[j,(J-j+1)]]-1)*(CumNjd[[j,(J-j+1)]]-2)
    SumPjPId3 = SumPjPId3 + (Pj[[j,1]]*sum(PId[1:(J-j+1)]))^3
  }
  mu1 = SumNjd/SumPjPId
  mu2 = SumNjd2/SumPjPId2
  mu3 = SumNjd3/SumPjPId3
  mom_theta[,1] = mu1
  mom_theta[,2] = mu2 - mu1^2
  mom_theta[,3] = mu3 -3*mom_theta[2]*mu1 - mu1^3
  return(mom_theta)
}


# 2nd approach Maximum likelihood estimation
# For the MLE we assume that theta follows a Gamma distribution with parameters alpha and beta
LogLik_1 <- function(alpha,beta){
  J = nrow(Cum_Njd_1)
  LOG = 0
  for(j in 1:J){
    LOG_j = alpha * log (beta/(beta+Pj_1[[j,1]]*sum(PId_DF_1[1:(J-j+1)])))
    for(n in 1: Cum_Njd_1[[j,(J-j+1)]]){
      LOG_j = LOG_j + log((alpha+n-1)/(beta+Pj_1[[j,1]]*sum(PId_DF_1[1:(J-j+1)])))
    }
    LOG = LOG + LOG_j
  }
  return(-LOG)
}


# Not working for this data!
# LogLik_2 <- function(alpha,beta){
#   J = nrow(Cum_Njd_2)
#   LOG = 0
#   for(j in 1:J){
#     LOG_j = alpha * log (beta/(beta+Pj_2[[j,1]]*sum(PId_DF_2[1:(J-j+1)])))
#     for(n in 1: Cum_Njd_2[[j,(J-j+1)]]){
#       LOG_j = LOG_j + log((alpha+n-1)/(beta+Pj_2[[j,1]]*sum(PId_DF_2[1:(J-j+1)])))
#     }
#     LOG = LOG + LOG_j
#   }
#   return(-LOG)
# }



# 3rd approach De Vylder's iterative procedure
# For this approach we assume that theta follows a Gamma distribution with parameters alpha and beta
DeVylder <- function(CumNjd, Pj, PId){
  J = nrow(CumNjd)
  CL = rep(0,J)
  Z = rep(0,J)
  Tau0 = 1
  Lambda0 = 1
  Taui = 1
  Lambdai = 1
  error = 1
  while(abs(error)>0.0000000001){
    Tau0 = Taui
    Lambda0 = Lambdai
    for(j in 1:J){
      CL[j] = CumNjd[[j,(J-j+1)]]/(Pj[[j,1]]*sum(PId[1:(J-j+1)]))
      Z[j] = (Pj[[j,1]]*sum(PId[1:(J-j+1)])*Lambda0)/(Pj[[j,1]]*sum(PId[1:(J-j+1)])*Lambda0+Tau0)
    }
    Taui = sum(Z*CL)/sum(Z)
    Lambdai = 1/(J-1)*sum(Z*(CL-Taui)^2)
    error = abs(Taui - Tau0) + abs(Lambdai - Lambda0)
  }
  prior_param = rep(0,2)
  prior_param[1] = (Taui^2)/Lambdai
  prior_param[2] = Taui/Lambdai
  return(prior_param)
}


# Function to get the prior moments of theta
# The function is applied regardless of the method chosen to estimate the prior parameters (MLE or DV)
Prior_Mom <- function(PriorParam,PJ){ #The Njd to input must be accumulated
  J = length(PJ)
  alpha = rep(0,J)
  beta = rep(0,J)
  for(j in 1:J){
    alpha[j] = PriorParam[1]
    beta[j] = PriorParam[2] 
  }
  mom_theta <- data.frame(alpha,beta)
  mom_theta <- mom_theta %>% 
    mutate(mu1 = alpha/beta, mu2 = alpha/beta^2, mu3 = alpha/beta^3) %>% 
    subset(select=-c(alpha,beta))
  return(mom_theta)
}


# Function to get the posterior moments of theta
# The function is applied regardless of the method chosen to estimate the prior parameters (MLE or DV)
Posterior_Mom <- function(PriorParam,CumNjd,Pj,PId){ #The Njd to input must be accumulated
  J = nrow(CumNjd)
  alpha = rep(0,J)
  beta = rep(0,J)
  for(j in 1:J){
    alpha[j] = PriorParam[1] + CumNjd[[j,(J-j+1)]]
    beta[j] = PriorParam[2] + Pj[[j,1]]*sum(PId[1:(J-j+1)]) 
  }
  mom_theta <- data.frame(alpha,beta)
  mom_theta <- mom_theta %>% 
    mutate(mu1 = alpha/beta, mu2 = alpha/beta^2, mu3 = alpha/beta^3) %>% 
    subset(select=-c(alpha,beta))
  return(mom_theta)
}


# Function to obtain the discount factors from the RFR curve
Discount <- function(RFR){
  P = length(RFR)
  for(i in 1:P){
    RFR[i] = (1+RFR[i])^(-i)
  }
  return(RFR)
}


# Function defined to solve for the equivalent single discount rate
# This function can be applied for each case: RBNS, IBNR and CBNI, the arguments need to be defined accordingly
f <- function(CF_UNDISC, CF_DISC, D){
  SUM_CF = 0
 P = length(CF_UNDISC)
  for(t in 1:P){
    SUM_CF = SUM_CF + CF_UNDISC[t]*(D)^(t)
  }
 sum(CF_DISC) - SUM_CF
}
