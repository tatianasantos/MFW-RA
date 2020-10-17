# General functions for the MFW "USE OF THE NP-APPROXIMATION TO DETERMINE THE RISK ADJUSTMENT UNDER IFRS 17 IN A NON-LIFE PORTFOLIO"

# Function to determine the RA for the LIC at a defined confidence level
# CL is the confidence leve specified by the user
# Remaining arguments defined based on other functions
# Equation 1
ra_lic <- function(CL, MOM_RBNS, MOM_IBNR){
  MOM_LIC = MOM_RBNS + MOM_IBNR
  RA_LIC = qnorm(CL)*sqrt(MOM_LIC[2])+((qnorm(CL)^2-1)*MOM_LIC[3])/(6*MOM_LIC[2])
  return(RA_LIC)
}

# Equation 2
ra_lrc <- function(CL, MOM_CBNI){
  RA_LRC = qnorm(CL)*sqrt(MOM_CBNI[2])+((qnorm(CL)^2-1)*MOM_CBNI[3])/(6*MOM_CBNI[2])
  return(RA_LRC)
}


# Functions that define the moments of the severity distribution for all possible values of t
# Function is defined to return a cumulative matrix, i.e., the first row will be the sum for all values of t
# Each column represents the order of the moment: 1st, 2nd and 3rd respectively
cmom_cp <- function(D, Vt, G){ 
  J = length(Vt)
  mom = matrix(data=0, nrow=J, ncol=3)
  for(s in 1:3){
    for(i in 1:J){
      for(t in (i-1):(J-1)){
        mom[i,s] = mom[i,s] + D^(s*t)*Vt[t+1]*G[[s]] #need to adjust the index for Vt since t starts at zero
      }
    }
  }
  return(mom)
}

cmom_mult <- function(D, Vt, G){
  J = length(Vt)
  mom_raw = matrix(data=0, nrow=J, ncol=3)
  mom = matrix(data=0, nrow=J, ncol=3)
  for(s in 1:3){
    for(i in 1:J){
      for(t in (i-1):(J-1)){
        mom_raw[i,s] = mom_raw[i,s] + D^(s*t)*Vt[t+1]*G[[s]] #need to adjust the index for Vt since t starts at zero
      }
    }
  }
  mom[,1] = mom_raw[,1]
  mom[,2] = mom_raw[,2]-mom_raw[,1]^2
  mom[,3] = mom_raw[,3]-3*mom_raw[,2]*mom_raw[,1]+2*mom_raw[,1]^3
  return(mom)
}

cmom_dir <- function(D, Delta, G){
  J = length(Delta)
  mom_raw = matrix(data=0, nrow=J, ncol=3)
  mom = matrix(data=0, nrow=J, ncol=3)
  TOTDelta = sum(Delta)
  for(i in 1:J){
    for(t in (i-1):(J-1)){
      k = i-1
      Deltat <- Delta[(k+1):length(Delta)] #deltat is fixed at a certain t, the first we consider
      TOTDeltat = sum(Deltat)
      mom_raw[i,1] = mom_raw[i,1] + G[[1]]*Delta[t+1]*D^t/TOTDelta
      mom_raw[i,2] = mom_raw[i,2] + (TOTDeltat+1)*G[[2]]*Delta[t+1]*D^(2*t)/((TOTDelta+1)*TOTDelta)
      mom_raw[i,3] = mom_raw[i,3] + (TOTDeltat+2)*(TOTDeltat+1)*G[[3]]*Delta[t+1]*D^(3*t)/((TOTDelta+2)*(TOTDelta+1)*TOTDelta)
    }
  }
  mom[,1] = mom_raw[,1]
  mom[,2] = mom_raw[,2]-mom_raw[,1]^2
  mom[,3] = mom_raw[,3]-3*mom_raw[,2]*mom_raw[,1]+2*mom_raw[,1]^3
  return(mom)  
}


# Functions used for the moments of the IBNR (disregarding theta)
# Equation 7
A <- function(D, Pj, PId, cmom){
  J = nrow(Pj)
  a = rep(0,J)
  for(j in 1:J){
    d=(J-j+1)
    while(d<=(length(PId)-1)){
      a[j] = a[j] + D^(j+d-J)*Pj[[j,1]]*PId[d+1]*cmom[[1,1]]
      d = d+1
    }
  }
  return(a)
}

# Equation 8
B <- function(D, Pj, PId, cmom){
  J = nrow(Pj)
  b = rep(0,J)
  for(j in 1:J){
    d=(J-j+1)
    while(d<=(length(PId)-1)){
      b[j] = b[j] + D^(2*(j+d-J))*Pj[[j,1]]*PId[d+1]*(cmom[[1,2]]+cmom[[1,1]]^2)
      d = d+1
    }
  }
  return(b)
}

# Equation 9
C <- function(D, Pj, PId, cmom){
  J = nrow(Pj)
  c = rep(0,J)
  for(j in 1:J){
    d=(J-j+1)
    while(d<=(length(PId)-1)){
      c[j] = c[j] + D^(3*(j+d-J))*Pj[[j,1]]*PId[d+1]*(cmom[[1,3]]+3*cmom[[1,1]]*cmom[[1,2]]+cmom[[1,1]]^3)
      d = d+1
    }
  }
  return(c)
}


# Functions used for the moments of the CBNI (disregarding theta)
# Equation 16
A_J <- function(D, PJ, PId, cmom){
  J = length(PId)
  a = rep(0,length(PJ))
  for(j in (J+1):(J+length(PJ))){
    for(d in 0:(J-1)){
      a[j-J] = a[j-J] + D^(j+d-J)*PJ[j-J]*PId[d+1]*cmom[[1,1]]
    }
  }
  return(a)
}

# Equation 17
B_J <- function(D, PJ, PId, cmom){
  J = length(PId)
  b = rep(0,length(PJ))
  for(j in (J+1):(J+length(PJ))){
    for(d in 0:(J-1)){
      b[j-J] = b[j-J] + D^(2*(j+d-J))*PJ[j-J]*PId[d+1]*(cmom[[1,2]]+cmom[[1,1]]^2)
    }
  }
  return(b)
}

# Equation 18
C_J <- function(D, PJ, PId, cmom){
  J = length(PId)
  c = rep(0,length(PJ))
  for(j in (J+1):(J+length(PJ))){
    for(d in 0:(J-1)){
      c[j-J] = c[j-J] + D^(3*(j+d-J))*PJ[j-J]*PId[d+1]*(cmom[[1,3]]+3*cmom[[1,1]]*cmom[[1,2]]+cmom[[1,1]]^3)
    }
  }
  return(c)
}


# Functions to determine the 3 moments needed for each of the cases: RBNS, IBNR and CBNI
# To call this function besides the data regarding the number of reported claims, the moments of the severity distribution need to be already calculated
# This function works for each case considered: CP, Mult and Dir, we just need to call the appropriate CMOM
# Equation 3
mom_rbns_gen <- function(D, Njd, CMOM){ 
  J = nrow(Njd)
  mom = rep(0,3)
  for(s in 1:3){
    for(j in 1:J){
      for(d in 0:(J-j)){
        if((J-(j+d)+2)<=J){
          mom[s] = mom[s] + D^(s*(j+d-J))*Njd[[j,d+1]]*CMOM[[(J-(j+d)+2),s]] #since d starts at zero to access the first column we need to sum 1
          #for the cmom we need to add 2 in the index: 1 to correct the index since d starts at zero and another since the sign is higher than  
        }
      }
    }
  }
  return(mom)
}


# This function assumes that the estimation of theta follows one of the first two approaches mentioned
# The moments defined are conditional on theta
# Equations 4, 5 and 6
mom_ibnr_cond <- function(D, Pj, PId, THETA, cmom){
  J = nrow(Pj)
  mom = rep(0,3)
  for(j in 1:J){
      mom[1] = mom[1] + A(D, Pj, PId, cmom)[j]*THETA
      mom[2] = mom[2] + B(D, Pj, PId, cmom)[j]*THETA
      mom[3] = mom[3] + C(D, Pj, PId, cmom)[j]*THETA
  }
  return(mom)
}

# This function assumes that the estimation of theta is performed based on a Bayesian approach
# The moments defined are unconditional on theta
# Equations 10, 11 and 12
mom_ibnr_gen <- function(D, Pj, PId, mom_theta, cmom){
  J = nrow(Pj)
  mom = rep(0,3)
  for(j in 1:J){
    mom[1] = mom[1] + A(D, Pj, PId, cmom)[j]*mom_theta[[j,1]]
    mom[2] = mom[2] + B(D, Pj, PId, cmom)[j]*mom_theta[[j,1]] +
      (A(D, Pj, PId, cmom)[j])^2*mom_theta[[j,2]]
    mom[3] = mom[3] + C(D, Pj, PId, cmom)[j]*mom_theta[[j,1]] +
      3*B(D, Pj, PId, cmom)[j]*A(D, Pj, PId, cmom)[j]*mom_theta[[j,2]] +
      2*(A(D, Pj, PId, cmom)[j])^3*mom_theta[[j,3]]
  }
  return(mom)
}



# This function assumes that the estimation of theta follows one of the first two approaches mentioned
# The moments defined are conditional on theta
# Equations 13, 14 and 15
mom_cbni_cond <- function(D, PJ, PId, THETA, cmom){
  J = length(PJ)
  mom = rep(0,3)
  for(j in 1:J){
    mom[1] = mom[1] + A_J(D, PJ, PId, cmom)[j]*THETA
    mom[2] = mom[2] + B_J(D, PJ, PId, cmom)[j]*THETA
    mom[3] = mom[3] + C_J(D, PJ, PId, cmom)[j]*THETA
  }
  return(mom)
}


# This function assumes that the estimation of theta is performed based on a Bayesian approach
# The moments defined are unconditional on theta
# Equations 19, 20 and 21
mom_cbni_gen <- function(D, PJ, PId, mom_theta, cmom){
  J = length(PJ)
  mom = rep(0,3)
  for(j in 1:J){
    mom[1] = mom[1] + A_J(D, PJ, PId, cmom)[j]*mom_theta[[j,1]]
    mom[2] = mom[2] + B_J(D, PJ, PId, cmom)[j]*mom_theta[[j,1]] + 
      (A_J(D, PJ, PId, cmom)[j])^2*mom_theta[[j,2]]
    mom[3] = mom[3] + C_J(D, PJ, PId, cmom)[j]*mom_theta[[j,1]] +
      3*B_J(D, PJ, PId, cmom)[j]*A_J(D, PJ, PId, cmom)[j]*mom_theta[[j,2]] + 
      2*(A_J(D, PJ, PId, cmom)[j])^3*mom_theta[[j,3]]
  }
  return(mom)
}

