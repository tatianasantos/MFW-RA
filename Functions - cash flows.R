# Functions to determine the projected cash flows for the RBNs, IBNR and CBNI
# Needed to determine the equivalent single discount rate to be applied
# Write into a vector the cash flows for each of the periods
# Structure of the functions very similar to the general functions defined

cf_rbns <- function(Njd, Vt, G){ 
  J = nrow(Njd)
  CF = rep(0,150)
    for(j in 1:J){
      for(d in 0:(J-j)){
        if((J-(j+d)+2)<=J){
          for(t in (J-(j+d)+1):(J-1)){
            CF[j+d+t-J] = CF[j+d+t-J] + Njd[[j,d+1]]*Vt[t+1]*G[[1]] 
          }
        }
      }
    }
  return(CF)
}


cf_ibnr <- function(Pj, PId, Vt, G, mom_theta){
  J = nrow(Pj)
  CF = rep(0,150) 
  for(j in 1:J){
    d=(J-j+1)
    while(d<=(length(PId)-1)){
      for(t in 0:(J-1)){
        CF[j+d+t-J] = CF[j+d+t-J] + Pj[[j,1]]*PId[d+1]*Vt[t+1]*G[[1]]*mom_theta[[j,1]]
      }
      d = d+1
    }
  }
  return(CF)
}


cf_cbni <- function(PJ, PId, Vt, G, mom_theta){
  J = length(PId)
  CF = rep(0,150) 
  for(j in (J+1):(J+length(PJ))){
    for(d in 0:(J-1)){
      for(t in 0:(J-1)){
        CF[j+d+t-J] = CF[j+d+t-J] + PJ[j-J]*PId[d+1]*Vt[t+1]*G[[1]]*mom_theta[[j-J,1]]
      }
    }
  }
  return(CF)
}


