# Functions to present the results 

# Data frame that presents for different confidence levels from 50% to 99.5% the RA for the LIC for the compound Poisson, Multinomial and Dirichlet
ra_lic_cl <- function(mom_rbns_cp, mom_rbns_mult, mom_rbns_dir, mom_ibnr_cp, mom_ibnr_mult, mom_ibnr_dir){
  CL = seq(0.5, 0.995, 0.01)
  RA_LIC = as.data.frame(matrix(0,nrow = length(CL), ncol = 4))
  for(i in 1:length(CL)){
    RA_LIC[i,1] = CL[i]
    RA_LIC[i,2] = ra_lic(CL[i],mom_rbns_cp,mom_ibnr_cp)
    RA_LIC[i,3] = ra_lic(CL[i],mom_rbns_mult,mom_ibnr_mult)
    RA_LIC[i,4] = ra_lic(CL[i],mom_rbns_dir,mom_ibnr_dir)
  }
  colnames(RA_LIC) <- c("CL","CP","MULT","DIR")
  return(RA_LIC)
}


# Data frame that presents for different confidence levels from 50% to 99.5% the RA for the LRC for the compound Poisson, Multinomial and Dirichlet
ra_lrc_cl <- function(mom_cbni_cp, mom_cbni_mult, mom_cbni_dir){
  CL = seq(0.5, 0.995, 0.01)
  RA_LRC = as.data.frame(matrix(0,nrow = length(CL), ncol = 4))
  for(i in 1:length(CL)){
    RA_LRC[i,1] = CL[i]
    RA_LRC[i,2] = ra_lrc(CL[i],mom_cbni_cp)
    RA_LRC[i,3] = ra_lrc(CL[i],mom_cbni_mult)
    RA_LRC[i,4] = ra_lrc(CL[i],mom_cbni_dir)
  }
  colnames(RA_LRC) <- c("CL","CP","MULT","DIR")
  return(RA_LRC)
}
