# Libraries used in the program
library(openxlsx) # retrieve data from an excel file
library(tidyverse) # organize the data 
library(moments) # determine the moments based on empirical information
library(ChainLadder) # estimate the development factors (function ata)
library(ggplot2) # visual analysis of the data
library(rootSolve) # solve for the single discount rate
library(stats4) # call function mle
library(formattable) # format data
library(plotly) # present the results in a graphic

source("Functions - general.R")
source("Functions - cash flows.R")
source("Functions - parameters.R")
source("Functions - results.R")
source("Data.R")


# Risk group 5
# Determine the severity distribution moments based on the CP
CMOM_RBNS_CP_1 <- cmom_cp(D_RBNS_1,Vt_DF_1,G_1_CP)
CMOM_IBNR_CP_1 <- cmom_cp(D_IBNR_1,Vt_DF_1,G_1_CP)
CMOM_CBNI_CP_1 <- cmom_cp(D_CBNI_1,Vt_DF_1,G_1_CP)

# Determine the moments based on the CP
MOM_RBNS_CP_1 <- mom_rbns_gen(D_RBNS_1,Njd_1,CMOM_RBNS_CP_1)
MOM_IBNR_CP_1 <- mom_ibnr_gen(D_IBNR_1,Pj_1,PId_DF_1,Post_Mom_DV_1,CMOM_IBNR_CP_1)
MOM_CBNI_CP_1 <- mom_cbni_gen(D_CBNI_1,PJ_1,PId_DF_1,Prior_Mom_DV_1,CMOM_CBNI_CP_1)

#Determine the RA based on the CP
RA_LIC_CP_1 <- ra_lic(0.75,MOM_RBNS_CP_1,MOM_IBNR_CP_1)
RA_LRC_CP_1 <- ra_lrc(0.75,MOM_CBNI_CP_1)


# Determine the severity distribution moments based on the multinomial
CMOM_RBNS_MULT_1 <- cmom_mult(D_RBNS_1,Vt_DF_1,G_1)
CMOM_IBNR_MULT_1 <- cmom_mult(D_IBNR_1,Vt_DF_1,G_1)
CMOM_CBNI_MULT_1 <- cmom_mult(D_CBNI_1,Vt_DF_1,G_1)

# Determine the moments based on the multinomial
MOM_RBNS_MULT_1 <- mom_rbns_gen(D_RBNS_1,Njd_1,CMOM_RBNS_MULT_1)
MOM_IBNR_MULT_1 <- mom_ibnr_gen(D_IBNR_1,Pj_1,PId_DF_1,Post_Mom_DV_1,CMOM_IBNR_MULT_1)
MOM_CBNI_MULT_1 <- mom_cbni_gen(D_CBNI_1,PJ_1,PId_DF_1,Prior_Mom_DV_1,CMOM_CBNI_MULT_1)

# Determine the RA based on the multinomial
RA_LIC_MULT_1 <- ra_lic(0.75,MOM_RBNS_MULT_1,MOM_IBNR_MULT_1)
RA_LRC_MULT_1 <- ra_lrc(0.75,MOM_CBNI_MULT_1)


#Determine the severity distribution moments based on the Dirichlet
#Assume that delta=1 so that the payment pattern for the Dirichlet deltat is the same as for the other models vt
#The delta chosen impacts the volatility of the model
CMOM_RBNS_DIR_1 <- cmom_dir(D_RBNS_1,Vt_DF_1,G_1)
CMOM_IBNR_DIR_1 <- cmom_dir(D_IBNR_1,Vt_DF_1,G_1)
CMOM_CBNI_DIR_1 <- cmom_dir(D_CBNI_1,Vt_DF_1,G_1)

#Determine the moments based on the Dirichlet
MOM_RBNS_DIR_1 <- mom_rbns_gen(D_RBNS_1,Njd_1,CMOM_RBNS_DIR_1)
MOM_IBNR_DIR_1 <- mom_ibnr_gen(D_IBNR_1,Pj_1,PId_DF_1,Post_Mom_DV_1,CMOM_IBNR_DIR_1)
MOM_CBNI_DIR_1 <- mom_cbni_gen(D_CBNI_1,PJ_1,PId_DF_1,Prior_Mom_DV_1,CMOM_CBNI_DIR_1)

#Determine the RA based on the Dirichlet
RA_LIC_DIR_1 <- ra_lic(0.75,MOM_RBNS_DIR_1,MOM_IBNR_DIR_1)
RA_LRC_DIR_1 <- ra_lrc(0.75,MOM_CBNI_DIR_1)



# Risk group 6
# Determine the severity distribution moments based on the CP
CMOM_RBNS_CP_2 <- cmom_cp(D_RBNS_2,Vt_DF_2,G_2_CP)
CMOM_IBNR_CP_2 <- cmom_cp(D_IBNR_2,Vt_DF_2,G_2_CP)
CMOM_CBNI_CP_2 <- cmom_cp(D_CBNI_2,Vt_DF_2,G_2_CP)

# Determine the moments based on the CP
MOM_RBNS_CP_2 <- mom_rbns_gen(D_RBNS_2,Njd_2,CMOM_RBNS_CP_2)
MOM_IBNR_CP_2 <- mom_ibnr_gen(D_IBNR_2,Pj_2,PId_DF_2,Post_Mom_DV_2,CMOM_IBNR_CP_2)
MOM_CBNI_CP_2 <- mom_cbni_gen(D_CBNI_2,PJ_2,PId_DF_2,Prior_Mom_DV_2,CMOM_CBNI_CP_2)

#Determine the RA based on the CP
RA_LIC_CP_2 <- ra_lic(0.75,MOM_RBNS_CP_2,MOM_IBNR_CP_2)
RA_LRC_CP_2 <- ra_lrc(0.75,MOM_CBNI_CP_2)


# Determine the severity distribution moments based on the multinomial
CMOM_RBNS_MULT_2 <- cmom_mult(D_RBNS_2,Vt_DF_2,G_2)
CMOM_IBNR_MULT_2 <- cmom_mult(D_IBNR_2,Vt_DF_2,G_2)
CMOM_CBNI_MULT_2 <- cmom_mult(D_CBNI_2,Vt_DF_2,G_2)

# Determine the moments based on the multinomial
MOM_RBNS_MULT_2 <- mom_rbns_gen(D_RBNS_2,Njd_2,CMOM_RBNS_MULT_2)
MOM_IBNR_MULT_2 <- mom_ibnr_gen(D_IBNR_2,Pj_2,PId_DF_2,Post_Mom_DV_2,CMOM_IBNR_MULT_2)
MOM_CBNI_MULT_2 <- mom_cbni_gen(D_CBNI_2,PJ_2,PId_DF_2,Prior_Mom_DV_2,CMOM_CBNI_MULT_2)

# Determine the RA based on the multinomial
RA_LIC_MULT_2 <- ra_lic(0.75,MOM_RBNS_MULT_2,MOM_IBNR_MULT_2)
RA_LRC_MULT_2 <- ra_lrc(0.75,MOM_CBNI_MULT_2)


#Determine the severity distribution moments based on the Dirichlet
#Assume that delta=1 so that the payment pattern for the Dirichlet deltat is the same as for the other models vt
#The delta chosen impacts the volatility of the model
CMOM_RBNS_DIR_2 <- cmom_dir(D_RBNS_2,Vt_DF_2,G_2)
CMOM_IBNR_DIR_2 <- cmom_dir(D_IBNR_2,Vt_DF_2,G_2)
CMOM_CBNI_DIR_2 <- cmom_dir(D_CBNI_2,Vt_DF_2,G_2)

#Determine the moments based on the Dirichlet
MOM_RBNS_DIR_2 <- mom_rbns_gen(D_RBNS_2,Njd_2,CMOM_RBNS_DIR_2)
MOM_IBNR_DIR_2 <- mom_ibnr_gen(D_IBNR_2,Pj_2,PId_DF_2,Post_Mom_DV_2,CMOM_IBNR_DIR_2)
MOM_CBNI_DIR_2 <- mom_cbni_gen(D_CBNI_2,PJ_2,PId_DF_2,Prior_Mom_DV_2,CMOM_CBNI_DIR_2)

#Determine the RA based on the Dirichlet
RA_LIC_DIR_2 <- ra_lic(0.75,MOM_RBNS_DIR_2,MOM_IBNR_DIR_2)
RA_LRC_DIR_2 <- ra_lrc(0.75,MOM_CBNI_DIR_2)



# Presentation of the results
# Data frame with the mean obtained for each case: RBNS, IBNR and CBNI
Summary_mean <- data.frame("RBNS" = c(CF_RBNS_DISC_1, CF_RBNS_DISC_2), 
                           "IBNR" = c(CF_IBNR_DISC_1, CF_IBNR_DISC_2),
                           "CBNI" = c(CF_CBNI_DISC_1, CF_CBNI_DISC_2))
Summary_mean <- Summary_mean %>% mutate(IBNS = RBNS + IBNR)

#Data frame with the elements of the Risk Adjustment separately: SD adjustment and skewness adjustment
Summary_85_CP <- data.frame("Element"=c("SD_1", "Skewness_1","SD_2", "Skewness_2"),
                            "RBNS"=c(qnorm(0.85)*sqrt(MOM_RBNS_CP_1[2]),
                                     ((qnorm(0.85)^2-1)*MOM_RBNS_CP_1[3])/(6*MOM_RBNS_CP_1[2]),
                                     qnorm(0.85)*sqrt(MOM_RBNS_CP_2[2]),
                                     ((qnorm(0.85)^2-1)*MOM_RBNS_CP_2[3])/(6*MOM_RBNS_CP_2[2])),
                            "IBNR"=c(qnorm(0.85)*sqrt(MOM_IBNR_CP_1[2]),
                                     ((qnorm(0.85)^2-1)*MOM_IBNR_CP_1[3])/(6*MOM_IBNR_CP_1[2]),
                                     qnorm(0.85)*sqrt(MOM_IBNR_CP_2[2]),
                                     ((qnorm(0.85)^2-1)*MOM_IBNR_CP_2[3])/(6*MOM_IBNR_CP_2[2])),
                            "IBNS"=c(qnorm(0.85)*sqrt(MOM_RBNS_CP_1[2]+MOM_IBNR_CP_1[2]),
                                     ((qnorm(0.85)^2-1)*(MOM_RBNS_CP_1[3]+MOM_IBNR_CP_1[3]))/(6*(MOM_RBNS_CP_1[2]+MOM_IBNR_CP_1[2])),
                                     qnorm(0.85)*sqrt(MOM_RBNS_CP_2[2]+MOM_IBNR_CP_2[2]),
                                     ((qnorm(0.85)^2-1)*(MOM_RBNS_CP_2[3]+MOM_IBNR_CP_2[3]))/(6*(MOM_RBNS_CP_2[2]+MOM_IBNR_CP_2[2]))),
                            "CBNI"=c(qnorm(0.85)*sqrt(MOM_CBNI_CP_1[2]),
                                     ((qnorm(0.85)^2-1)*MOM_CBNI_CP_1[3])/(6*MOM_CBNI_CP_1[2]),
                                     qnorm(0.85)*sqrt(MOM_CBNI_CP_2[2]),
                                     ((qnorm(0.85)^2-1)*MOM_CBNI_CP_2[3])/(6*MOM_CBNI_CP_2[2])))
 
# Risk group 5
# Data frame with the RA for different confidence levels and different assumptions: CP, Mult, Dir
RA_LIC_CL_1 <- ra_lic_cl(MOM_RBNS_CP_1,MOM_RBNS_MULT_1,MOM_RBNS_DIR_1,MOM_IBNR_CP_1,MOM_IBNR_MULT_1,MOM_IBNR_DIR_1)
RA_LRC_CL_1 <- ra_lrc_cl(MOM_CBNI_CP_1,MOM_CBNI_MULT_1,MOM_CBNI_DIR_1)

# Add columns with the relative size of the RA compared to the mean
RA_LIC_CL_1 <- RA_LIC_CL_1 %>% 
  mutate(RA_CP_PER = CP / Summary_mean$IBNS[1], 
         RA_MULT_PER = MULT / Summary_mean$IBNS[1],
         RA_DIR_PER = DIR / Summary_mean$IBNS[1])

RA_LRC_CL_1 <- RA_LRC_CL_1 %>% 
  mutate(RA_CP_PER = CP / Summary_mean$CBNI[1], 
         RA_MULT_PER = MULT / Summary_mean$CBNI[1],
         RA_DIR_PER = DIR / Summary_mean$CBNI[1])


# Risk group 6
# Data frame with the RA for different confidence levels and different assumptions: CP, Mult, Dir
RA_LIC_CL_2 <- ra_lic_cl(MOM_RBNS_CP_2,MOM_RBNS_MULT_2,MOM_RBNS_DIR_2,MOM_IBNR_CP_2,MOM_IBNR_MULT_2,MOM_IBNR_DIR_2)
RA_LRC_CL_2 <- ra_lrc_cl(MOM_CBNI_CP_2,MOM_CBNI_MULT_2,MOM_CBNI_DIR_2)

# Add columns with the relative size of the RA compared to the mean
RA_LIC_CL_2 <- RA_LIC_CL_2 %>% 
  mutate(RA_CP_PER = CP / Summary_mean$IBNS[2], 
         RA_MULT_PER = MULT / Summary_mean$IBNS[2],
         RA_DIR_PER = DIR / Summary_mean$IBNS[2])

RA_LRC_CL_2 <- RA_LRC_CL_2 %>% 
  mutate(RA_CP_PER = CP / Summary_mean$CBNI[2], 
         RA_MULT_PER = MULT / Summary_mean$CBNI[2],
         RA_DIR_PER = DIR / Summary_mean$CBNI[2])


# Graphics with the results
# Risk group 5 LIC
FIG_LIC_1 <- plot_ly(data = RA_LIC_CL_1, x = ~CL) %>%
  add_trace(y = ~RA_CP_PER, type = "scatter", name = "Compound Poisson", mode = "lines",
            line = list(color = "#5BC8AC")) 

FIG_LIC_1 <- FIG_LIC_1 %>% 
  add_trace(y = ~RA_MULT_PER, type = "scatter", name = "Multinomial", mode = "lines",
            line = list(color = "#E6D72A"))

FIG_LIC_1 <- FIG_LIC_1 %>% 
  add_trace(y = ~RA_DIR_PER, type = "scatter", name = "Dirichlet", mode = "lines",
            line = list(color = "#F18D9E"))

FIG_LIC_1 <- FIG_LIC_1 %>% layout(xaxis = list(tickformat = "%", title = "Confidence level"),
                        yaxis = list(tickformat = "%", title = "Risk adjustment"),
                        legend = list(x=0.1, y=0.9), title ="Risk adjustment LIC for risk group 5")


#Risk group 5 LRC
FIG_LRC_1 <- plot_ly(data = RA_LRC_CL_1, x = ~CL) %>%
  add_trace(y = ~RA_CP_PER, type = "scatter", name = "Compound Poisson", mode = "lines",
            line = list(color = "#9BC01C")) 

FIG_LRC_1 <- FIG_LRC_1 %>% 
  add_trace(y = ~RA_MULT_PER, type = "scatter", name = "Multinomial", mode = "lines",
            line = list(color = "#FA6775"))

FIG_LRC_1 <- FIG_LRC_1 %>% 
  add_trace(y = ~RA_DIR_PER, type = "scatter", name = "Dirichlet", mode = "lines",
            line = list(color = "#FFD64D"))

FIG_LRC_1 <- FIG_LRC_1 %>% layout(xaxis = list(tickformat = "%", title = "Confidence level"),
                                  yaxis = list(tickformat = "%", title = "Risk adjustment"),
                                  legend = list(x=0.1, y=0.9), title ="Risk adjustment LRC for risk group 5")


# Risk group 6 LIC
FIG_LIC_2 <- plot_ly(data = RA_LIC_CL_2, x = ~CL) %>%
  add_trace(y = ~RA_CP_PER, type = "scatter", name = "Compound Poisson", mode = "lines",
            line = list(color = "#5BC8AC")) 

FIG_LIC_2 <- FIG_LIC_2 %>% 
  add_trace(y = ~RA_MULT_PER, type = "scatter", name = "Multinomial", mode = "lines",
            line = list(color = "1E656D"))

FIG_LIC_2 <- FIG_LIC_2 %>% 
  add_trace(y = ~RA_DIR_PER, type = "scatter", name = "Dirichlet", mode = "lines",
            line = list(color = "#F62A00"))

FIG_LIC_2 <- FIG_LIC_2 %>% layout(xaxis = list(tickformat = "%", title = "Confidence level"),
                                  yaxis = list(tickformat = "%", title = "Risk adjustment"),
                                  legend = list(x=0.1, y=0.9), title ="Risk adjustment LIC for risk group 6")


#Risk group 6 LRC
FIG_LRC_2 <- plot_ly(data = RA_LRC_CL_2, x = ~CL) %>%
  add_trace(y = ~RA_CP_PER, type = "scatter", name = "Compound Poisson", mode = "lines",
            line = list(color = "#FD974F")) 

FIG_LRC_2 <- FIG_LRC_2 %>% 
  add_trace(y = ~RA_MULT_PER, type = "scatter", name = "Multinomial", mode = "lines",
            line = list(color = "#C60000"))

FIG_LRC_2 <- FIG_LRC_2 %>% 
  add_trace(y = ~RA_DIR_PER, type = "scatter", name = "Dirichlet", mode = "lines",
            line = list(color = "#805A3B"))

FIG_LRC_2 <- FIG_LRC_2 %>% layout(xaxis = list(tickformat = "%", title = "Confidence level"),
                                  yaxis = list(tickformat = "%", title = "Risk adjustment"),
                                  legend = list(x=0.1, y=0.9), title ="Risk adjustment LRC for risk group 6")

FIG_LIC_1
FIG_LIC_2
FIG_LRC_1
FIG_LRC_2
