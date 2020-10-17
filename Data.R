# Data used for the MFW
# This part is the one that would change the most depending on the data available

# Read the excel file with the data on claims and exposure
Data <- read.xlsx("C:/Users/tmarques011/Documents/Mestrado/Dissertation/R/Data_MFW.xlsx")
Data <- as.data.frame(Data) #converts to a data frame
is.data.frame(Data) #makes sure it works and this is a data frame

# Determine the moments of the claim severity
# Assess which claims should be used to estimate the severity
# We have exluded claims settled without any payment
# See which years have enough observations and can make sense to select (we have chosen 2012 - 2017)
Plot_1 <- Data %>% 
  filter(Include == TRUE, Risk.group == 5, Valuation.year == 2017, Claim.status == "Closed", Paid.claims != 0) %>% 
  select(Paid.claims, Settled.year)

ggplot(data = Plot_1, mapping = aes(Paid.claims))+
  geom_histogram() +
  facet_wrap(~Settled.year)

Plot_1 %>% 
  count(Settled.year) 

Plot_2 <- Data %>% 
  filter(Include == TRUE, Risk.group == 6, Valuation.year == 2017, Claim.status == "Closed", Paid.claims != 0) %>% 
  select(Paid.claims, Settled.year)

ggplot(data = Plot_1, mapping = aes(Paid.claims))+
  geom_histogram() +
  facet_wrap(~Settled.year)

Plot_2 %>% 
  count(Settled.year)


# Observations for each of the settled claims
# Sum the claim severity for the same claim_ID, only for valuation year 2017
X_1 <- Data %>% 
  filter(Include == TRUE, Risk.group == 5, Valuation.year == 2017, Settled.year %in% 2012:2017) %>% 
  group_by(Claim_ID) %>% 
  summarize(IndClaim = sum(Paid.claims)) %>% 
  filter(is.na(Claim_ID)==FALSE)

# Same as above but for another LoB
X_2 <- Data %>% 
  filter(Include == TRUE, Risk.group == 6, Valuation.year == 2017, Settled.year %in% 2012:2017) %>% 
  group_by(Claim_ID) %>% 
  summarize(IndClaim = sum(Paid.claims)) %>% 
  filter(is.na(Claim_ID) == FALSE)
  
# Determine the moments of the severity distribution
# Different depending on whether we are applying the CP or the multinomial / Dirichlet
G_1 <- X_1 %>% 
  summarise(mean = moment(IndClaim, order=1, central = FALSE),
            rawmom2 = moment(IndClaim, order=2, central = FALSE),
            rawmom3 = moment(IndClaim, order=3, central = FALSE))

G_2 <- X_2 %>% 
  summarise(mean = moment(IndClaim, order=1, central = FALSE),
            rawmom2 = moment(IndClaim, order=2, central = FALSE),
            rawmom3 = moment(IndClaim, order=3, central = FALSE))

G_1_CP <- X_1 %>% 
  summarise(mean = moment(IndClaim, order=1, central = FALSE),
            rawmom2 = moment(IndClaim, order=2, central = TRUE),
            rawmom3 = moment(IndClaim, order=3, central = TRUE))

G_2_CP <- X_2 %>% 
  summarise(mean = moment(IndClaim, order=1, central = FALSE),
            rawmom2 = moment(IndClaim, order=2, central = TRUE),
            rawmom3 = moment(IndClaim, order=3, central = TRUE))


# Create exposure vector for each of the LoBs
Pj_1 <- Data %>% 
  filter(Include == TRUE, Risk.group == 5, is.na(Claim_ID) == TRUE) %>% 
  group_by(Accident.year) %>% 
  summarize(Exposure = sum(Exposure)) %>% 
  select(Exposure)

Pj_2 <- Data %>% 
  filter(Include == TRUE, Risk.group == 6, is.na(Claim_ID) == TRUE) %>% 
  group_by(Accident.year) %>% 
  summarize(Exposure = sum(Exposure)) %>% 
  select(Exposure)


# Create the matrix with the number of reported claims for each of the LoBs
Njd_1 <- Data %>% 
  filter(Include == TRUE, Risk.group == 5, is.na(Claim_ID) == FALSE) %>% 
  mutate(Reporting.delay = Reported.year - Accident.year) %>% 
  filter(Number.of.claims == 1, Valuation.year == 2017) %>% 
  count(Accident.year, Reporting.delay) %>% 
  pivot_wider(names_from = Reporting.delay, values_from = n) %>% 
  subset(select=c(-Accident.year)) %>% 
  mutate("10" = c(0,rep(NA,10))) # Need to add a column at the end (specific to this data)
  Njd_1[[2,10]] <- 0 # Need to add this element to get a triangle (specific to this data)
 

Njd_2 <- Data %>% 
  filter(Include == TRUE, Risk.group == 6, is.na(Claim_ID) == FALSE) %>% 
  mutate(Reporting.delay = Reported.year - Accident.year) %>% 
  filter(Number.of.claims == 1, Valuation.year == 2017) %>% 
  count(Accident.year, Reporting.delay) %>% 
  pivot_wider(names_from = Reporting.delay, values_from = n) %>% 
  subset(select=c(-Accident.year))

# Estimate the reporting pattern based on the DF and CR
Cum_Njd_1 <- incr2cum(Njd_1)
DF_PI_1 <- Cum_Njd_1 %>% 
  ata() %>% 
  attr("vwtd") %>% 
  append(1,after=0) # Need to add a first element for d=0

Cum_Njd_2 <- incr2cum(Njd_2)
DF_PI_2 <- Cum_Njd_2 %>% 
  ata() %>% 
  attr("vwtd") %>% 
  append(1,after=0) # Need to add a first element for d=0

PId_DF_1 <- Pattern_DF(DF_PI_1)
PId_DF_2 <- Pattern_DF(DF_PI_2)

PId_CR_1 <- Reporting_Pattern_CR(Njd_1,Pj_1)
PId_CR_2 <- Reporting_Pattern_CR(Njd_2,Pj_2)


# Create the matrix with the payments for the reported claims: reporting period vs valuation date
Xjdt_1 <- Data %>% 
  filter(Include == TRUE, Risk.group == 5, is.na(Claim_ID) == FALSE) %>% 
  mutate(Payment.delay = Valuation.year - Reported.year) %>% 
  select(Reported.year, Payment.delay, Paid.claims) %>% 
  group_by(Reported.year, Payment.delay) %>% 
  summarize(Payments=sum(Paid.claims)) %>% 
  pivot_wider(names_from = Payment.delay, values_from = Payments) %>% 
  subset(select=c(-Reported.year)) %>% 
  as.matrix()

Xjdt_2 <- Data %>% 
  filter(Include == TRUE, Risk.group == 6, is.na(Claim_ID) == FALSE) %>% 
  mutate(Payment.delay = Valuation.year - Reported.year) %>% 
  select(Reported.year, Payment.delay, Paid.claims) %>% 
  group_by(Reported.year, Payment.delay) %>% 
  summarize(Payments=sum(Paid.claims)) %>% 
  pivot_wider(names_from = Payment.delay, values_from = Payments) %>% 
  subset(select=c(-Reported.year)) %>%
  as.matrix()


# Estimate the payment pattern based on the DF and CR
DF_Vt_1 <- Xjdt_1 %>% 
  ata() %>% 
  attr("vwtd") %>% 
  append(1,after=0) #need to add a first element for d=0

DF_Vt_2 <- Xjdt_2 %>% 
  ata() %>% 
  attr("vwtd") %>% 
  append(1,after=0) #need to add a first element for d=0

Vt_DF_1 <- Pattern_DF(DF_Vt_1)
Vt_DF_2 <- Pattern_DF(DF_Vt_2)

Inc_Xjdt_1 <- cum2incr(Xjdt_1)
Inc_Xjdt_2 <- cum2incr(Xjdt_2)

Vt_CR_1 <- Payment_Pattern_CR(Inc_Xjdt_1,Njd_1)
Vt_CR_2 <- Payment_Pattern_CR(Inc_Xjdt_2,Njd_2)


# Exposure for the LRC not obtained based on real data
# We have defined a vector of observations to show the mechanics of the calculation of a RA for the LRC
PJ_1 <- c(350,100,50)
PJ_2 <- c(2400, 650, 325)


# Determine the moments of theta
# Risk group 5
Prior_Param_MLE_1 <- coef(mle(minuslogl=LogLik_1,start=list(alpha=100,beta=100)))
Prior_Param_DV_1 <- DeVylder(Cum_Njd_1,Pj_1,PId_DF_1)

Post_Mom_MM_1 <- Method_Moments(Cum_Njd_1,Pj_1,PId_DF_1)
Post_Mom_MLE_1 <- Posterior_Mom(Prior_Param_MLE_1,Cum_Njd_1,Pj_1,PId_DF_1)
Post_Mom_DV_1 <- Posterior_Mom(Prior_Param_DV_1,Cum_Njd_1,Pj_1,PId_DF_1)

Prior_Mom_MLE_1 <- Prior_Mom(Prior_Param_MLE_1,PJ_1)
Prior_Mom_DV_1 <- Prior_Mom(Prior_Param_DV_1,PJ_1)


# Risk group 6
#Prior_Param_MLE_2 <- coef(mle(minuslogl=LogLik_2,start=list(alpha=100,beta=100)))
Prior_Param_DV_2 <- DeVylder(Cum_Njd_2,Pj_2,PId_DF_2)

Post_Mom_MM_2 <- Method_Moments(Cum_Njd_2,Pj_2,PId_DF_2)
#Post_Mom_MLE_2 <- Posterior_Mom(Prior_Param_MLE_2,Cum_Njd_2,Pj_2,PId_DF_2)
Post_Mom_DV_2 <- Posterior_Mom(Prior_Param_DV_2,Cum_Njd_2,Pj_2,PId_DF_2)

#Prior_Mom_MLE_2 <- Prior_Mom(Prior_Param_MLE_2,PJ_2)
Prior_Mom_DV_2 <- Prior_Mom(Prior_Param_DV_2,PJ_2)



# Determine the equivalent single discount rate
# The curve to be applied is EIOPA's RFR + VA as at 31 December 2017
# Read and store the curve into a variable Data_RFR
# Determine the cash flows for each case: RBNS, IBNR and CBNI
# For this case we have use the prior and posterior parameters estimated using De Vylder's iterative procedure
Data_RFR <- read.xlsx("C:/Users/tmarques011/Documents/Mestrado/Dissertation/R/RFR+VA 31-12-2017.xlsx")
RFR <- Data_RFR[,2]

Discount_factor <- Discount(RFR)


# Risk group 5
CF_RBNS_UNDISC_1 <- cf_rbns(Njd_1,Vt_DF_1,G_1_CP)
CF_IBNR_UNDISC_1 <- cf_ibnr(Pj_1,PId_DF_1,Vt_DF_1,G_1_CP,Post_Mom_DV_1)
CF_CBNI_UNDISC_1 <- cf_cbni(PJ_1,PId_DF_1,Vt_DF_1,G_1_CP,Prior_Mom_DV_1)

CF_RBNS_DISC_1 <- sum(CF_RBNS_UNDISC_1*Discount_factor) %>% formattable::comma()
CF_IBNR_DISC_1 <- sum(CF_IBNR_UNDISC_1*Discount_factor) %>% formattable::comma()
CF_CBNI_DISC_1 <- sum(CF_CBNI_UNDISC_1*Discount_factor) %>% formattable::comma()

D_RBNS_1 <- uniroot(f,c(0,2),CF_UNDISC=CF_RBNS_UNDISC_1, CF_DISC=CF_RBNS_DISC_1)$root
D_IBNR_1 <- uniroot(f,c(0,1),CF_UNDISC=CF_IBNR_UNDISC_1, CF_DISC=CF_IBNR_DISC_1)$root
D_CBNI_1 <- uniroot(f,c(0,1),CF_UNDISC=CF_CBNI_UNDISC_1, CF_DISC=CF_CBNI_DISC_1)$root


# Risk group 6
CF_RBNS_UNDISC_2 <- cf_rbns(Njd_2,Vt_DF_2,G_2_CP)
CF_IBNR_UNDISC_2 <- cf_ibnr(Pj_2,PId_DF_2,Vt_DF_2,G_2_CP,Post_Mom_DV_2)
CF_CBNI_UNDISC_2 <- cf_cbni(PJ_2,PId_DF_2,Vt_DF_2,G_2_CP,Prior_Mom_DV_2)

CF_RBNS_DISC_2 <- sum(CF_RBNS_UNDISC_2*Discount_factor) %>% formattable::comma()
CF_IBNR_DISC_2 <- sum(CF_IBNR_UNDISC_2*Discount_factor) %>% formattable::comma()
CF_CBNI_DISC_2 <- sum(CF_CBNI_UNDISC_2*Discount_factor) %>% formattable::comma()

D_RBNS_2 <- uniroot(f,c(0,2),CF_UNDISC=CF_RBNS_UNDISC_2, CF_DISC=CF_RBNS_DISC_2)$root
D_IBNR_2 <- uniroot(f,c(0,1),CF_UNDISC=CF_IBNR_UNDISC_2, CF_DISC=CF_IBNR_DISC_2)$root
D_CBNI_2 <- uniroot(f,c(0,1),CF_UNDISC=CF_CBNI_UNDISC_2, CF_DISC=CF_CBNI_DISC_2)$root
