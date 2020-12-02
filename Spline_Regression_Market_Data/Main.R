library("readxl")
library("stargazer")
library("openxlsx")
library("splines")
library("AER")

# load the dataset
setwd("D:/Uni/2/Theory and Application of Regression Analysis (MGMT-PHD 201B)/Project/Code")
amd <- read_excel("MarketShare-Discount-Table.xlsx")

# add CapL variable
# OPN = amd$OPN
# Date = amd$FirstDay
# TQ = amd$TQ
# CapL = c()
# for (i in 1:nrow(amd)) {
# totalCap = amd$MarketSize[i]/0.85
# soldCap = 0
#  for (j in 1:nrow(amd)) {
#    if(OPN[j]==OPN[i] & as.Date(Date[j])<as.Date(Date[i])) {
#      soldCap = soldCap + TQ[j]
#    }
#  }
#  CapL[i] = (totalCap-soldCap)/(amd$MarketSize[i])
#  if(i==250 | i==500) {
#    print(i)
#  }
# }
# write.xlsx(data.frame("OPN" = amd$OPN, "CustID" = amd$CustID, "CapL" = CapL), file = "CapL_data.xlsx", colNames = TRUE)

# remove truncated data
rows_to_keep = c()
iter = 1
first_date = amd$FirstDay
last_date = amd$LastDay
for (i in 1:nrow(amd)) {
  if (as.Date(first_date[i])>=as.Date('4/1/2009', "%m/%d/%Y") & as.Date(last_date[i])<=as.Date('12/25/2011', "%m/%d/%Y")) {
      rows_to_keep[iter] = i
      iter = iter + 1
  }
}
amd_dates = amd[rows_to_keep,]

# remove products with few number of customers
rows_to_keep = c()
iter = 1
OPN = amd_dates$OPN
num_buyers = as.data.frame((table(amd_dates$OPN)))
for (i in 1:nrow(amd_dates)) {
  if (num_buyers$Freq[which(num_buyers$Var1==OPN[i])]>3) {
    rows_to_keep[iter] = i
    iter = iter + 1
  }
}
amd_dates_noncustomized = amd_dates[rows_to_keep,]

# select fixed price contracts
rows_to_keep = c()
iter = 1
CVP = amd_dates_noncustomized$CVP
for (i in 1:nrow(amd_dates_noncustomized)) {
  if (is.na(CVP[i])) {
    rows_to_keep[iter] = i
    iter = iter + 1
  } else if (CVP[i]==0) {
    rows_to_keep[iter] = i
    iter = iter + 1
  }
}
amd_dates_noncustomized_fixed = amd_dates_noncustomized[rows_to_keep,]

# add variables
amd_dates_noncustomized_fixed$r4ds = amd_dates_noncustomized_fixed$DS^0.25
amd_dates_noncustomized_fixed$lndod = log(amd_dates_noncustomized_fixed$DoD+1)
amd_dates_noncustomized_fixed$lndrt = log(amd_dates_noncustomized_fixed$Duration)

# add fixed effects
major_buyer1_dummy = c()
major_buyer2_dummy = c()
major_buyer3_dummy = c()
major_buyer4_dummy = c()
major_buyer5_dummy = c()
major_buyer6_dummy = c()
major_buyer7_dummy = c()
major_buyer8_dummy = c()
major_buyer9_dummy = c()
major_buyer10_dummy = c()
CustID = amd_dates_noncustomized_fixed$CustID
for (i in 1:nrow(amd_dates_noncustomized_fixed)) {
  major_buyer1_dummy[i] = ifelse(CustID[i]=="400048",1,0)
  major_buyer2_dummy[i] = ifelse(CustID[i]=="400975",1,0)
  major_buyer3_dummy[i] = ifelse(CustID[i]=="401438",1,0)
  major_buyer4_dummy[i] = ifelse(CustID[i]=="400249",1,0)
  major_buyer5_dummy[i] = ifelse(CustID[i]=="401093",1,0)
  major_buyer6_dummy[i] = ifelse(CustID[i]=="400110",1,0)
  major_buyer7_dummy[i] = ifelse(CustID[i]=="401173",1,0)
  major_buyer8_dummy[i] = ifelse(CustID[i]=="401470",1,0)
  major_buyer9_dummy[i] = ifelse(CustID[i]=="401294",1,0)
  major_buyer10_dummy[i] = ifelse(CustID[i]=="401042",1,0)
}
amd_dates_noncustomized_fixed$MajBuyer1 = major_buyer1_dummy
amd_dates_noncustomized_fixed$MajBuyer2 = major_buyer2_dummy
amd_dates_noncustomized_fixed$MajBuyer3 = major_buyer3_dummy
amd_dates_noncustomized_fixed$MajBuyer4 = major_buyer4_dummy
amd_dates_noncustomized_fixed$MajBuyer5 = major_buyer5_dummy
amd_dates_noncustomized_fixed$MajBuyer6 = major_buyer6_dummy
amd_dates_noncustomized_fixed$MajBuyer7 = major_buyer7_dummy
amd_dates_noncustomized_fixed$MajBuyer8 = major_buyer8_dummy
amd_dates_noncustomized_fixed$MajBuyer9 = major_buyer9_dummy
amd_dates_noncustomized_fixed$MajBuyer10 = major_buyer10_dummy

major_product1_dummy = c()
major_product2_dummy = c()
major_product3_dummy = c()
major_product4_dummy = c()
major_product5_dummy = c()
major_product6_dummy = c()
major_product7_dummy = c()
major_product8_dummy = c()
major_product9_dummy = c()
major_product10_dummy = c()
major_product11_dummy = c()
major_product12_dummy = c()
major_product13_dummy = c()
major_product14_dummy = c()
major_product15_dummy = c()
major_product16_dummy = c()
major_product17_dummy = c()
major_product18_dummy = c()
major_product19_dummy = c()
major_product20_dummy = c()
OPN = amd_dates_noncustomized_fixed$OPN
for (i in 1:nrow(amd_dates_noncustomized_fixed)) {
  major_product1_dummy[i] = ifelse(OPN[i]=="HDZ955FBGMBOX",1,0)
  major_product2_dummy[i] = ifelse(OPN[i]=="HDZ965FBGMBOX",1,0)
  major_product3_dummy[i] = ifelse(OPN[i]=="SDX140HBGQBOX",1,0)
  major_product4_dummy[i] = ifelse(OPN[i]=="ADX240OCGQBOX",1,0)
  major_product5_dummy[i] = ifelse(OPN[i]=="ADX245OCGQBOX",1,0)
  major_product6_dummy[i] = ifelse(OPN[i]=="HDZ555WFGMBOX",1,0)
  major_product7_dummy[i] = ifelse(OPN[i]=="HDT55TFBGRBOX",1,0)
  major_product8_dummy[i] = ifelse(OPN[i]=="HDX945WFGMBOX",1,0)
  major_product9_dummy[i] = ifelse(OPN[i]=="ADX250OCGMBOX",1,0)
  major_product10_dummy[i] = ifelse(OPN[i]=="ADX250OCGQBOX",1,0)
  major_product11_dummy[i] = ifelse(OPN[i]=="ADX620WFGIBOX",1,0)
  major_product12_dummy[i] = ifelse(OPN[i]=="ADX640WFGMBOX",1,0)
  major_product13_dummy[i] = ifelse(OPN[i]=="HDZ955FBGIBOX",1,0)
  major_product14_dummy[i] = ifelse(OPN[i]=="ADX630WFGIBOX",1,0)
  major_product15_dummy[i] = ifelse(OPN[i]=="HDT90ZFBGRBOX",1,0)
  major_product16_dummy[i] = ifelse(OPN[i]=="HDZ720WFGIBOX",1,0)
  major_product17_dummy[i] = ifelse(OPN[i]=="ADX215OCK22GQ",1,0)
  major_product18_dummy[i] = ifelse(OPN[i]=="ADX435WFGIBOX",1,0)
  major_product19_dummy[i] = ifelse(OPN[i]=="HDX925WFGIBOX",1,0)
  major_product20_dummy[i] = ifelse(OPN[i]=="HDZ550WFGIBOX",1,0)
}
amd_dates_noncustomized_fixed$MajProd1 = major_product1_dummy
amd_dates_noncustomized_fixed$MajProd2 = major_product2_dummy
amd_dates_noncustomized_fixed$MajProd3 = major_product3_dummy
amd_dates_noncustomized_fixed$MajProd4 = major_product4_dummy
amd_dates_noncustomized_fixed$MajProd5 = major_product5_dummy
amd_dates_noncustomized_fixed$MajProd6 = major_product6_dummy
amd_dates_noncustomized_fixed$MajProd7 = major_product7_dummy
amd_dates_noncustomized_fixed$MajProd8 = major_product8_dummy
amd_dates_noncustomized_fixed$MajProd9 = major_product9_dummy
amd_dates_noncustomized_fixed$MajProd10 = major_product10_dummy
amd_dates_noncustomized_fixed$MajProd11 = major_product11_dummy
amd_dates_noncustomized_fixed$MajProd12 = major_product12_dummy
amd_dates_noncustomized_fixed$MajProd13 = major_product13_dummy
amd_dates_noncustomized_fixed$MajProd14 = major_product14_dummy
amd_dates_noncustomized_fixed$MajProd15 = major_product15_dummy
amd_dates_noncustomized_fixed$MajProd16 = major_product16_dummy
amd_dates_noncustomized_fixed$MajProd17 = major_product17_dummy
amd_dates_noncustomized_fixed$MajProd18 = major_product18_dummy
amd_dates_noncustomized_fixed$MajProd19 = major_product19_dummy
amd_dates_noncustomized_fixed$MajProd20 = major_product20_dummy

# add segmentation dummies
amd_dates_noncustomized_fixed$SegI1 = ifelse(amd_dates_noncustomized_fixed$r4ds<0.2 & amd_dates_noncustomized_fixed$r4ds>=0.0, 1, 0)
amd_dates_noncustomized_fixed$SegI2 = ifelse(amd_dates_noncustomized_fixed$r4ds<0.35 & amd_dates_noncustomized_fixed$r4ds>=0.2, 1, 0)
amd_dates_noncustomized_fixed$SegI3 = ifelse(amd_dates_noncustomized_fixed$r4ds<0.5 & amd_dates_noncustomized_fixed$r4ds>=0.35, 1, 0)
amd_dates_noncustomized_fixed$SegI4 = ifelse(amd_dates_noncustomized_fixed$r4ds<0.65 & amd_dates_noncustomized_fixed$r4ds>=0.5, 1, 0)
amd_dates_noncustomized_fixed$SegI5 = ifelse(amd_dates_noncustomized_fixed$r4ds<0.8 & amd_dates_noncustomized_fixed$r4ds>=0.65, 1, 0)
amd_dates_noncustomized_fixed$SegI6 = ifelse(amd_dates_noncustomized_fixed$r4ds<1.0 & amd_dates_noncustomized_fixed$r4ds>=0.8, 1, 0)

# OLS
# fit_ols = lm (ED ~ r4ds + Cbase + TQ + Herf + lndod+ lndrt + CapL + Cshr + Vrate + CapL:lndod + MajBuyer1 + MajBuyer2 + MajBuyer3 + MajBuyer4 + MajBuyer5 + MajBuyer6 + MajBuyer7 + MajBuyer8 + MajBuyer9 + MajBuyer10 + MajProd1 + MajProd2 + MajProd3 + MajProd4 + MajProd5 + MajProd6 + MajProd7 + MajProd8 + MajProd9 + MajProd10 + MajProd11 + MajProd12 + MajProd13 + MajProd14 + MajProd15 + MajProd16 + MajProd17 + MajProd18 + MajProd19 + MajProd20, data=amd_dates_noncustomized_fixed)
# print(summary(fit_ols))

# Cubic polynomial
# fit_cbc = lm (ED ~ r4ds + I(r4ds^2) + I(r4ds^3) + Cbase + TQ + Herf + lndod+ lndrt + CapL + Cshr + Vrate + CapL:lndod + MajBuyer1 + MajBuyer2 + MajBuyer3 + MajBuyer4 + MajBuyer5 + MajBuyer6 + MajBuyer7 + MajBuyer8 + MajBuyer9 + MajBuyer10 + MajProd1 + MajProd2 + MajProd3 + MajProd4 + MajProd5 + MajProd6 + MajProd7 + MajProd8 + MajProd9 + MajProd10 + MajProd11 + MajProd12 + MajProd13 + MajProd14 + MajProd15 + MajProd16 + MajProd17 + MajProd18 + MajProd19 + MajProd20, data=amd_dates_noncustomized_fixed)
# print(summary(fit_cbc))

# Segmentation
# fit_seg = lm (ED ~ SegI2 + SegI3 + SegI4 + SegI5 + SegI6 + Cbase + TQ + Herf + lndod + lndrt + CapL + Cshr + Vrate + CapL:lndod + MajBuyer1 + MajBuyer2 + MajBuyer3 + MajBuyer4 + MajBuyer5 + MajBuyer6 + MajBuyer7 + MajBuyer8 + MajBuyer9 + MajBuyer10 + MajProd1 + MajProd2 + MajProd3 + MajProd4 + MajProd5 + MajProd6 + MajProd7 + MajProd8 + MajProd9 + MajProd10 + MajProd11 + MajProd12 + MajProd13 + MajProd14 + MajProd15 + MajProd16 + MajProd17 + MajProd18 + MajProd19 + MajProd20, data=amd_dates_noncustomized_fixed)
# print(summary(fit_seg))

# Quadratic spline
# fit_spln <- lm(ED ~ bs(amd_dates_noncustomized_fixed$r4ds, knots = c(5.6), intercept = TRUE, degree = 2) + Cbase + TQ + Herf + lndod + lndrt + CapL + Cshr + Vrate + CapL:lndod + MajBuyer1 + MajBuyer2 + MajBuyer3 + MajBuyer4 + MajBuyer5 + MajBuyer6 + MajBuyer7 + MajBuyer8 + MajBuyer9 + MajBuyer10 + MajProd1 + MajProd2 + MajProd3 + MajProd4 + MajProd5 + MajProd6 + MajProd7 + MajProd8 + MajProd9 + MajProd10 + MajProd11 + MajProd12 + MajProd13 + MajProd14 + MajProd15 + MajProd16 + MajProd17 + MajProd18 + MajProd19 + MajProd20, data = amd_dates_noncustomized_fixed)
# print(summary(fit_spln))

# Plot DS (Figure 1)
# hist(amd_dates_noncustomized_fixed$DS, main="Demand Share Distribution", xlab="Demand Share", xlim=c(0,1), col="darkmagenta", freq=FALSE)
# hist(amd_dates_noncustomized_fixed$r4ds, main="Fourth Root of Demand Share Distribution", xlab="The Fourth Root of Demand Share (r4ds)", xlim=c(0,1), col="darkmagenta", freq=FALSE)

# Summary statistics (Table 3)
# print(summary(amd_dates_noncustomized_fixed$ED))
# amd_summary = data.frame(amd_dates_noncustomized_fixed[c("ED","DS","r4ds","Cbase","TQ","Herf", "lndod", "lndrt", "CapL", "Cshr", "Vrate")])
# stargazer(amd_summary, summary.stat = c("mean","sd","min","max"), title="Summary statistics", align=TRUE)

# r4ds statistics (Table 5)
# stargazer(data.frame(amd_dates_noncustomized_fixed$r4ds[amd_dates_noncustomized_fixed$SegI1==1]), summary.stat = c("mean","sd","n"), title="Summary statistics", align=TRUE)
# stargazer(data.frame(amd_dates_noncustomized_fixed$r4ds[amd_dates_noncustomized_fixed$SegI2==1]), summary.stat = c("mean","sd","n"), title="Summary statistics", align=TRUE)
# stargazer(data.frame(amd_dates_noncustomized_fixed$r4ds[amd_dates_noncustomized_fixed$SegI3==1]), summary.stat = c("mean","sd","n"), title="Summary statistics", align=TRUE)
# stargazer(data.frame(amd_dates_noncustomized_fixed$r4ds[amd_dates_noncustomized_fixed$SegI4==1]), summary.stat = c("mean","sd","n"), title="Summary statistics", align=TRUE)
# stargazer(data.frame(amd_dates_noncustomized_fixed$r4ds[amd_dates_noncustomized_fixed$SegI5==1]), summary.stat = c("mean","sd","n"), title="Summary statistics", align=TRUE)
# stargazer(data.frame(amd_dates_noncustomized_fixed$r4ds[amd_dates_noncustomized_fixed$SegI6==1]), summary.stat = c("mean","sd","n"), title="Summary statistics", align=TRUE)
# stargazer(fit_seg, style = "all")

# Plot the predicted values (Figure 2) -> plots are drawn in Python
# ED_hat = predict(fit_seg)
# stargazer(data.frame(ED_hat[amd_dates_noncustomized_fixed$SegI1==1]), summary.stat = c("mean","sd","n"), title="Summary statistics", align=TRUE)
# stargazer(data.frame(ED_hat[amd_dates_noncustomized_fixed$SegI1==2]), summary.stat = c("mean","sd","n"), title="Summary statistics", align=TRUE)
# stargazer(data.frame(ED_hat[amd_dates_noncustomized_fixed$SegI1==3]), summary.stat = c("mean","sd","n"), title="Summary statistics", align=TRUE)
# stargazer(data.frame(ED_hat[amd_dates_noncustomized_fixed$SegI1==4]), summary.stat = c("mean","sd","n"), title="Summary statistics", align=TRUE)
# stargazer(data.frame(ED_hat[amd_dates_noncustomized_fixed$SegI1==5]), summary.stat = c("mean","sd","n"), title="Summary statistics", align=TRUE)
# stargazer(data.frame(ED_hat[amd_dates_noncustomized_fixed$SegI1==6]), summary.stat = c("mean","sd","n"), title="Summary statistics", align=TRUE)

# Estimations for covariates (Table 7)
# stargazer(fit_seg, fit_spln, fit_spln) # model 4 is implemented in python and the results are entered manually in the table

