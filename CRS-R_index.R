# Read file with highest CRS-R subscores (with column names Auditory, Visual, Motor, Oromotor, Arousal).
# Alternatively, a data file with binary information for the presence or absence of every CRS-R item can
# be provided to calculate the CRS-R index. In that case, data should be organized per CRS-R item in a 
# binary fashion (0 for absent and 1 for present). Column names should be numbered by the hierachical order
# of the CRS-R items (Aud.1-Aud.4, Vis.1-Vis.5, Mot.1-Mot.5, Orom.1-Orom.3, Arou.1-Arou.3). Items for a
# diagnosis of EMCS are excluded.

# mydata = read.csv() 

# Initialize the transposition matrix to calculate the CRS-R index (Annen*, Filippini* et al., (2019)), 
# same as from CRS-R modified score (Sattin et al., 2015).
RBvalue = c(1.00, 0.86, 0.71, 0.57, 0.43, 0.29, 0.14, 0.00)
CMBvalue = c(0.00, 0.09, 0.18, 0.27, 0.36, 0.45, 0.55, 0.64, 0.73, 0.82, 0.91, 1.00)

MSmatrix = matrix(c(7.29, 15.63, 23.97, 32.31, 40.64, 48.98, 57.32, 65.65, 73.99, 82.33, 90.66, 99.00,
                    6.25, 14.59, 22.93, 31.26, 39.60, 47.94, 56.27, 64.61, 72.95, 81.28, 89.62, 97.96,
                    5.21, 13.55, 21.88, 30.22, 38.56, 46.89, 55.23, 63.57, 71.91, 80.24, 88.58, 96.92,
                    4.17, 12.51, 20.84, 29.18, 37.52, 45.85, 54.19, 62.53, 70.86, 79.20, 87.54, 95.87,
                    3.13, 11.46, 19.80, 28.14, 36.47, 44.81, 53.15, 61.48, 69.82, 78.16, 86.49, 94.83,
                    2.08, 10.42, 18.76, 27.09, 35.43, 43.77, 52.11, 60.44, 68.78, 77.12, 85.45, 93.79,
                    1.04, 9.38, 17.72, 26.05, 34.39, 42.73, 51.06, 59.40, 67.74, 76.07, 84.41, 92.75,
                    0.00, 8.34, 16.67, 25.01, 33.35, 41.68, 50.02, 58.36, 66.69, 75.03, 83.37, 91.71),
                    ncol = 12, nrow = 8, byrow = TRUE)

# Use this if highest score per subscale is provided. This is appropriate to calculate the 
# CRS-R index as proposed by Annen*, Filippini* et al., (2019).
mydata$AuditoryRB = ifelse(mydata$Auditory > 2, 2, mydata$Auditory)
mydata$AuditoryCMB = ifelse(mydata$Auditory < 3, 0, mydata$Auditory - 2)
mydata$VisualRB = ifelse(mydata$Visual > 1, 1, mydata$Visual)
mydata$VisualCMB = ifelse(mydata$Visual < 2, 0, mydata$Visual - 1)
mydata$MotorRB = ifelse(mydata$Motor > 2, 2, mydata$_Motor)
mydata$MotorCMB = ifelse(mydata$Motor < 3, 0, mydata$Motor - 3)
mydata$OromotorRB = ifelse(mydata$Oromotor > 2, 2, mydata$Oromotor)
mydata$OromotorCMB = ifelse(mydata$Oromotor < 3, 0, mydata$Oromotor - 2)
mydata$ArousalMS = mydata$Arousal * (1/3)

# Use this (instead of lines 22-30) if binary data per item in every subscale is provided.
# mydata$AuditoryCMB = with(mydata, ifelse(Aud.4 %in% c(1), 2, ifelse(Aud.3 %in% c(1), 1, 0)))
# mydata$AuditoryRB = with(mydata, ifelse(Aud.2 %in% c(1), 2, ifelse(Aud.1 %in% c(1), 1, 0)))
# mydata$VisualCMB = with(mydata, ifelse(Vis.5 %in% c(1), 4, ifelse(Vis.4 %in% c(1), 3, 
#                         ifelse(Vis.3 %in% c(1), 2, ifelse(Vis.2 %in% c(1), 1, 0)))))
# mydata$VisualRB = with(mydata, ifelse(Vis.1 %in% c(1), 1, 0))
# mydata$MotorCMB = with(mydata, ifelse(Mot.5 %in% c(1), 3, ifelse(Mot.4 %in% c(1), 2, 
#                        ifelse(Mot.3 %in% c(1), 1, 0))))
# mydata$MotorRB = with(mydata, ifelse(Mot.2 %in% c(1), 2, ifelse(Mot.1 %in% c(1), 1, 0)))
# mydata$OromotorCMB = ifelse(Orom.3 %in% c(1), 1, 0)
# mydata$OromotorRB = with(mydata, ifelse(Orom.2 %in% c(1), 2, ifelse(Orom.1 %in% c(1), 1, 0)))
# mydata$ArousalMS = with(mydata, ifelse(Arou.3 %in% c(1), 1, ifelse(Arou.2 %in% c(1), 2/3, 
#                         ifelse(Arou.1 %in% c(1), 1/3, 0))))


#Obtain the reflexive behavior and cognitively mediated behavior value by addition of 
mydata$RB = with(mydata, AuditoryRB + VisualRB + MotorRB + OromotorRB)/7
mydata$CMB = with(mydata, AuditoryCMB + VisualCMB + MotorCMB + OromotorCMB)/11

mydata$index_rb = 0
mydata$index_cmb = 0
mydata$MS_value = 0
for (i in 1:length(mydata$RB)){
  mydata$index_rb[i]  = match(round(mydata$RB[i], 2), RBvalue)
  mydata$index_cmb[i] = match(round(mydata$CMB[i], 2), CMBvalue)
  mydata$MS_value[i] = MSmatrix[mydata$index_rb[i], mydata$index_cmb[i]]
}

mydata$CRS_R_index = mydata$MS_value + mydata$ArousalMS
