
RBvalue = c(1.00, 0.86, 0.71, 0.57, 0.43, 0.29, 0.14, 0.00)
CMBvalue = c(0.00, 0.09, 0.18, 0.27, 0.36, 0.45, 0.55, 0.64, 0.73, 0.82, 0.91, 1.00)

MSmatrix = matrix(c(7.29, 15.63, 23.97, 32.31, 40.64, 48.98, 57.32, 65.65, 73.99, 82.33, 90.66, 99.00,
                    6.25, 14.59, 22.93, 31.26, 39.60, 47.94, 56.27, 64.61, 72.95, 81.28, 89.62, 97.96,
                    5.21, 13.55, 21.88, 30.22, 38.56, 46.89, 55.23, 63.57, 71.91, 80.24, 88.58, 96.92,
                    4.17, 12.51, 20.84, 29.18, 37.52, 45.85, 54.19, 62.53, 70.86, 79.20, 87.54, 95.87,
                    3.13, 11.46, 19.80, 28.14, 36.47, 44.81, 53.15, 61.48, 69.82, 78.16, 86.49, 94.83,
                    2.08, 10.42, 18.76, 27.09, 35.43, 43.77, 52.11, 60.44, 68.78, 77.12, 85.45, 93.79,
                    1.04, 9.38, 17.72, 26.05, 34.39, 42.73, 51.06, 59.40, 67.74, 76.07, 84.41, 92.75,
                    0.00, 8.34, 16.67, 25.01, 33.35, 41.68, 50.02, 58.36, 66.69, 75.03, 83.37, 91.71),  ncol = 12, nrow = 8, byrow = TRUE)

#If binary data per item in every subscale is provided
demo_pat$AuditoryCMB = with(demo_pat, ifelse(Aud.4 %in% c(1), 2, ifelse(Aud.3 %in% c(1), 1, 0)))
demo_pat$AuditoryRB = with(demo_pat, ifelse(Aud.2 %in% c(1), 2, ifelse(Aud.1 %in% c(1), 1, 0)))
demo_pat$VisualCMB = with(demo_pat, ifelse(Vis.5 %in% c(1), 4, ifelse(Vis.4 %in% c(1), 3, ifelse(Vis.3 %in% c(1), 2, ifelse(Vis.2 %in% c(1), 1, 0)))))
demo_pat$VisualRB = with(demo_pat, ifelse(Vis.1 %in% c(1), 1, 0))
demo_pat$MotorCMB = with(demo_pat, ifelse(Mot.5 %in% c(1), 3, ifelse(Mot.4 %in% c(1), 2, ifelse(Mot.3 %in% c(1), 1, 0))))
demo_pat$MotorRB = with(demo_pat, ifelse(Mot.2 %in% c(1), 2, ifelse(Mot.1 %in% c(1), 1, 0)))
demo_pat$OromotorCMB = ifelse(Orom.3 %in% c(1), 1, 0)
demo_pat$OromotorRB = with(demo_pat, ifelse(Orom.2 %in% c(1), 2, ifelse(Orom.1 %in% c(1), 1, 0)))

#If highest score per subscale is provided
AuditoryRB = ifelse(demo_pat$bestCRS_Auditory > 2, 2, demo_pat$bestCRS_Auditory)
AuditoryCMB = ifelse(demo_pat$bestCRS_Auditory < 3, 0, demo_pat$bestCRS_Auditory - 2)
VisualRB = ifelse(demo_pat$best_CRS_visual_function > 1, 1, demo_pat$best_CRS_visual_function)
VisualCMB = ifelse(demo_pat$best_CRS_visual_function < 2, 0, demo_pat$best_CRS_visual_function - 1)
MotorRB = ifelse(demo_pat$best_CRS_Motor > 2, 2, demo_pat$best_CRS_Motor)
MotorCMB = ifelse(demo_pat$best_CRS_Motor < 3, 0, demo_pat$best_CRS_Motor - 3)
OromotorRB = ifelse(demo_pat$bestCRS_oromotor_verbal > 2, 2, demo_pat$bestCRS_oromotor_verbal)
OromotorCMB = ifelse(demo_pat$bestCRS_oromotor_verbal < 3, 0, demo_pat$bestCRS_oromotor_verbal - 2)

demo_pat$ArousalMS = with(demo_pat, ifelse(Eveil.3 %in% c(1), 1, ifelse(Eveil.2 %in% c(1), 2/3, ifelse(Eveil.1 %in% c(1), 1/3, 0))))

demo_pat$RB = with(demo_pat, AuditoryRB + VisualRB + MotorRB + OromotorRB)/7
demo_pat$CMB = with(demo_pat, AuditoryCMB + VisualCMB + MotorCMB + OromotorCMB + CommunicationCMB)/11

demo_pat$index_rb = 0
demo_pat$index_cmb = 0
demo_pat$MS_value = 0
for (i in 1:length(demo_pat$RB)){
  demo_pat$index_rb[i]  = match(round(demo_pat$RB[i], 2), RBvalue)
  demo_pat$index_cmb[i] = match(round(demo_pat$CMB[i], 2), CMBvalue)
  demo_pat$MS_value[i] = MSmatrix[demo_pat$index_rb[i], demo_pat$index_cmb[i]]
}

demo_pat$CRS-R_index = demo_pat$MS_value + demo_pat$ArousalMS
