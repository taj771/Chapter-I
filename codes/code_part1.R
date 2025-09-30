# This code derive the elasticity estimations based on primary study's parameters (accounting functioanl form
# and distance interactions) at the distacne interval of 50 meters
# Finally extarct those elasticity measures at different ditance bins and other important parameters

# clear memory
rm(list = ls())

# Withould distance involvement.  #################################################

# Define the range of x values
x_values <- seq(50, 1000, by = 50)

# Initialize a dataframe to store x and y values
results <- data.frame(x = x_values)

# Apply the functions over the x values and extract y for each
results <- results %>%
  mutate(
    `Ara 2007 - IH Cluster 5 - OLS` = if_else(x <= 300, 0.009, NA_real_),
    `Ara 2007 - IH Cluster 6 - OLS` = if_else(x <= 500, 0.172, NA_real_),
    `Ara 2007 - IH Cluster 7 - OLS` = if_else(x <= 500, -0.033, NA_real_),
    `Ara 2007 - IH Cluster 11 - OLS` = if_else(x <= 500, -0.033, NA_real_),
    `Ara 2007 - IH Cluster 5 - GMM` = if_else(x <= 500, 0.012, NA_real_),
    `Ara 2007 - IH Cluster 6 - GMM` = if_else(x <= 500, 0.163, NA_real_),
    `Ara 2007 - IH Cluster 7 - GMM` = if_else(x <= 500, -0.007, NA_real_),
    `Ara 2007 - IH Cluster 11 - GMM` = if_else(x <= 500, -0.029, NA_real_),
    `Ara 2007 - CBG Cluster 2 - OLS` = if_else(x <= 500, 0.204, NA_real_),
    `Ara 2007 - CBG Cluster 3 - OLS` = if_else(x <= 500, 0.029, NA_real_),
    `Ara 2007 - CBG Cluster 3 - OLS` = if_else(x <= 500, 0.029, NA_real_),
    `Ara 2007 - CBG Cluster 4 - OLS` = if_else(x <= 500, 0.12, NA_real_),
    `Ara 2007 - CBG Cluster 7 - OLS` = if_else(x <= 500, 0.037, NA_real_),
    `Ara 2007 - CBG Cluster 9 - OLS` = if_else(x <= 500, 0.012, NA_real_),
    `Ara 2007 - CBG Cluster 2 - GMM` = if_else(x <= 500, 0.203, NA_real_),
    `Ara 2007 - CBG Cluster 3 - GMM` = if_else(x <= 500, 0.026, NA_real_),
    `Ara 2007 - CBG Cluster 4 - GMM` = if_else(x <= 500, 0.056, NA_real_),
    `Ara 2007 - CBG Cluster 7 - GMM` = if_else(x <= 500, 0.052, NA_real_),
    `Ara 2007 - CBG Cluster 9 - GMM` = if_else(x <= 500, 0.016, NA_real_),
    `Boyle and Taylor 2001 - G1-Town Data` = if_else(x <= 500, 4.369999886*3515/102746, NA_real_),
    `Boyle and Taylor 2001 - G1-Survey Data` = if_else(x <= 500, 3.920000076*3515/102746, NA_real_),
    `Boyle and Taylor 2001 - G2-Town Data` = if_else(x <= 500, 2.099999905*3515/85197, NA_real_),
    `Boyle and Taylor 2001 - G2-Survey Data` = if_else(x <= 500, 1.389999986*3515/85197, NA_real_),
    `Boyle and Taylor 2001 - G3-Town Data` = if_else(x <= 500, 4.730000019*3515/32779, NA_real_),
    `Boyle and Taylor 2001 - G3-Survey Data` = if_else(x <= 500, 6.130000114*3515/32779, NA_real_),
    `Boyle and Taylor 2001 - G4-Town Data` = if_else(x <= 500, 40.02999878*3515/97482, NA_real_),
    `Boyle and Taylor 2001 - G4-Survey Data` = if_else(x <= 500, 36.13999939*3515/97482, NA_real_),
    `Boyle et al 1999 - Lewiston/Auburn` = if_else(x <= 500, 3514/104069*7.375697136, NA_real_),
    `Boyle et al 1999 - Waterville` = if_else(x <= 500, 3514/85880*3.167430162, NA_real_),
    `Boyle et al 1999 - Bangor` = if_else(x <= 500, 3514/73938*3.584641457, NA_real_),
    `Boyle et al 1999 - Camden` = if_else(x <= 500, 3514/100350*13.07054615, NA_real_),
    `Calder¢n-Arrieta 2019 - model 1` = if_else(x <= 500, 178.3*3.83/2239.2, NA_real_),
    `Calder¢n-Arrieta 2019 - model 2` = if_else(x <= 500, 185.9*3.83/2239.2, NA_real_),
    `Clapper & Caudill 2014 - liner - sale price` = if_else(x <= 500, 43520.43*3.851/648415.8, NA_real_),
    `Clapper & Caudill 2014 - log-liner - sale price` = if_else(x <= 500, 0.065*3.851, NA_real_),
    `Clapper & Caudill 2014 - log-log - sale price` = if_else(x <= 500, 0.269, NA_real_),
    `Clapper & Caudill 2014 - liner - sale price per square foot` = if_else(x <= 500, 30.988*3.851/443.566, NA_real_),
    `Clapper & Caudill 2014 - log-liner - sale price per square foot` = if_else(x <= 500, 0.064*3.851, NA_real_),
    `Clapper & Caudill 2014 - log-log - sale price per square foot` = if_else(x <= 500, 0.268, NA_real_),
    `Gibbs et al 2002 - Conway/Milton` = if_else(x <= 500, 4.48059988*1235.77002/138763.05, NA_real_),
    `Gibbs et al 2002 - Winnipesaukee` = if_else(x <= 500, 17.34000015*1879.099976/175157.73, NA_real_),
    `Gibbs et al 2002 - Derry/Amherst` = if_else(x <= 500, 76.76999664*213.5800018/132162.84, NA_real_),
    `Gibbs et al 2002 - Spofford/Greenfield` = if_else(x <= 500, 149.6000061*283.6900024/167104.7, NA_real_),
    `Horsch & Lewis 2009 - Model 1` = if_else(x <= 500, 13893.84961*3.28*0.926591992/268034.57, NA_real_),
    `Horsch & Lewis 2009 - Model 2` = if_else(x <= 500, 14355.2998*3.28*0.926591992/268034.57, NA_real_),
    `Horsch & Lewis 2009 - Model 3` = if_else(x <= 500, 13367.92969*3.28*0.926591992/268034.57, NA_real_),
    `Horsch & Lewis 2009 - Random Effects with Year Dummies` = if_else(x <= 500, 7072.709961*3.28*0.926591992/268034.57, NA_real_),
    `Horsch & Lewis 2009 - Random Effects with Year Trend Variable` = if_else(x <= 500, 6443.779785*3.28*0.926591992/268034.57, NA_real_),
    `Hsu 2000 - Northeast Kingdom` = if_else(x <= 500, 8.75*866/65363.49, NA_real_),
    `Hsu 2000 - Lake Champlain` = if_else(x <= 500, 12.84000015*2021/120063.58, NA_real_),
    `Hsu 2000 - Rutland County` = if_else(x <= 500, 10.48999977*1552/104471.03, NA_real_),
    `Hsu 2000 - Milfoil` = if_else(x <= 500, 10.60999966*1581/105410.9, NA_real_),
    `Hsu 2000 - exp(milfoil)` = if_else(x <= 500, 13.32999992*1581/105410.9, NA_real_),
    `Hsu 2000 - Total Weed` = if_else(x <= 500, 44.31000137*1581/105410.9, NA_real_),
    `Hsu 2000 - exp(Total Weed)` = if_else(x <= 500, 22.92000008*1581/105410.9, NA_real_),
    `Irwin & Wolf 2022 - Model 1` = if_else(x <= 2000, 0.059, NA_real_),
    `Irwin & Wolf 2022 - Model 2` = if_else(x <= 2000, 0.093, NA_real_),
    `Irwin & Wolf 2022 - Model 3` = if_else(x <= 2000, 0.103, NA_real_),
    `Irwin & Wolf 2022 - Model 4` = if_else(x <= 2000, 0.086, NA_real_),
    `Irwin & Wolf 2022 - Model 6` = if_else(x <= 2000, 0.079, NA_real_),
    `Kashian et al 2006 - Hedonic Equation (3)` = if_else(x <= 500, 5207*3.28*3.627120018/184892, NA_real_),
    `Kemp et al 2017 - Model 1 (SD)` = if_else(x <= 500, 46459.33/(218562*3.28), NA_real_),
    `Kemp et al 2017 - Model 2 (SD)` = if_else(x <= 500, 45292.89/(238142*3.28), NA_real_),
    `Kemp et al 2017 - Model 3 (SD)` = if_else(x <= 500, 66046.22/(175331*3.28), NA_real_),
    `Krysel et al 2003 - Boyle et al - Aitkin Lake Group` = if_else(x <= 500, 13.22999954*702.6099854/100313, NA_real_),
    `Krysel et al 2003 - Boyle et al - Brainerd Lake Group` = if_else(x <= 500, 4.71999979*985.7600098/176461, NA_real_),
    `Krysel et al 2003 - Boyle et al - Grand Rapids Lake Group` = if_else(x <= 500, 1.100000024*852.2600098/135905, NA_real_),
    `Krysel et al 2003 - Boyle et al - Walker Lake Group` = if_else(x <= 500, 2.150000095*1031.869995/179621, NA_real_),
    `Krysel et al 2003 - Boyle et al - Park Rapids Lake Group` = if_else(x <= 500, 21.75*882.9099731/124390, NA_real_),
    `Krysel et al 2003 - MN - Aitkin Lake Group` = if_else(x <= 500, 7.309999943*702.6099854/100313, NA_real_),
    `Krysel et al 2003 - MN - Brainerd Lake Group` = if_else(x <= 500, 1.940000057*985.7600098/176461, NA_real_),
    `Krysel et al 2003 - MN - Grand Rapids Lake Group` = if_else(x <= 500, 1.730000019*852.2600098/135905, NA_real_),
    `Krysel et al 2003 - MN - Walker Lake Group` = if_else(x <= 500, 1.909999967*1031.869995/179621, NA_real_),
    `Krysel et al 2003 - MN - Park Rapids Lake Group` = if_else(x <= 500, 19.95000076*882.9099731/124390, NA_real_),
    `Krysel et al 2003 - MN - Bemidji Lake Group` = if_else(x <= 500, 9.720000267*1101.5/142829, NA_real_),
    `Liao et al 2016 - Model 1` = if_else(x <= 500, 0.275000006, NA_real_),
    `Liao et al 2016 - Model 3` = if_else(x <= 500, 0.231999993, NA_real_),
    `Liao et al 2016 - Spatial Regime Model 1` = if_else(x <= 500, 0.219999999, NA_real_),
    `Liao et al 2016 - Spatial Regime Model 1` = if_else(x <= 500, 0.165000007, NA_real_),
    `Liao et al 2016 - Spatial Regime Model 3` = if_else(x <= 500, 0.223000005, NA_real_),
    `Liao et al 2016 - Spatial Regime Model 3` = if_else(x <= 500, 0.165999994, NA_real_),
    `Liu et al 2014 - Sale Price - semi-log` = if_else(x <= 2000, 0.30340001*0.709999979, NA_real_),
    `Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:1` = if_else(x <= 2000, -0.0145, NA_real_) ,
    `Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:2` = if_else(x <= 2000, 0.024, NA_real_),
    `Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:3` = if_else(x <= 2000, -0.0094, NA_real_),
    `Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:4` = if_else(x <= 2000, 0.028, NA_real_),
    `Michael et al 2000 - Model 1 - Group 1 - CMIN` = if_else(x <= 500, 0.413004667, NA_real_),
    `Michael et al 2000 - Model 1 - Group 2 -  CMIN` = if_else(x <= 500, 0.093727589, NA_real_),
    `Michael et al 2000 - Model 1 - Group 3 -  CMIN` = if_else(x <= 500, 0.491040945, NA_real_),
    `Michael et al 2000 - Model 2 - Group 1 -  PMIN` = if_else(x <= 500, 0.285336018, NA_real_), 
    `Michael et al 2000 - Model 2 - Group 2 -  PMIN` = if_else(x <= 500, 0.10351032, NA_real_), 
    `Michael et al 2000 - Model 2 - Group 3 - PMIN` = if_else(x <= 500, 0.664476693, NA_real_), 
    `Michael et al 2000 - Model 3 - Group 1 - HMIN` = if_else(x <= 500, 0.376048774, NA_real_),   
    `Michael et al 2000 - Model 3 - Group 2 - HMIN` = if_else(x <= 500, 0.138936102, NA_real_), 
    `Michael et al 2000 -Model 3 - Group 3 - HMIN` = if_else(x <= 500, 0.916609764, NA_real_),  
    `Michael et al 2000 -Model 4 - Group 1 - CMIN * HMIN` = if_else(x <= 500, 0.385807753, NA_real_), 
    `Michael et al 2000 -Model 4 - Group 2 - CMIN * HMIN` = if_else(x <= 500, 0.079734102, NA_real_),  
    `Michael et al 2000 - Model 4 - Group 3 - CMIN * HMIN` = if_else(x <= 500, 0.431535065, NA_real_), 
    `Michael et al 2000 - Model 5 - Group 1 - CMIN * HMIN +/-` = if_else(x <= 500, -0.062146027, NA_real_), 
    `Michael et al 2000 - Model 5 - Group 2 - CMIN * HMIN +/-` = if_else(x <= 500, 0.015297231, NA_real_),  
    `Michael et al 2000 - Model 5 - Group 3 - CMIN * HMIN +/-` = if_else(x <= 500, -0.054691911, NA_real_),  
    `Michael et al 2000 - Model 6 - Group 1 - CMAX/CMIN` = if_else(x <= 500, 0.040301826, NA_real_),
    `Michael et al 2000 - Model 6 - Group 2 - CMAX/CMIN` = if_else(x <= 500, 0.004697338, NA_real_),
    `Michael et al 2000 - Model 6 - Group 3 - CMAX/CMIN` = if_else(x <= 500, 0.193003386, NA_real_),
    `Michael et al 2000 - Model 7 - Group 1 - CMAX/CMIN%` = if_else(x <= 500, 0.432574719, NA_real_),
    `Michael et al 2000 - Model 7 - Group 2 - CMAX/CMIN%` = if_else(x <= 500, 0.104469016, NA_real_),
    `Michael et al 2000 - Model 7 - Group 3 - CMAX/CMIN%` = if_else(x <= 500, 0.542586148, NA_real_),
    `Michael et al 2000 - Model 8 - Group 1 - CMIN-HMIN` = if_else(x <= 500, 0.452805698, NA_real_),
    `Michael et al 2000 - Model 8 - Group 2 - CMIN-HMIN` = if_else(x <= 500, 0.114391185, NA_real_),
    `Michael et al 2000 - Model 8 - Group 3 - CMIN-HMIN` = if_else(x <= 500, 0.589391351, NA_real_),
    `Michael et al 2000 - Model 9 - Group 1 - HMIN+ and HMIN -` = if_else(x <= 500, 0.325085133, NA_real_),
    `Michael et al 2000 - Model 9 - Group 2 - HMIN+ and HMIN -` = if_else(x <= 500, 0.098492384, NA_real_),
    `Michael et al 2000 - Model 9 - Group 3 - HMIN+ and HMIN -` = if_else(x <= 500, 0.401052326, NA_real_),
    `Moore et al 2020 - OLS - 1a` = if_else(x <= 160, 2.1*0.151, NA_real_),
    `Moore et al 2020 - IV-GMM - 1a` = if_else(x <= 160, 2.1*0.154, NA_real_),
    `Moore et al 2020 - OLS - 2a` = if_else(x <= 160, 2.1*0.114, NA_real_),
    `Moore et al 2020 - IV-GMM - 2b` = if_else(x <= 160, 2.1*0.117, NA_real_),
    `Moore et al 2020 - OLS - 3a` = if_else(x <= 160, 2.1*0.121, NA_real_),
    `Moore et al 2020 - IV-GMM - 3b` = if_else(x <= 160, 2.1*0.126, NA_real_),
    `Moore et al 2020 - OLS - 4a` = if_else(x <= 160, 2.1*0.099, NA_real_),
    `Moore et al 2020 - IV-GMM - 4b` = if_else(x <= 160, 2.1*0.101, NA_real_),
    `Moore et al 2020 - OLS - 1a` = if_else(x <= 160, 2.1*0.151,NA_real_),
    `Moore et al 2020 - OLS - 1a` = if_else(x <= 160, 2.1*0.151,NA_real_),
    `Moore et al 2020 - OLS - 1a` = if_else(x <= 160, 2.1*0.151,NA_real_),
    `Moore et al 2020 - OLS - 4a` = if_else(x <= 160, 2.1*0.096,NA_real_),
    `Moore et al 2020 - OLS - 4a` = if_else(x <= 160, 2.1*0.094,NA_real_),
    `Moore et al 2020 - OLS - 4a` = if_else(x <= 160, 2.1*0.098,NA_real_),
    `Moore et al 2020 - OLS - 1a - Florida` = if_else(x <= 160, 2.1*(0.153 + -0.118*1 + -0.068*0 + -0.017*0),NA_real_),
    `Moore et al 2020 - OLS - 1a - Indiana` = if_else(x <= 160, 2.1*(0.153 + -0.118*0 + -0.068*1 + -0.017*0),NA_real_),
    `Moore et al 2020 - OLS - 1a - Washington` = if_else(x <= 160, 2.1*(0.153 + -0.118*0 + -0.068*0 + -0.017*1),NA_real_),
    `Moore et al 2020 - OLS - 2a - Florida` = if_else(x <= 160, 2.1*(0.128 + 0.407*1 + -0.029*0 + -0.02*0),NA_real_),
    `Moore et al 2020 - OLS - 2a - Indiana` = if_else(x <= 160, 2.1*(0.128 + 0.407*0 + -0.029*1 + -0.02*0),NA_real_),
    `Moore et al 2020 - OLS - 2a - Washington` = if_else(x <= 160, 2.1*(0.128 + 0.407*0 + -0.029*0 + -0.02*1),NA_real_),
    `Moore et al 2020 - OLS - 3a - Florida` = if_else(x <= 160, 2.1*(0.138 + 0.358*1 + -0.041*0 + -0.03*0),NA_real_),
    `Moore et al 2020 - OLS - 3a - Indiana` = if_else(x <= 160, 2.1*(0.138 + 0.358*0 + -0.041*1 + -0.03*0),NA_real_),
    `Moore et al 2020 - OLS - 3a - Washington` = if_else(x <= 160, 2.1*(0.138 + 0.358*0 + -0.041*0 + -0.03*1),NA_real_),
    `Moore et al 2020 - OLS - 3a - Florida` = if_else(x <= 160, 2.1*(0.111 + 0.251*1 + -0.072*0 + -0.007*0),NA_real_),
    `Moore et al 2020 - OLS - 3a - Indiana` = if_else(x <= 160, 2.1*(0.111 + 0.251*0 + -0.072*1 + -0.007*0),NA_real_),
    `Moore et al 2020 - OLS - 3a - Washington` = if_else(x <= 160, 2.1*(0.111 + 0.251*0 + -0.072*0 + -0.007*1),NA_real_),
    `Moore et al 2020 - OLS - 1a` = if_else(x <= 160, 2.1*0.21,NA_real_),
    `Moore et al 2020 - OLS - 2a` = if_else(x <= 160, 2.1*0.09,NA_real_),
    `Moore et al 2020 - OLS - 3a` = if_else(x <= 160, 2.1*0.103,NA_real_),
    `Moore et al 2020 - OLS - 4a` = if_else(x <= 160, 2.1*0.085,NA_real_),
    `Moore et al 2020 - OLS - 1a` = if_else(x <= 160, 2.1*0.186,NA_real_),
    `Moore et al 2020 - OLS - 2a` = if_else(x <= 160, 2.1*0.07,NA_real_),
    `Moore et al 2020 - OLS - 3a` = if_else(x <= 160, 2.1*0.084,NA_real_),
    `Moore et al 2020 - OLS - 4a` = if_else(x <= 160, 2.1*0.089,NA_real_),
    `Olden & Tamayo 2014 - Model 1` = if_else(x <= 500, -7430.29*3.4/502312.8,NA_real_),
    `Olden & Tamayo 2014 - Model 2` = if_else(x <= 500, -6962.7*3.4/502312.8,NA_real_),
    `Olden & Tamayo 2014 - Model 3` = if_else(x <= 500, -6954.8*3.4/502312.8,NA_real_),
    `Poor et al 2001 - Lewiston` = if_else(x <= 500, 10.36999989*3209/103853,NA_real_),
    `Poor et al 2001 - Augusta` = if_else(x <= 500, 2.25*4391/86880,NA_real_),
    `Poor et al 2001 - Bangor` = if_else(x <= 500, 1.46*2517/67881,NA_real_),
    `Poor et al 2001 - Northern Maine` = if_else(x <= 500, -0.88*5161/31287,NA_real_),
    `Swedberg el al 2020 - Multi-state model- ME ###Outlier` = if_else(x <= 500, 0.17+6.80,NA_real_),
    `Swedberg el al 2020 - Multi-state model- MI` = if_else(x <= 500, 0.17+ -1.04,NA_real_),
    `Swedberg el al 2020 - Multi-state model-  NY` = if_else(x <= 500, 0.17+ 0.08,NA_real_),
    `Swedberg el al 2020 - Multi-state model-  VT` = if_else(x <= 500, 0.17+ 0.14,NA_real_),
    `Swedberg el al 2020 - Multi-state model-   WI` = if_else(x <= 500, 0.17+ -0.11,NA_real_),
    `Swedberg el al 2020 - Multi-state model` = if_else(x <= 500, 0.17,NA_real_),
    `Swedberg el al 2020 - Multi-state model - drop ME` = if_else(x <= 500, 0.17,NA_real_),
    `Swedberg el al 2020 - Multi-state model - drop MI` = if_else(x <= 500, 0.17,NA_real_),
    `Swedberg el al 2020 - Multi-state model - drop MN` = if_else(x <= 500, 0.13,NA_real_),
    `Swedberg el al 2020 - Multi-state model - drop NY` = if_else(x <= 500, 0.16,NA_real_),
    `Swedberg el al 2020 - Multi-state model - drop VT` = if_else(x <= 500, 0.17,NA_real_),
    `Swedberg el al 2020 - Multi-state model - drop WI` = if_else(x <= 500, 0.17,NA_real_),
    `Swedberg el al 2020 - State level model - Maine` = if_else(x <= 500, -2.71,NA_real_),
    `Swedberg el al 2020 - State level model - Michigan` = if_else(x <= 500, 9.20,NA_real_),
    `Swedberg el al 2020 - State level model - Minnesota` = if_else(x <= 500, 0.17,NA_real_),
    `Swedberg el al 2020 - State level model - New York` = if_else(x <= 500, 0.13,NA_real_),
    `Swedberg el al 2020 - State level model - Vermont` = if_else(x <= 500, -0.32,NA_real_),
    `Swedberg el al 2020 - State level model - Wisconsin` = if_else(x <= 500, 0.1,NA_real_),
    `Swedberg el al 2020 - Regional model - Otter Tail` = if_else(x <= 500, 0.5,NA_real_),
    `Swedberg el al 2020 - Regional model - Twin Cities` = if_else(x <= 500, 0.1,NA_real_),
    `Swedberg el al 2020 - Regional model - Adirondacks` = if_else(x <= 500, -0.21,NA_real_),
    `Swedberg el al 2020 - Regional model - Regional model - Finger Lakes` = if_else(x <= 500, 0.41,NA_real_),
    `Weng et al 2020 - Secci depth` = if_else(x <= 500, 0.129,NA_real_),
    `Wolf & Kemp 2021 - Model 1` = if_else(x <= 139, 0.0352 + 0.0755 * log(2.39),NA_real_),
    `Wolf & Kemp 2021 - Model 1` = if_else(x <= 139, 0.0136 + 0.0543 * log(2.39),NA_real_),
    `Wolf & Kemp 2021 - Model 2` = if_else(x <= 139, 0.0548 * log(2.39),NA_real_),
    `Wolf & Kemp 2021 - Model 2` = if_else(x <= 139, 0.0291 * log(2.39),NA_real_),
    `Wolf & Kemp 2021 - Model 3` = if_else(x <= 139, 0.0092 + 0.0498 * log(2.39),NA_real_),
    `Wolf & Kemp 2021 - Model 3` = if_else(x <= 139, -0.0205 + 0.0444 * log(2.39),NA_real_),
    `Wolf & Kemp 2021 - Model 4` = if_else(x <= 139, -0.0147 + 0.0447 * log(2.39),NA_real_),
    `Wolf & Kemp 2021 - Model 4` = if_else(x <= 139, -0.0242 + 0.0421 * log(2.39),NA_real_),
    `Wolf & Kemp 2021 - Model 5` = if_else(x <= 139, -0.0086 + 0.0432 * log(2.39),NA_real_),
    `Wolf & Kemp 2021 - Model 5` = if_else(x <= 139, 0.022 + 0.0407 * log(2.39),NA_real_),
    `Zhang & Boyle 2010 - Milfoil - Quadratic` = if_else(x <= 500, -0.647772014,NA_real_),
    `Zhang & Boyle 2010 - Milfoil - Exponential` = if_else(x <= 500, -0.647772014,NA_real_),
    `Zhang & Boyle 2010 - Total macrophytes - Quadratic` = if_else(x <= 500, -0.323886007,NA_real_),
    `Zhang & Boyle 2010 - Total macrophytes - Exponential` = if_else(x <= 500, -0.323886007,NA_real_),
    `Zhang & Boyle 2010 - Best Model - Quadratic` = if_else(x <= 500, -0.161943004,NA_real_),
    `Zhang & Boyle 2010 - Best Model - Exponential` = if_else(x <= 500, -0.323886007,NA_real_),
    `Zhang et al 2015 - VT1` = if_else(x <= 500, 0.18130137,NA_real_),
    `Zhang et al 2015 - VT2` = if_else(x <= 500, 0.168578461,NA_real_),
    `Zhang et al 2015 - VT3` = if_else(x <= 500, 0.031807259,NA_real_),
    `Zhang et al 2015 - ME1` = if_else(x <= 500, 0.368520737,NA_real_),
    `Zhang et al 2015 - ME2` = if_else(x <= 500, -0.044222489,NA_real_),
    `Zhang et al 2015 - ME3` = if_else(x <= 500, 0.078617759,NA_real_),
    `Zhang et al 2015 - ME4` = if_else(x <= 500, 0.560151517,NA_real_),
    `Zhang et al 2015 - ME5` = if_else(x <= 500, -0.00491361,NA_real_),
    `Zhang et al 2015 - ME6` = if_else(x <= 500, -0.01965444,NA_real_),
    `Zhang et al 2015 - ME7` = if_else(x <= 500, 1.719763517,NA_real_),
    `Zhang et al 2015 - NH1` = if_else(x <= 500, 0.035835754,NA_real_),
    `Zhang et al 2015 - NH2` = if_else(x <= 500, 0.04596325,NA_real_),
    `Zhang et al 2015 - NH3` = if_else(x <= 500, 1.505880713,NA_real_),
    `Zhang et al 2015 - NH4` = if_else(x <= 500, 0.971460581,NA_real_),
    `Zhang et al 2015 - NH5` = if_else(x <= 500, 0.185411081,NA_real_)
    
  )


# View results
head(results)


# Assuming 'results' dataframe is already created as shown in the previous example
results_nodist <- results %>%
  pivot_longer(
    cols = -x,  # Select all columns except 'x'
    names_to = "Function",  # New column name for the variable/function names
    values_to = "y_value"  # New column name for the y values
  )%>%
  mutate(distclass="nodist")

# View the long-format data
head(results_nodist)

# Plot using geom_point
ggplot(results_nodist, aes(x = x, y = y_value)) +
  geom_point(position = position_jitter(width = 0.2), size = 3, alpha = 0.7) + #Add jitter for visibility)
  labs(title = "Dot Plot of Extracted Y Values",
       x = "Groups",
       y = "Y Values") +
  theme_minimal() +
  theme(legend.title = element_blank())



##############################################################################
# Linear distance

# Define the range of x values
x_values <- seq(50, 1000, by = 50)

# Initialize a dataframe to store x and y values
results <- data.frame(x = x_values)


results <- results %>%
  mutate(
    `Irwin & Wolf 2022 - Model 5 (2km)` = if_else(x <= 2000, 0.248 - 0.076 * (log(x / 100)), NA_real_),
    `Liu et al 2019 - without distance threshold interaction (sample within 5 miles)` = if_else(x <= 2000, (0.0964 - 0.0305 * (x / 1609.34)) * 0.542544 * 3.28, NA_real_), 
    `Liu et al 2019 - property fixed effect model - 2 or more sales` = if_else(x <= 2000, (0.2096 - 0.0426 * (x / 1609.34)) * 0.542544 * 3.28, NA_real_),
    `Liu et al 2019 - property fixed effect model - 3 or more sales` = if_else(x <= 2000, (0.2638 - 0.0682 * (x / 1609.34)) * 0.542544 * 3.28, NA_real_),
    `Liu et al 2019 - property fixed effect model - 4 or more sales` = if_else(x <= 2000, (0.4503 - 0.1117 * (x / 1609.34)) * 0.542544 * 3.28, NA_real_),
    `Liu et al 2019 - linear model` = if_else(x <= 2000, (6966.6554 - 469.5857 * (x / 1609.34)) * (0.542544 * 3.28 / 217951.2), NA_real_)
  )

# Assuming 'results' dataframe is already created as shown in the previous example
results_linear <- results %>%
  pivot_longer(
    cols = -x,  # Select all columns except 'x'
    names_to = "Function",  # New column name for the variable/function names
    values_to = "y_value"  # New column name for the y values
  )%>%
  mutate(distclass="linear")


# Plot using geom_point
ggplot(results_linear, aes(x = x, y = y_value)) +
  geom_point(position = position_jitter(width = 0.2), size = 3, alpha = 0.7) + #Add jitter for visibility
  labs(title = "Dot Plot of Extracted Y Values",
       x = "Groups",
       y = "Y Values") +
  theme_minimal() +
  theme(legend.title = element_blank())





###########################################################################
# Linear distance + dummy distance

# Define the range of x values
x_values <- seq(50, 1000, by = 50)

# Initialize a dataframe to store x and y values
results <- data.frame(x = x_values)


results <- results %>%
  mutate(
    `Liu et al 2019 - waterfron adjucent 300m (1)` = if_else(x <= 300,(0.0876 + 0.1973*1 + (-0.0287* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 300m (0)` = if_else(x >= 301 & x <= 2000,(0.0876 + 0.1973*0 + (-0.0287* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 161m (1)` = if_else(x <= 161,(0.0914 + 0.1568*1 + (-0.0292* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 161m (0)` = if_else(x >= 161 & x <= 2000,(0.0914 + 0.1568*0 + (-0.0292* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 482m (1)` = if_else(x <= 482,(0.0937 + 0.0772*1 + (-0.0294* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 482m (0)` = if_else(x >= 482 & x <= 2000,(0.0937 + 0.0772*0 + (-0.0294* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 804m (1)` = if_else(x <= 804,(0.0951 + 0.02*1 + (-0.0302* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 804m (0)` = if_else(x >= 804 & x <= 2000,(0.0951 + 0.02*0 + (-0.0302* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 965m (1)` = if_else(x <= 965,(0.0961 + 0.0055*1 + (-0.0304* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 965m (0)` = if_else(x >= 965 & x <= 2000,(0.0961 + 0.0055*0 + (-0.0304* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 300m (1)` = if_else(x <= 300,(0.0742 + 0.1978*1 + (-0.0237* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 300m (0)` = if_else(x >= 300 & x <= 2000, (0.0742 + 0.1978*0 + (-0.023* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 161m (1)` = if_else(x <= 161,(0.0783 + 0.1559*1 + (-0.0236* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 161m (0)` = if_else(x >= 161 & x <= 2000,(0.0783 + 0.1559*0 + (-0.0236* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 482m (1)` = if_else(x <= 482,(0.0809 + 0.077*1 + (-0.0238* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 482m (0)` = if_else(x >= 482 & x <= 2000,(0.0809 + 0.077*0 + (-0.0238* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 804m (1)` = if_else(x <= 804,(0.0822 + 0.0285*1 + (-0.0245* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 804m (0)` = if_else(x >= 804 & x <= 2000,(0.0822 + 0.0285*0 + (-0.0245* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 965m (1)` = if_else(x <= 965,(0.0834 + 0.0148*1 + (-0.0249* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 965m (0)` = if_else(x >= 965 & x <= 2000,(0.0834 + 0.0148*0 + (-0.0249* x/1609.34))*0.542544*3.28,NA_real_),
    `Liu et al 2019 - waterfron adjucent 300m (1) linear` = if_else(x <= 300,(4165.1918 + 63099.4773*1 + (96.6563* x/1609.34))*(0.542544*3.28/217951.2),NA_real_),
    `Liu et al 2019 - waterfron adjucent 300m (0) linear` = if_else(x >= 300 & x <= 2000,(4165.1918 + 63099.4773*0 + (96.6563* x/1609.34))*(0.542544*3.28/217951.2),NA_real_),
    `Liu et al 2019 - waterfron adjucent 161m (1) linear` = if_else(x <= 161,(5450.1438 + 48099.1068*1 + (-80.7421* x/1609.34))*(0.542544*3.28/217951.2),NA_real_),
    `Liu et al 2019 - waterfron adjucent 161m (0) linear` = if_else(x >= 161 & x <= 2000,(5450.1438 + 48099.1068*0 + (-80.7421* x/1609.34))*(0.542544*3.28/217951.2),NA_real_),
    `Liu et al 2019 - waterfron adjucent 482m (1) linear` = if_else(x <= 482,(6215.4558 + 21609.3494*1 + (-169.2611* x/1609.34))*(0.542544*3.28/217951.2),NA_real_),
    `Liu et al 2019 - waterfron adjucent 482m (0) linear` = if_else(x >= 482 & x <= 2000, (6215.4558 + 21609.3494*0 + (-169.2611* x/1609.34))*(0.542544*3.28/217951.2),NA_real_),
    `Liu et al 2019 - waterfron adjucent 804m (1) linear` = if_else(x <= 804,(6215.4558 + 21609.3494*1 + (-169.2611* x/1609.34))*(0.542544*3.28/217951.2),NA_real_),
    `Liu et al 2019 - waterfron adjucent 804m (0) linear` = if_else(x >= 804 & x <= 2000, (6215.4558 + 21609.3494*0 + (-169.2611* x/1609.34))*(0.542544*3.28/217951.2),NA_real_),
    `Liu et al 2019 - waterfron adjucent 965m (1) linear` = if_else(x <= 965,(6807.1986 + 3826.6165*1 + (-437.7854* x/1609.34))*(0.542544*3.28/217951.2),NA_real_),
    `Liu et al 2019 - waterfron adjucent 965m (0) linear` = if_else(x >= 965 & x <= 2000, (6807.1986 + 3826.6165*0 + (-437.7854* x/1609.34))*(0.542544*3.28/217951.2),NA_real_),
    `Nepf et al 2022 - Model 1 - continious distance waterfron adjucent 300m (1)` = if_else(x <= 300,(0.0084 + 0.0099*1 + (-0.0016*x)/1000)*1.862328*3.28,NA_real_),
    `Nepf et al 2022 - Model 1 - continious distance waterfron adjucent 300m (0)` = if_else(x >= 300 & x <= 2000, (0.0084 + 0.0099*0 + (-0.0016*x)/1000)*1.862328*3.28,NA_real_),
    `Nepf et al 2022 - Model 2 - continious distance waterfron adjucent 300m (1)` = if_else(x <= 300,(0.0208 + 0.001*1 + (-0.0029*x)/1000)*1.862328*3.28,NA_real_),
    `Nepf et al 2022 - Model 2 - continious distance waterfron adjucent 300m (0)` = if_else(x >= 300 & x <= 2000,(0.0208 + 0.001*0 + (-0.0029*x)/1000)*1.862328*3.28,NA_real_),
    `Nepf et al 2022 - Model 3 - continious distance waterfron adjucent 300m (1)` = if_else(x <= 300,(0.0191 + 0.004*1 + (-0.0022*x)/1000)*1.862328*3.28,NA_real_),
    `Nepf et al 2022 - Model 3 - continious distance waterfron adjucent 300m (0)` = if_else(x >= 300 & x <= 2000, (0.0191 + 0.004*0 + (-0.0022*x)/1000)*1.862328*3.28,NA_real_),
    `Nepf et al 2022 - Model 4 - continious distance waterfron adjucent 300m (1)` = if_else(x <= 300, (0.0221 + -0.0003*1 + (-0.0031*x)/1000)*1.862328*3.28,NA_real_),
    `Nepf et al 2022 - Model 4 - continious distance waterfron adjucent 300m (0)` = if_else(x >= 300 & x <= 2000, (0.0221 + 0.004*0 + (-0.0003*x)/1000)*1.862328*3.28,NA_real_),
    `Nepf et al 2022 - Model 9 - continious distance/regional analysis waterfron adjucent 300m (1)` = if_else(x <= 300, (0.0026 + 0.0277*1 + (0.0005*x)/1000)*1.862328*3.28,NA_real_),
    `Nepf et al 2022 - Model 9 - continious distance/regional analysis waterfron adjucent 300m (0)` = if_else(x >= 300 & x <= 2000, (0.0026 + 0.0277*0  + (0.0005*x)/1000)*1.862328*3.28,NA_real_),
    `Nepf et al 2022 - Model 10 - continious distance/regional analysis waterfron adjucent 300m (1)` = if_else(x <= 300,(-0.0016 + 0.0262*1 + (0.0012*x)/1000)*1.862328*3.28,NA_real_),
    `Nepf et al 2022 - Model 10 - continious distance/regional analysis waterfron adjucent 300m (0)` = if_else(x >= 300 & x <= 2000, (-0.0016 + 0.0262*0  + (0.0012*x)/1000)*1.862328*3.28,NA_real_),
    `Nepf et al 2022 - Model 11 - continious distance waterfron adjucent 300m (1)` = if_else(x <= 300,(0.007 + 0.0263*1 + (0.0004*x)/1000)*1.862328*3.28,NA_real_),
    `Nepf et al 2022 - Model 11 - continious distance waterfron adjucent 300m (0)` = if_else(x >= 300 & x <= 2000, (0.007 + 0.0263*0  + (0.0004*x)/1000)*1.862328*3.28,NA_real_),
    `Nepf et al 2022 - Model 12 - continious distance waterfron adjucent 300m (1)` = if_else(x <= 300,(-0.009 + 0.0266*1 + (0.0016*x)/1000)*1.862328*3.28,NA_real_), 
    `Nepf et al 2022 - Model 12 - continious distance waterfron adjucent 300m (0)` = if_else(x >= 300 & x <= 2000, (-0.009 + 0.0266*0  + (0.0016*x)/1000)*1.862328*3.28,NA_real_), 
    `Walsh et al 2011a - Model 2 waterfron adjucent 50m (1)` = if_else(x <= 50,0.117 + 0.08*1 + -0.017* log(x),NA_real_), 
    `Walsh et al 2011a - Model 2 waterfron adjucent 50m (0)` = if_else(x >= 50 & x <= 467,  0.117 + 0.08*0 + -0.017* log(x) ,NA_real_),
    `Walsh et al 2011a - Model 3 waterfron adjucent 50m (1)` = if_else(x <= 50,0.117 + 0.08*1 + -0.017* log(x) + 0.011*log(519.2999878),NA_real_), 
    `Walsh et al 2011a - Model 3 waterfron adjucent 50m (0)` = if_else(x >= 50 & x <= 467,  0.117 + 0.08*0 + -0.017* log(x) + 0.011*log(519.2999878) ,NA_real_), 
    `Walsh et al 2011a - Model 2S waterfron adjucent 50m (1)` = if_else(x <= 50,0.118 + 0.081*1 + -0.017* log(x),NA_real_), 
    `Walsh et al 2011a - Model 2S waterfron adjucent 50m (0)` = if_else(x >= 50 & x <= 467,  0.118 + 0.08*0 + -0.017* log(x) ,NA_real_),
    `Walsh et al 2011a - Model 3S waterfron adjucent 50m (1)` = if_else(x <= 50,-0.03 + 0.079*1 + -0.017* log(x) + 0.012*log(519.2999878),NA_real_), 
    `Walsh et al 2011a - Model 3S waterfron adjucent 50m (0)` = if_else(x >= 50 & x <= 467,  0.117 + 0.08*0 + -0.017* log(x) + 0.011*log(519.2999878) ,NA_real_)
  )


# Assuming 'results' dataframe is already created as shown in the previous example
results_linearAndDistdum <- results %>%
  pivot_longer(
    cols = -x,  # Select all columns except 'x'
    names_to = "Function",  # New column name for the variable/function names
    values_to = "y_value"  # New column name for the y values
  )%>%
  mutate(distclass="linBuf")



# Plot using geom_point
ggplot(results_linearAndDistdum, aes(x = x, y = y_value)) +
  geom_point(position = position_jitter(width = 0.2), size = 3, alpha = 0.7) +# Add jitter for visibility
  labs(title = "Dot Plot of Extracted Y Values",
       x = "Groups",
       y = "Y Values") +
  theme_minimal() +
  theme(legend.title = element_blank())


############Distance Dummy##########Distance Dummy##########


# Define the range of x values
x_values <- seq(50, 1000, by = 50)

# Initialize a dataframe to store x and y values
results <- data.frame(x = x_values)



results <- results %>%
  mutate(
    `Guignet et al 2017 - 2.C:0_200m` = if_else(x <= 200, -0.099399999*-1,NA_real_), 
    `Guignet et al 2017 - 2.C:200_500m` = if_else(x >= 200 & x <= 500, -0.0058*-1,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year:0_100` = if_else(x <= 100,0.0076 + 0.1673, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year:100_300` = if_else(x >= 100 & x <= 300 , 0.0076 + 0.0405,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0076,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:County*year:0_100` = if_else(x <= 100, 0.0199 + 0.1778, NA_real_), 
    `Mamun et al 2023:Model 1 - discrete distance bins/FE:County*year:100_300` = if_else(x >= 100 & x <= 300 , 0.0199 + 0.0435, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:County*year:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0199,NA_real_), 
    `Mamun et al 2023-Model 1:discrete distance bins/FE:State*year:0_100` = if_else(x <= 100, -0.0383 + 0.1758, NA_real_), 
    `Mamun et al 2023-Model 1:discrete distance bins/FE:State*year:100_300` = if_else(x >= 100 & x <= 300 , -0.0383 + 0.0477, NA_real_), 
    `Mamun et al 2023-Model 1:discrete distance bins/FE:State*year:300_2000` = if_else(x >= 300 & x <= 2000 , -0.0383,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract+year:0_100` = if_else(x <= 100, -0.0105 + 0.1674, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract+year:100_300` = if_else(x >= 100 & x <= 300 , -0.0105 + 0.0437, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract+year:300_2000` = if_else(x >= 300 & x <= 2000 , -0.0105,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:County+year:0_100` = if_else(x <= 100, 0.0141 + 0.1804, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:County+year:100_300` = if_else(x >= 100 & x <= 300 , 0.0141 + 0.0475, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:County+year:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0141,NA_real_), 
    `Mamun et al 2023 (Model 1 - discrete distance bins/FE:State+year)` = if_else(x <= 100, -0.0343 + 0.1797, NA_real_), 
    `Mamun et al 2023 (Model 1 - discrete distance bins/FE:State+year)` = if_else(x >= 100 & x <= 300 , -0.0343 + 0.0514, NA_real_), 
    `Mamun et al 2023 (Model 1 - discrete distance bins/FE:State+year)` = if_else(x >= 300 & x <= 2000 , -0.0343,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:Block*year:0_100` = if_else(x <= 100, 0.0287 + 0.1581, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:Block*year:100_300` = if_else(x >= 100 & x <= 300 , 0.0287 + 0.0388, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:Block*year:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0287,NA_real_), 
    `Mamun et al 2023- Model 1:discrete distance bins/FE:Block+year:0_100` = if_else(x <= 100, 0.0108 + 0.1568, NA_real_), 
    `Mamun et al 2023- Model 1:discrete distance bins/FE:Block+year:100_300` = if_else(x >= 100 & x <= 300 , 0.0108 + 0.0413, NA_real_), 
    `Mamun et al 2023- Model 1:discrete distance bins/FE:Block+year:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0108,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:year:0_100` = if_else(x <= 100, -0.0051 + 0.2315, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:year:100_300` = if_else(x >= 100 & x <= 300 , -0.0051 + 0.0726, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:year:300_2000` = if_else(x >= 300 & x <= 2000 , -0.0051,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:tract:0_100` = if_else(x <= 100, -0.0123 + 0.1678, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:tract:100_300` = if_else(x >= 100 & x <= 300 , -0.0123 + 0.0439, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/FE:tract:300_2000` = if_else(x >= 300 & x <= 2000 , -0.0123,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S4:2:0_100` = if_else(x <= 100, 0.0085 + 0.1573, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S4:2:100_300` = if_else(x >= 100 & x <= 300 , 0.0085 + 0.0414, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S4:2:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0085,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S4:3:0_100` = if_else(x <= 100, 0.0072 + 0.1532, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S4:3:100_300` = if_else(x >= 100 & x <= 300 , 0.0072 + 0.0401, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S4:3:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0072,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S4:5:0_100` = if_else(x <= 100, 0.0136 + 0.1586, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S4:5:100_300` = if_else(x >= 100 & x <= 300 , 0.0136 + 0.0455, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S4:5:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0136,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S4:7:0_100` = if_else(x <= 100, 0.0101 + 0.1666, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S4:7:100_300` = if_else(x >= 100 & x <= 300 , 0.0101 + 0.055, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S4:7:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0101,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S4:10:0_100` = if_else(x <= 100, -0.0018 + 0.1377, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S4:10:100_300` = if_else(x >= 100 & x <= 300 , -0.0018 + 0.0497, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S4:10:300_2000` = if_else(x >= 300 & x <= 2000 , -0.0018,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S5:10:0_100` = if_else(x <= 100, 0.0108 + 0.1654, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S5:10:100_300` = if_else(x >= 100 & x <= 300 , 0.0108 + 0.0417, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S5:10:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0108,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S5:30:0_100` = if_else(x <= 100, 0.0225 + 0.1582, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S5:30:100_300` = if_else(x >= 100 & x <= 300 , 0.0225 + 0.0472, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S5:30:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0225,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S5:100:0_100` = if_else(x <= 100, 0.0231 + 0.129, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S5:100:100_300` = if_else(x >= 100 & x <= 300 , 0.0231 + 0.0461, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S5:300:2000` = if_else(x >= 300 & x <= 2000 , 0.0231,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S6:5:0_100` = if_else(x <= 100, 0.0104 + 0.1461, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S6:5:100_300` = if_else(x >= 100 & x <= 300 , 0.0104 + 0.0364, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S6:5:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0104,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:1:waterfront we define 300m(1)` = if_else(x <= 300, 0.0123 + 0.1563, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:1:waterfront we define 300m(0)` = if_else(x >= 300 & x <= 2000 , 0.0123,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:2` = if_else(x <= 50, 0.0424 + 0.0953, NA_real_), #:watefront 50m(1)
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:2` = if_else(x >= 50 & x <= 2000 , 0.0424,NA_real_), #:watefront 50m(0)
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:3)` = if_else(x <= 100, 0.0157 + 0.151, NA_real_), #:watefront 100m(1)
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:3)` = if_else(x >= 100 & x <= 2000 , 0.0157,NA_real_), #:watefront 100m(0)
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:4` = if_else(x <= 200, 0.0076 + 0.1296, NA_real_), #:watefront 200m(1)
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:4` = if_else(x >= 200 & x <= 2000 , 0.0076,NA_real_), #:watefront 200m(0)
    `Mamun et al 2023-Model 1:discrete distance bins/Table S7:5:0_50` = if_else(x <= 50, 0.022 + 0.1439, NA_real_), 
    `Mamun et al 2023-Model 1:discrete distance bins/Table S7:5:50_200` = if_else(x >= 50 & x <= 200 , 0.022 + 0.1134,NA_real_), 
    `Mamun et al 2023-Model 1:discrete distance bins/Table S7:5:200_2000` = if_else(x >= 200 & x <= 2000 , 0.022, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:6:0_100` = if_else(x <= 100, 0.0104 + 0.1636, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:6:100_200` = if_else(x >= 100 & x <= 200 , 0.0104 + 0.0507,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:6:200_2000` = if_else(x >= 200 & x <= 2000 , 0.0104, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:7:0_100` = if_else(x <= 100, 0.0076 + 0.1673, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:7:100_300` = if_else(x >= 100 & x <= 300 , 0.0076 + 0.0405,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:7:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0076, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:8:0_200` = if_else(x <= 200, 0.0037 + 0.1359, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:8:200_500` = if_else(x >= 200 & x <= 500 , 0.0037 + 0.0132,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:8:500_2000` = if_else(x >= 500 & x <= 2000 , 0.0037, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:9:0_300` = if_else(x <= 300, 0.0019 + 0.1089, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:9:300_1000` = if_else(x >= 300 & x <= 1000 , 0.0019 + 0.0031,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S7:9:100_2000` = if_else(x >= 1000 & x <= 2000 , 0.0019, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S8:0:0_100` = if_else(x <= 100, 0.0041 + 0.155, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S8:0:100_300` = if_else(x >= 100 & x <= 300 , 0.0041 + 0.041,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S8:0:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0041, NA_real_), 
    `Mamun et al 2023- Model 1:discrete distance bins/Table S8:1:0_100` = if_else(x <= 100, 0.0166 + 0.1579, NA_real_), 
    `Mamun et al 2023- Model 1:discrete distance bins/Table S8:1:100_300` = if_else(x >= 100 & x <= 300 , 0.0166 + 0.0375,NA_real_), 
    `Mamun et al 2023- Model 1:discrete distance bins/Table S8:1:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0166, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S8:3:0_100` = if_else(x <= 100, 0.0121 + 0.1645, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S8:3:100_300` = if_else(x >= 100 & x <= 300 , 0.0121 + 0.0393,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S8:3:300-2000` = if_else(x >= 300 & x <= 2000 , 0.0121, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S8:5:0_100` = if_else(x <= 100, 0.0076 + 0.1673, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S8:5:100_300` = if_else(x >= 100 & x <= 300 , 0.0076 + 0.0405,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S8:5:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0076, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S8:10:0_100` = if_else(x <= 100, 0.0072 + 0.1688, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S8:10:100_300` = if_else(x >= 100 & x <= 300 , 0.0072 + 0.041,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S8:10:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0072, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S8:anytime:0_100` = if_else(x <= 100, 0.0078 + 0.1728, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S8:anytime:100_300` = if_else(x >= 100 & x <= 300 , 0.0078 + 0.0382,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S8:anytime:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0078, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S9:customer price index:0_100` = if_else(x <= 100, 0.0076 + 0.1673, NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S9:customer price index:100-300` = if_else(x >= 100 & x <= 300 , 0.0076 + 0.0405,NA_real_), 
    `Mamun et al 2023 - Model 1:discrete distance bins/Table S9:customer price index:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0076, NA_real_), 
    `Mamun et al 2023 - Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index:0_100` = if_else(x <= 100, 0.0072 + 0.1532, NA_real_), 
    `Mamun et al 2023 - Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index:100_300` = if_else(x >= 100 & x <= 300 , 0.0072 + 0.0401,NA_real_), 
    `Mamun et al 2023 - Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0072, NA_real_), 
    `Mamun et al 2023 - Model 2:discrete distance bins/Table S10:1:0_100` = if_else(x <= 100, 0.0126 + 0.1148, NA_real_), 
    `Mamun et al 2023 - Model 2:discrete distance bins/Table S10:1:100_300` = if_else(x >= 100 & x <= 300 , 0.0126 + 0.0184,NA_real_), 
    `Mamun et al 2023 - Model 2:discrete distance bins/Table S10:1:300-2000` = if_else(x >= 300 & x <= 2000 , 0.0126, NA_real_), 
    `Mamun et al 2023 - Model 2:discrete distance bins/Table S10:2:0_100` = if_else(x <= 100, 0.0173 + 0.1676, NA_real_), 
    `Mamun et al 2023 - Model 2:discrete distance bins/Table S10:2:100_300` = if_else(x >= 100 & x <= 300 , 0.0173 + 0.0401,NA_real_), 
    `Mamun et al 2023 - Model 2:discrete distance bins/Table S10:2:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0401, NA_real_), 
    `Mamun et al 2023 - Model 2:discrete distance bins/Table S10:4:0_100` = if_else(x <= 100, 0.0076 + 0.1673, NA_real_), 
    `Mamun et al 2023 - Model 2:discrete distance bins/Table S10:4:100-300` = if_else(x >= 100 & x <= 300 , 0.0076 + 0.0405,NA_real_), 
    `Mamun et al 2023 - Model 2:discrete distance bins/Table S10:4:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0076, NA_real_), 
    `Mamun et al 2023 -Model 3:discrete distance bins/Table S11:ln(housing price)):0_100` = if_else(x <= 100, 0.007 + 0.0789, NA_real_), 
    `Mamun et al 2023 -Model 3:discrete distance bins/Table S11:ln(housing price)):100_300` = if_else(x >= 100 & x <= 300 , 0.007 + 0.0227,NA_real_), 
    `Mamun et al 2023 -Model 3:discrete distance bins/Table S11:ln(housing price)):300_2000` = if_else(x >= 300 & x <= 2000 , 0.007, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:CPL:0_100` = if_else(x <= 100, 0.0113 + 0.1285 + -0.0469, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:CPL:100_300` = if_else(x >= 100 & x <= 300 , 0.0113 + 0.0204  + 0.0155,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:CPL:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0113, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NAP:0_100` = if_else(x <= 100, 0.0113 + 0.1285 + 0.0824, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NAP:100_300` = if_else(x >= 100 & x <= 300 , 0.0113 + 0.0204 + 0.0333,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NAP:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0113, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NPL:0_100` = if_else(x <= 100, 0.0113 + 0.1285 + -0.2392, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NPL:100_300` = if_else(x >= 100 & x <= 300 , 0.0113 + 0.0204 + -0.0445,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NPL:300_1000` = if_else(x >= 300 & x <= 2000 , 0.0113, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SAP):0_100` = if_else(x <= 100, 0.0113 + 0.1285 + 0.1601, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SAP):100_300` = if_else(x >= 100 & x <= 300 , 0.0113 + 0.0204 + 0.1002,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SAP):300_2000` = if_else(x >= 300 & x <= 2000 , 0.0113, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SPL:0_100` = if_else(x <= 100, 0.0113 + 0.1285 + -0.1651, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SPL:100_300` = if_else(x >= 100 & x <= 300 , 0.0113 + 0.0204 + -0.0836,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SPL:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0113, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:TPL:0_100` = if_else(x <= 100, 0.0113 + 0.1285 + -0.1553, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:TPL:100_300` = if_else(x >= 100 & x <= 300 , 0.0113 + 0.0204 + -0.0477,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:TPL:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0113, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:WMT:0_100` = if_else(x <= 100, 0.0113 + 0.1285 + 0.0551, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:WMT:100_300` = if_else(x >= 100 & x <= 300 , 0.0113 + 0.0204 + 0.0303,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:WMT:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0113, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:XER:0_100` = if_else(x <= 100, 0.0113 + 0.1285 + -0.0765, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:XER:100_300` = if_else(x >= 100 & x <= 300 , 0.0113 + 0.0204 + 0.0002,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:XER:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0113, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AL:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.1364, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AL:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.0949,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AL:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AR:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.0547, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AR:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.207,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AR:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AZ:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.1572, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AZ:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.0368,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AZ:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CA):0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.1312, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CA):100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.0552,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CA):300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CO):0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.2389, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CO):100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.0367,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CO):300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CT):0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.1809, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CT):100-300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.065,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CT):300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:DE):0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.5047, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:DE):100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.2419,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:DE):300-2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:FL:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.1284, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:FL:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.0018,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:FL:300:2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:GA:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.5163, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:GA:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.2384,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:GA:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IA:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.0758, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IA:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.1,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IA:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IL:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.3359, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IL:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.2071,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IL:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IN:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.0794, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IN:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.039,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IN:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:KY:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 1.2532, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:KY:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.0888,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:KY:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:LA:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.0273, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:LA:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.0462,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:LA:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MA:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.0285, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MA:100-300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.059,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MA:300-2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MD:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.1581, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MD:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.0346,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MD:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:ME:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.1299, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:ME:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.1871,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:ME:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MI:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.0913, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MI:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.0146,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MI:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MO):0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.0672, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MO):100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.2849,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MO):300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MS:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.3935, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MS:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.6851,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MS:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MT:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.4651, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MT:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.3235,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MT:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NC):0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.108, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NC):100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.047,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NC):300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NE:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.0542, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NE:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.1447,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NE:300-2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NH:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.0968, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NH:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.0315,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NH:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NJ:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.2329, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NJ:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.0775,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NJ:300-2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NV:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.0915, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NV:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.0197,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NV:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NY:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.1677, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NY:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.074,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NY:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OH:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.3565, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OH:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.051,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OH:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OK:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.3811, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OK:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.3099,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OK:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OR:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.0729, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OR:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.0604,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OR:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:PA:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.0623, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:PA:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.0366,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:PA:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_),
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:RI:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.4243, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:RI:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.0761,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:RI:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SC:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.2388, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SC:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.1485,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SC:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SD:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.0699, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SD:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.0298,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SD:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TN:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.1608, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TN:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.1244,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TN:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TX:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.1807, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TX:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.0074,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TX:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:UT:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.9602, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:UT:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.0612,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:UT:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VA):0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.3189, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VA):100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.0059,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VA):300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VT:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + 0.0231, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VT:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + 0.0491,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VT:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WA:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.0623, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WA:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.0217,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WA:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WI:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.0069, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WI:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.0208,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WI:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WV:0_100` = if_else(x <= 100, 0.0134 + 0.1253 + -0.0314, NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WV:100_300` = if_else(x >= 100 & x <= 300 , 0.0134 + 0.0238 + -0.1154,NA_real_), 
    `Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WV:300_2000` = if_else(x >= 300 & x <= 2000 , 0.0134, NA_real_), 
    `Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:WF_300(1)` = if_else(x <= 300,(0.0190*1 + 0.0138*0 + 0.0083*0 + 0.0095*0 + -0.0003*0)*3.28*1.86 , NA_real_), 
    `Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:0_500` = if_else(x >= 0 & x <= 500 , (0.0190*0 + 0.0138*1 + 0.0083*0 + 0.0095*0 + -0.0003*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:500_1000` = if_else(x >= 500 & x <= 1000 , (0.0190*0 + 0.0138*0 + 0.0083*1 + 0.0095*0 + -0.0003*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:1000_1500` = if_else(x >= 1000 & x <= 1500 , (0.0190*0 + 0.0138*0 + 0.0083*0 + 0.0095*1 + -0.0003*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:1500:2000` = if_else(x >= 1500 & x <= 2000 , (0.0190*0 + 0.0138*0 + 0.0083*0 + 0.0095*1 + -0.0003*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 6:discrete distance bins:Town FE:WF_300(1)` = if_else(x <= 300,(0.0088 + 0.0108*1 + 0.0110*0 + 0.0077*0 + 0.0088*0 + 0.0040*0)*3.28*1.86 , NA_real_), 
    `Nepf et al 2022 - Model 6:discrete distance bins:Town FE:0_500` = if_else(x >= 0 & x <= 500 , (0.0088 + 0.0108*0 + 0.0110*1 + 0.0077*0 + 0.0088*0 + 0.0040*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 6:discrete distance bins:Town FE:500_1000` = if_else(x >= 500 & x <= 1000 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*1 + 0.0088*0 + 0.0040*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 6:discrete distance bins:Town FE:1000_1500` = if_else(x >= 1000 & x <= 1500 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*0 + 0.0088*1 + 0.0040*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 6:discrete distance bins:Town FE:1500_2000` = if_else(x >= 1500 & x <= 2000 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*0 + 0.0088*0 + 0.0040*1)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 7:discrete distance bins:School FE:WF_300(1)` = if_else(x <= 300,(0.0051 + 0.0155*1 + 0.0139*0 + 0.0116*0 + 0.0132*0 + 0.0046*0)*3.28*1.86 , NA_real_), 
    `Nepf et al 2022 - Model 7:discrete distance bins:School FE:0_500` = if_else(x >= 0 & x <= 500 , (0.0051 + 0.0155*0 + 0.0139*1 + 0.0116*0 + 0.0132*0 + 0.0046*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 7:discrete distance bins:School FE:500_1000` = if_else(x >= 500 & x <= 1000 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*1 + 0.0132*0 + 0.0046*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 7:discrete distance bins:School FE:1000_1500` = if_else(x >= 1000 & x <= 1500 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*0 + 0.0132*1 + 0.0046*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 7:discrete distance bins:School FE:1500_2000` = if_else(x >= 1500 & x <= 2000 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*0 + 0.0132*0 + 0.0046*1)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:WF_300(1)` = if_else(x <= 300,(0.0090 + 0.0103*1 + 0.0118*0 + 0.0084*0 + 0.0094*0 + 0.0051*0)*3.28*1.86 , NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:0_500` = if_else(x >= 0 & x <= 500 , (0.0090 + 0.0103*0 + 0.0118*1 + 0.0084*0 + 0.0094*0 + 0.0051*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:500_1000` = if_else(x >= 500 & x <= 1000 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*1 + 0.0094*0 + 0.0051*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:1000_1500` = if_else(x >= 1000 & x <= 1500 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*0 + 0.0094*1 + 0.0051*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:1500_2000` = if_else(x >= 1500 & x <= 2000 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*0 + 0.0094*0 + 0.0051*1)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:WF_300(1)` = if_else(x <= 300,(0.0050 + 0.0260*1 + 0.0096*0 + -0.0141*0 + -0.0056*0 + -0.0079*0)*3.28*1.86 , NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:0_500` = if_else(x >= 0 & x <= 500 , (0.0050 + 0.0260*0 + 0.0096*1 + -0.0141*0 + -0.0056*0 + -0.0079*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:500_1000` = if_else(x >= 500 & x <= 1000 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*1 + -0.0056*0 + -0.0079*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:1000_1500` = if_else(x >= 1000 & x <= 1500 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*0 + -0.0056*1 + -0.0079*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:1500_2000` = if_else(x >= 1500 & x <= 2000 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*0 + -0.0056*0 + -0.0079*1)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:WF_300(1)` = if_else(x <= 300,(0.0072 + 0.0170*1 + -0.0048*0 + -0.0193*0 + -0.0007*0 + -0.0167*0)*3.28*1.86 , NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:0_500` = if_else(x >= 0 & x <= 500 , (0.0072 + 0.0170*0 + -0.0048*1 + -0.0193*0 + -0.0007*0 + -0.0167*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:500_1000` = if_else(x >= 500 & x <= 1000 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*1 + -0.0007*0 + -0.0167*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:1000_1500` = if_else(x >= 1000 & x <= 1500 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*0 + -0.0007*1 + -0.0167*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:1500_2000` = if_else(x >= 1500 & x <= 2000 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*0 + -0.0007*0 + -0.0167*1)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:WF_300(1)` = if_else(x <= 300,(0.0098 + 0.0248*1 + 0.0114*0 + -0.0144*0 + -0.0011*0 + -0.0144*0)*3.28*1.86 , NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:0_500` = if_else(x >= 0 & x <= 500 , (0.0098 + 0.0248*0 + 0.0114*1 + -0.0144*0 + -0.0011*0 + -0.0144*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:500_1000` = if_else(x >= 500 & x <= 1000 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*1 + -0.0011*0 + -0.0144*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:1000_1500` = if_else(x >= 1000 & x <= 1500 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*0 + -0.0011*1 + -0.0144*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:1500_2000` = if_else(x >= 1500 & x <= 2000 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*0 + -0.0011*0 + -0.0144*1)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:WF_300(1)` = if_else(x <= 300,(0.0095 + 0.0156*1 + -0.0063*0 + -0.0205*0 + -0.0027*0 + -0.0184*0)*3.28*1.86 , NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:0_500` = if_else(x >= 0 & x <= 500 , (0.0095 + 0.0156*0 + -0.0063*1 + -0.0205*0 + -0.0027*0 + -0.0184*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:500_1000` = if_else(x >= 500 & x <= 1000 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*1 + -0.0027*0 + -0.0184*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:1000_1500` = if_else(x >= 1000 & x <= 1500 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*0 + -0.0027*1 + -0.0184*0)*3.28*1.86,NA_real_), 
    `Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:1500_2000` = if_else(x >= 1500 & x <= 2000 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*0 + -0.0027*0 + -0.0184*1)*3.28*1.86,NA_real_), 
    `TJ, Pat, Richard - Model 1:discrete distance bin:500` = if_else(x <= 500, 0.06, NA_real_), 
    `TJ, Pat, Richard - Model 2:discrete distance bin: 0_250` = if_else(x <= 250, 0.063, NA_real_), 
    `TJ, Pat, Richard - Model 2:discrete distance bin: 250_500` = if_else(x >= 250 & x <= 500 , 0.056,NA_real_), 
    `TJ, Pat, Richard - Model 3:discrete distance bin: 0_250` = if_else(x <= 250, 0.056, NA_real_), 
    `TJ, Pat, Richard - Model 3:discrete distance bin: 250_500` = if_else(x >= 250 & x <= 500 , 0.007,NA_real_), 
    `TJ, Pat, Richard - Model 4:discrete distance bin:500` = if_else(x <= 500, 0.06, NA_real_), 
    `TJ, Pat, Richard - Model 5:discrete distance bin:0_250` = if_else(x <= 250, 0.061, NA_real_), 
    `TJ, Pat, Richard - Model 5:discrete distance bin:250_500` = if_else(x >= 250 & x <= 500 , 0.059,NA_real_), 
    `TJ, Pat, Richard - Model 6:discrete distance bin:0_250` = if_else(x <= 250, 0.087, NA_real_), 
    `TJ, Pat, Richard - Model 6:discrete distance bin:250_500` = if_else(x >= 250 & x <= 500 , 0.063,NA_real_), 
    `TJ, Pat, Richard - Model 7:discrete distance bin:0_250` = if_else(x <= 250, 0.063, NA_real_), 
    `TJ, Pat, Richard - Model 7:discrete distance bin:250_500` = if_else(x >= 250 & x <= 500 , 0.056,NA_real_), 
    `TJ, Pat, Richard - Model 8:discrete distance bin:500` = if_else(x <= 500, 0.044, NA_real_), 
    `TJ, Pat, Richard - Model 9:discrete distance bin:0_250` = if_else(x <= 250, 0.041, NA_real_), 
    `TJ, Pat, Richard - Model 9:discrete distance bin:250_500` = if_else(x >= 250 & x <= 500 ,0.048,NA_real_), 
    `TJ, Pat, Richard - Model 9:discrete distance bin:0_250` = if_else(x <= 250, 0.041, NA_real_), 
    `TJ, Pat, Richard - Model 9:discrete distance bin:250_500` = if_else(x >= 250 & x <= 500 ,0.048,NA_real_), 
    `TJ, Pat, Richard - Model 10:discrete distance bin:0_250` = if_else(x <= 250, 0.152, NA_real_), 
    `TJ, Pat, Richard - Model 10:discrete distance bin:250_500` = if_else(x >= 250 & x <= 500 ,0.083,NA_real_), 
    `TJ, Pat, Richard - Model 11:discrete distance bin:0_100` = if_else(x <= 100, 0.072, NA_real_), 
    `TJ, Pat, Richard - Model 11:discrete distance bin:100_200` = if_else(x >= 100 & x <= 200 , 0.062,NA_real_), 
    `TJ, Pat, Richard - Model 11:discrete distance bin:200_300` = if_else(x >=200 & x <= 300 , 0.053,NA_real_), 
    `TJ, Pat, Richard - Model 11:discrete distance bin:300_400` = if_else(x >=300 & x <= 400 , 0.056,NA_real_), 
    `TJ, Pat, Richard - Model 11:discrete distance bin:400_500` = if_else(x >=400 & x <= 500 , 0.062,NA_real_), 
    `TJ, Pat, Richard - Model 12:discrete distance bin:0_250` = if_else(x <= 250, 0.058, NA_real_), 
    `TJ, Pat, Richard - Model 12:discrete distance bin:250_500` = if_else(x >= 250 & x <= 500 ,0.056,NA_real_), 
    `TJ, Pat, Richard - Model 13:discrete distance bin:0_250` = if_else(x <= 250, 0.124, NA_real_), 
    `TJ, Pat, Richard - Model 13:discrete distance bin:250_500` = if_else(x >= 250 & x <= 500 ,0.074,NA_real_), 
    `TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects` = if_else(x <= 500, 0.031, NA_real_), 
    `TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects` = if_else(x <= 500, 0.024, NA_real_), 
    `TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects` = if_else(x <= 500, 0.029, NA_real_), 
    `TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects` = if_else(x <= 500, 0.018, NA_real_), 
    `TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects` = if_else(x <= 500, 0.064, NA_real_), 
    `TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects` = if_else(x <= 500, 0.06, NA_real_), 
    `TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects` = if_else(x <= 500, -0.04, NA_real_), 
    `TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects` = if_else(x <= 500, 0.01, NA_real_), 
    `TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects` = if_else(x <= 500, 0.03, NA_real_) , 
    `Walsh et al 2011a - Model 1:50` = if_else(x <= 50, 0.017 + 0.115, NA_real_), 
    `Walsh et al 2011a - Model 5-_467` = if_else(x >= 50 & x <= 467 ,0.017,NA_real_), 
    `Walsh et al 2011a - Model 1S:50` = if_else(x <= 50, 0.017 + 0.11, NA_real_), 
    `Walsh et al 2011a - Model 1S:50_467` = if_else(x >= 50 & x <= 467 ,0.017,NA_real_) , 
    `Walsh et al 2017 - Anne Arundel 1 Year Average:Log:300` = if_else(x <= 300, 0.126, NA_real_),
    `Walsh et al 2017 - Anne Arundel 1 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 , 0.023, NA_real_),
    `Walsh et al 2017 - Anne Arundel 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 , 0.009,NA_real_),
    `Walsh et al 2017 - Baltimore county 1 Year Average:Log:300` = if_else(x <= 300, 0.090, NA_real_),
    `Walsh et al 2017 - Baltimore county 1 Year Average:Log:500` = if_else(x >= 0 & x <= 500 , -0.009, NA_real_),
    `Walsh et al 2017 - Baltimore county 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,  0.015,NA_real_),
    `Walsh et al 2017 - Calvert 1 Year Average:Log:300` = if_else(x <= 300, 0.033, NA_real_),
    `Walsh et al 2017 - Calvert 1 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 , -0.001, NA_real_),
    `Walsh et al 2017 - Calvert 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,  -0.021,NA_real_),
    `Walsh et al 2017 - Cecil 1 Year Average:Log:300` = if_else(x <= 300, -0.010, NA_real_),
    `Walsh et al 2017 - Cecil 1 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 , 0.001, NA_real_),
    `Walsh et al 2017 - Cecil 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,  -0.003,NA_real_),
    `Walsh et al 2017 - Charles 1 Year Average:Log:300` = if_else(x <= 300, 0.058, NA_real_),
    `Walsh et al 2017 - Charles 1 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 , 0.056, NA_real_),
    `Walsh et al 2017 - Charles 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,  0.107,NA_real_),
    `Walsh et al 2017 - Dorchester 1 Year Average:Log:300` = if_else(x <= 300, 0.078, NA_real_),
    `Walsh et al 2017 - Dorchester 1 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,0.008, NA_real_),
    `Walsh et al 2017 - Dorchester 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,   0.013,NA_real_),
    `Walsh et al 2017 - Harford 1 Year Average:Log:300` = if_else(x <= 300, 0.096, NA_real_),
    `Walsh et al 2017 - Harford 1 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,-0.001, NA_real_),
    `Walsh et al 2017 - Harford 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,   -0.012,NA_real_),
    `Walsh et al 2017 - Kent 1 Year Average:Log:300` = if_else(x <= 300, 0.142, NA_real_),
    `Walsh et al 2017 - Kent 1 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,-0.008, NA_real_),
    `Walsh et al 2017 - Kent 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,   -0.002,NA_real_),
    `Walsh et al 2017 - Prince George's 1 Year Average:Log:300` = if_else(x <= 300, 0.062, NA_real_),
    `Walsh et al 2017 - Prince George's 1 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,0.001, NA_real_),
    `Walsh et al 2017 - Prince George's 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,   -0.022,NA_real_),
    `Walsh et al 2017 - Queen Anne's 1 Year Average:Log:300` = if_else(x <= 300, -0.017, NA_real_),
    `Walsh et al 2017 - Queen Anne's 1 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,0.060, NA_real_),
    `Walsh et al 2017 - Queen Anne's 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,   0.068,NA_real_),
    `Walsh et al 2017 - Somerset 1 Year Average:Log:300` = if_else(x <= 300, 0.091, NA_real_),
    `Walsh et al 2017 - Somerset 1 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,0.055, NA_real_),
    `Walsh et al 2017 - Somerset 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,0.141,NA_real_),
    `Walsh et al 2017 - St Mary's 1 Year Average:Log:300` = if_else(x <= 300, -0.014, NA_real_),
    `Walsh et al 2017 - St Mary's 1 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,0.015, NA_real_),
    `Walsh et al 2017 - St Mary's 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,-0.017,NA_real_),
    `Walsh et al 2017 - Talbot 1 Year Average:Log:300` = if_else(x <= 300, 0.156, NA_real_),
    `Walsh et al 2017 - Talbot 1 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,0.014, NA_real_),
    `Walsh et al 2017 - Talbot 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 , 0.031,NA_real_),
    `Walsh et al 2017 - Wicomico 1 Year Average:Log:300` = if_else(x <= 300, -0.046, NA_real_),
    `Walsh et al 2017 - Wicomico 1 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,0.015, NA_real_),
    `Walsh et al 2017 - Wicomico 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 , 0.010,NA_real_) ,
    `Walsh et al 2017 - Anne Arundel 1 Year Average - lelvel:300` = if_else(x <= 300, 0.0585*1.45/0.759, NA_real_),
    `Walsh et al 2017 - Anne Arundel 1 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,0.0249*1.45/0.759, NA_real_),
    `Walsh et al 2017 - Anne Arundel 1 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 ,0.0089*1.45/0.759,NA_real_) ,
    `Walsh et al 2017 - Baltimore county 1 Year Average - level:300` = if_else(x <= 300, 0.0293*1.45/0.472, NA_real_),
    `Walsh et al 2017 - Baltimore county 1 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,0.0032*1.45/0.472, NA_real_),
    `Walsh et al 2017 - Baltimore county 1 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 ,0.0060*1.45/0.472,NA_real_) ,
    `Walsh et al 2017 - Calvert 1 Year Average - level:300` = if_else(x <= 300, 0.0088*1.45/0.929, NA_real_),
    `Walsh et al 2017 - Calvert 1 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,-0.0174*1.45/0.929, NA_real_),
    `Walsh et al 2017 - Calvert 1 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 ,-0.0196*1.45/0.929,NA_real_) ,
    `Walsh et al 2017 - Cecil 1 Year Average - level:300` = if_else(x <= 300, -0.0024*1.45/0.472, NA_real_),
    `Walsh et al 2017 - Cecil 1 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,-0.0086*1.45/0.472, NA_real_),
    `Walsh et al 2017 - Cecil 1 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 ,-0.0012*1.45/0.472,NA_real_) ,
    `Walsh et al 2017 - Charles 1 Year Average - level:300` = if_else(x <= 300, 0.041*1.45/0.557, NA_real_),
    `Walsh et al 2017 - Charles 1 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,0.0252*1.45/0.557, NA_real_),
    `Walsh et al 2017 - Charles 1 Year Average - level:500-1000` = if_else(x >= 500 & x <= 1000 ,0.0335*1.45/0.557,NA_real_) ,
    `Walsh et al 2017 - Dorchester 1 Year Average - level:300` = if_else(x <= 300, 0.0557*1.45/0.728, NA_real_),
    `Walsh et al 2017 - Dorchester 1 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,0.0076*1.45/0.728, NA_real_),
    `Walsh et al 2017 - Dorchester 1 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 ,0.0079*1.45/0.728,NA_real_) ,
    `Walsh et al 2017 - Harford 1 Year Average - level:300` = if_else(x <= 300, 0.0243*1.45/0.379, NA_real_),
    `Walsh et al 2017 - Harford 1 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,-0.0022*1.45/0.379, NA_real_),
    `Walsh et al 2017 - Harford 1 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 ,0.0022*1.45/0.379,NA_real_) ,
    `Walsh et al 2017 - Kent 1 Year Average - level:300` = if_else(x <= 300, 0.0289*1.45/0.406, NA_real_),
    `Walsh et al 2017 - Kent 1 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,-0.0120*1.45/0.406, NA_real_),
    `Walsh et al 2017 - Kent 1 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 ,-0.0049*1.45/0.406,NA_real_) ,
    `Walsh et al 2017 - Prince George's 1 Year Average - level:300` = if_else(x <= 300, 0.0093*1.45/0.470, NA_real_),
    `Walsh et al 2017 - Prince George's 1 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,0.0018*1.45/0.470, NA_real_),
    `Walsh et al 2017 - Prince George's 1 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 , 0.0023*1.45/0.470,NA_real_) ,
    `Walsh et al 2017 - Queen Anne's 1 Year Average - level:300` = if_else(x <= 300,  0.0151*1.45/0.783, NA_real_),
    `Walsh et al 2017 - Queen Anne's 1 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,0.041*1.45/0.783, NA_real_),
    `Walsh et al 2017 - Queen Anne's 1 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 , 0.0470*1.45/0.783,NA_real_) ,
    `Walsh et al 2017 - Somerset 1 Year Average - level:300` = if_else(x <= 300,  0.0300*1.45/0.683, NA_real_),
    `Walsh et al 2017 - Somerset 1 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,0.0207*1.45/0.683, NA_real_),
    `Walsh et al 2017 - Somerset 1 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 , 0.0498*1.45/0.683,NA_real_) ,
    `Walsh et al 2017 - St Mary's 1 Year Average - level:300` = if_else(x <= 300,  -0.0375*1.45/0.833, NA_real_),
    `Walsh et al 2017 - St Mary's 1 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,0.0082*1.45/0.833, NA_real_),
    `Walsh et al 2017 - St Mary's 1 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 , -0.0115*1.45/0.833,NA_real_) ,
    `Walsh et al 2017 - St Talbot 1 Year Average - level:300` = if_else(x <= 300,  0.0631*1.45/1.02, NA_real_),
    `Walsh et al 2017 - St Talbot 1 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,0.0122*1.45/1.02, NA_real_),
    `Walsh et al 2017 - St Talbot 1 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 , 0.0190*1.45/1.02,NA_real_) ,
    `Walsh et al 2017 - Wicomico 1 Year Average - level:300` = if_else(x <= 300,  0.0018*1.45/0.399, NA_real_),
    `Walsh et al 2017 - Wicomico 1 Year Average - level:0:500` = if_else(x >= 0 & x <= 500 ,0.0130*1.45/0.399, NA_real_),
    `Walsh et al 2017 - Wicomico 1 Year Average - level:500:100` = if_else(x >= 500 & x <= 1000 , 0.0116*1.45/0.399,NA_real_) ,
    `Walsh et al 2017 - Anne Arundel 3 Year Average:Log:300` = if_else(x <= 300, 0.3058, NA_real_),
    `Walsh et al 2017 - Anne Arundel 3 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 , 0.1020, NA_real_),
    `Walsh et al 2017 - Anne Arundel 3 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 , 0.0123,NA_real_),
    `Walsh et al 2017 - Baltimore county 3 Year Average:Log:300` = if_else(x <= 300, 0.05560, NA_real_),
    `Walsh et al 2017 - Baltimore county 3 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 , -0.0386, NA_real_),
    `Walsh et al 2017 - Baltimore county 3 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,  0.0077,NA_real_),
    `Walsh et al 2017 - Calvert 3 Year Average:Log:300` = if_else(x <= 300, -0.0134, NA_real_),
    `Walsh et al 2017 - Calvert 3 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 , -0.0779, NA_real_),
    `Walsh et al 2017 - Calvert 3 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,  -0.0653,NA_real_),
    `Walsh et al 2017 - Cecil 3 Year Average:Log:300` = if_else(x <= 300, 0.0010, NA_real_),
    `Walsh et al 2017 - Cecil 3 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 , -0.1257, NA_real_),
    `Walsh et al 2017 - Cecil 3 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,  -0.0362,NA_real_),
    `Walsh et al 2017 - Charles 3 Year Average:Log:300` = if_else(x <= 300, 0.6413, NA_real_),
    `Walsh et al 2017 - Charles 3 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 , 0.1764, NA_real_),
    `Walsh et al 2017 - Charles 3 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,  0.3021,NA_real_),
    `Walsh et al 2017 - Dorchester 3 Year Average:Log:300` = if_else(x <= 300, 0.0607, NA_real_),
    `Walsh et al 2017 - Dorchester 3 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,0.0429, NA_real_),
    `Walsh et al 2017 - Dorchester 3 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 , 0.0053,NA_real_),
    `Walsh et al 2017 - Harford 3 Year Average:Log:300` = if_else(x <= 300, 0.2600, NA_real_),
    `Walsh et al 2017 - Harford 3 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,-0.0213, NA_real_),
    `Walsh et al 2017 - Harford 3 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,   -0.0370,NA_real_),
    `Walsh et al 2017 - Kent 3 Year Average:Log:300` = if_else(x <= 300, 0.0745, NA_real_),
    `Walsh et al 2017 - Kent 3 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,-0.1147, NA_real_),
    `Walsh et al 2017 - Kent 3 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,   -0.1083,NA_real_),
    `Walsh et al 2017 - Prince George's 3 Year Average:Log:300` = if_else(x <= 300, -0.0090, NA_real_),
    `Walsh et al 2017 - Prince George's 3 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,0.1411, NA_real_),
    `Walsh et al 2017 - Prince George's 3 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,   -0.1427,NA_real_),
    `Walsh et al 2017 - Queen Anne's 1 Year Average:Log:300` = if_else(x <= 300, 0.1310, NA_real_),
    `Walsh et al 2017 - Queen Anne's 1 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,0.1838, NA_real_),
    `Walsh et al 2017 - Queen Anne's 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,0.1983,NA_real_),
    `Walsh et al 2017 - Somerset 3 Year Average:Log:300` = if_else(x <= 300, 0.0839, NA_real_),
    `Walsh et al 2017 - Somerset 3 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,0.0632, NA_real_),
    `Walsh et al 2017 - Somerset 3 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 ,0.1635,NA_real_),
    `Walsh et al 2017 - St Mary's 3 Year Average:Log:300` = if_else(x <= 300, -0.1265, NA_real_),
    `Walsh et al 2017 - St Mary's 3 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,-0.0855, NA_real_),
    `Walsh et al 2017 - St Mary's 3 Year Average:Log:500-1000` = if_else(x >= 500 & x <= 1000 ,-0.1324,NA_real_),
    `Walsh et al 2017 - Talbot 3 Year Average:Log:300` = if_else(x <= 300, 0.0793, NA_real_),
    `Walsh et al 2017 - Talbot 3 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,-0.1082, NA_real_),
    `Walsh et al 2017 - Talbot 3 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 , -0.0984,NA_real_),
    `Walsh et al 2017 - Wicomico 1 Year Average:Log:300` = if_else(x <= 300, 0.0751, NA_real_),
    `Walsh et al 2017 - Wicomico 1 Year Average:Log:0_500` = if_else(x >= 0 & x <= 500 ,0.0869, NA_real_),
    `Walsh et al 2017 - Wicomico 1 Year Average:Log:500_1000` = if_else(x >= 500 & x <= 1000 , 0.0878,NA_real_) ,
    `Walsh et al 2017 - Anne Arundel 3 Year Average - level:300` = if_else(x <= 300, 0.1660*1.45/0.759, NA_real_),
    `Walsh et al 2017 - Anne Arundel 3 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,0.0586*1.45/0.759, NA_real_),
    `Walsh et al 2017 - Anne Arundel 3 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 ,0.0103*1.45/0.759,NA_real_) ,
    `Walsh et al 2017 - Baltimore county 3 Year Average - level:300` = if_else(x <= 300, 0.0191*1.45/0.472, NA_real_),
    `Walsh et al 2017 - Baltimore county 3 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,-0.0117*1.45/0.472, NA_real_),
    `Walsh et al 2017 - Baltimore county 3 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 ,0.0015*1.45/0.472,NA_real_) ,
    `Walsh et al 2017 - Calvert 3 Year Average - level:300` = if_else(x <= 300, 0.0133*1.45/0.929, NA_real_),
    `Walsh et al 2017 - Calvert 3 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,-0.0247*1.45/0.929, NA_real_),
    `Walsh et al 2017 - Calvert 3 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 ,-0.0237*1.45/0.929,NA_real_) ,
    `Walsh et al 2017 - Cecil 3 Year Average - level:300` = if_else(x <= 300, 0.0023*1.45/0.472, NA_real_),
    `Walsh et al 2017 - Cecil 3 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,-0.0329*1.45/0.472, NA_real_),
    `Walsh et al 2017 - Cecil 3 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 ,-0.0128*1.45/0.472,NA_real_) ,
    `Walsh et al 2017 - Charles 3 Year Average - level:300` = if_else(x <= 300, 0.2421*1.45/0.557, NA_real_),
    `Walsh et al 2017 - Charles 3 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 , 0.0670*1.45/0.557, NA_real_),
    `Walsh et al 2017 - Charles 3 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 ,0.1037*1.45/0.557,NA_real_) ,
    `Walsh et al 2017 - Dorchester 3 Year Average - level:300` = if_else(x <= 300, 0.0309*1.45/0.728, NA_real_),
    `Walsh et al 2017 - Dorchester 3 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,-0.0284*1.45/0.728, NA_real_),
    `Walsh et al 2017 - Dorchester 3 Year Average - level:500-1000` = if_else(x >= 500 & x <= 1000 ,-0.0040*1.45/0.728,NA_real_) ,
    `Walsh et al 2017 - Harford 3 Year Average - level:300` = if_else(x <= 300, 0.0760*1.45/0.379, NA_real_),
    `Walsh et al 2017 - Harford 3 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,-0.0066*1.45/0.379, NA_real_),
    `Walsh et al 2017 - Harford 3 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 ,-0.0109*1.45/0.379,NA_real_) ,
    `Walsh et al 2017 - Kent 3 Year Average - level:300` = if_else(x <= 300, 0.0277*1.45/0.406, NA_real_),
    `Walsh et al 2017 - Kent 3 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,-0.0349*1.45/0.406, NA_real_),
    `Walsh et al 2017 - Kent 3 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 ,-0.0306*1.45/0.406,NA_real_) ,
    `Walsh et al 2017 - Prince George's 3 Year Average - level:300` = if_else(x <= 300, -0.0227*1.45/0.470, NA_real_),
    `Walsh et al 2017 - Prince George's 3 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,0.0399*1.45/0.470, NA_real_),
    `Walsh et al 2017 - Prince George's 3 Year Average - level:500-1000` = if_else(x >= 500 & x <= 1000 , 0.0439*1.45/0.470,NA_real_) ,
    `Walsh et al 2017 - Queen Anne's 3 Year Average - level:300` = if_else(x <= 300,  0.0402*1.45/0.783, NA_real_),
    `Walsh et al 2017 - Queen Anne's 3 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 , 0.0633*1.45/0.783, NA_real_),
    `Walsh et al 2017 - Queen Anne's 3 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 , 0.0664*1.45/0.783,NA_real_) ,
    `Walsh et al 2017 - Somerset 3 Year Average - level:300` = if_else(x <= 300,  0.0547*1.45/0.683, NA_real_),
    `Walsh et al 2017 - Somerset 3 Year Average - level:0-500` = if_else(x >= 0 & x <= 500 ,0.0499*1.45/0.683, NA_real_),
    `Walsh et al 2017 - Somerset 3 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 , 0.0761*1.45/0.683,NA_real_) ,
    `Walsh et al 2017 - St Mary's 3 Year Average - level:300` = if_else(x <= 300,  -0.0839*1.45/0.833, NA_real_),
    `Walsh et al 2017 - St Mary's 3 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,-0.0476*1.45/0.833, NA_real_),
    `Walsh et al 2017 - St Mary's 3 Year Average - level:0_500` = if_else(x >= 500 & x <= 1000 , -0.0665*1.45/0.833,NA_real_) ,
    `Walsh et al 2017 - St Talbot 3 Year Average - level:300` = if_else(x <= 300,  0.0473*1.45/1.02, NA_real_),
    `Walsh et al 2017 - St Talbot 3 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,-0.0149*1.45/1.02, NA_real_),
    `Walsh et al 2017 - St Talbot 3 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 , 0.0226*1.45/1.02,NA_real_) ,
    `Walsh et al 2017 - Wicomico 3 Year Average - level:300` = if_else(x <= 300,  0.0053*1.45/0.399, NA_real_),
    `Walsh et al 2017 - Wicomico 3 Year Average - level:0_500` = if_else(x >= 0 & x <= 500 ,0.0187*1.45/0.399, NA_real_),
    `Walsh et al 2017 - Wicomico 3 Year Average - level:500_1000` = if_else(x >= 500 & x <= 1000 , 0.0190*1.45/0.399,NA_real_),
    `Wolf et al 2022 - Model 1` = if_else(x <= 500, 2704.7*0.00030,NA_real_), 
    `Wolf et al 2022 - Model 2` = if_else(x <= 500, 1750.7*0.00050,NA_real_), 
    `Wolf et al 2022 - Model 3` = if_else(x <= 500, 782.4*0.00190,NA_real_),
    `Wolf et al 2022 - Model 4` = if_else(x <= 500, 3196.1*0.00060,NA_real_), 
    `Wolf et al 2022 - Model 5` = if_else(x <= 500, 2479.9*0.00060,NA_real_), 
    `Wolf et al 2022 - Model 6` = if_else(x <= 500, 1301.7*0.00280,NA_real_), 
    `Wolf et al 2022 - Model 7` = if_else(x <= 500, 1424.9*-0.00010,NA_real_), 
    `Wolf et al 2022 - Model 8` = if_else(x <= 500, 410.4*-0.00500,NA_real_), 
    `Wolf et al 2022 - Model 9` = if_else(x <= 500, 312.7*0.00240,NA_real_), 
    `Wolf et al 2022 - Model 10` = if_else(x <= 500, 1071.3*0.00160,NA_real_), 
    `Wolf et al 2022 - Model 11` = if_else(x <= 500, 2006.9*0.00020,NA_real_), 
    `Wolf et al 2022 - Model 12` = if_else(x <= 500, 4367.5*0.00000,NA_real_), 
    `Wolf et al 2022 - Model 13` = if_else(x <= 500, 1220.4*0.00370,NA_real_), 
    `Wolf et al 2022 - Model 14` = if_else(x <= 500, 282.8*0.00440,NA_real_), 
    `Wolf et al 2022 - Model 15` = if_else(x <= 500, 2357.8*0.00020,NA_real_), 
    `Wolf et al 2022 - Model 16` = if_else(x <= 500, 1115.7*0.00100,NA_real_), 
    `Wolf et al 2022 - Model 17` = if_else(x <= 500, 3038.6*0.00020,NA_real_), 
    `Wolf et al 2022 - Model 18` = if_else(x <= 500, 749.6*-0.00010,NA_real_), 
    `Wolf et al 2022 - Model 19` = if_else(x <= 500, 976.9*-0.00210,NA_real_), 
    `Wolf et al 2022 - Model 20` = if_else(x <= 500, 1376.1*0.00100,NA_real_), 
    `Wolf et al 2022 - Model 21` = if_else(x <= 500, 259.8*0.00820,NA_real_), 
    `Wolf et al 2022 - Model 22` = if_else(x <= 500, 38.8*0.09360,NA_real_), 
    `Wolf et al 2022 - Model 23` = if_else(x <= 500, 90.4*0.02720,NA_real_), 
    `Wolf et al 2022 - Model 24` = if_else(x <= 500, 1277.8*0.00000,NA_real_), 
    `Wolf et al 2022 - Model 25` = if_else(x <= 500, 1327.7*-0.00020,NA_real_), 
    `Wolf et al 2022 - Model 26` = if_else(x <= 500, 688.5*0.00090,NA_real_), 
    `Wolf et al 2022 - Model 27` = if_else(x <= 500, 688.5*0.00480,NA_real_), 
    `Wolf et al 2022 - Model 1` = if_else(x <= 500, 2704.7*0.00030,NA_real_), 
    `Wolf et al 2022 - Model 2` = if_else(x <= 500, 1750.7*0.00060,NA_real_), 
    `Wolf et al 2022 - Model 3` = if_else(x <= 500, 782.4*0.00180,NA_real_), 
    `Wolf et al 2022 - Model 4` = if_else(x <= 500, 3196.1*0.00070,NA_real_), 
    `Wolf et al 2022 - Model 5` = if_else(x <= 500, 2479.9*0.00070,NA_real_), 
    `Wolf et al 2022 - Model 6` = if_else(x <= 500, 1301.7*0.00290,NA_real_), 
    `Wolf et al 2022 - Model 7` = if_else(x <= 500, 1424.9*-0.00020,NA_real_), 
    `Wolf et al 2022 - Model 8` = if_else(x <= 500, 410.4*-0.00570,NA_real_), 
    `Wolf et al 2022 - Model 9` = if_else(x <= 500, 312.7*0.00200,NA_real_), 
    `Wolf et al 2022 - Model 10` = if_else(x <= 500, 1071.3*0.00100,NA_real_), 
    `Wolf et al 2022 - Model 11` = if_else(x <= 500, 2006.9*0.00040,NA_real_),
    `Wolf et al 2022 - Model 12` = if_else(x <= 500, 4367.5*0.00000,NA_real_), 
    `Wolf et al 2022 - Model 13` = if_else(x <= 500, 1220.4*0.00370,NA_real_), 
    `Wolf et al 2022 - Model 14` = if_else(x <= 500, 282.8*0.00520,NA_real_), 
    `Wolf et al 2022 - Model 15` = if_else(x <= 500, 2357.8*0.00030,NA_real_), 
    `Wolf et al 2022 - Model 16` = if_else(x <= 500, 1115.7*0.00140,NA_real_), 
    `Wolf et al 2022 - Model 17` = if_else(x <= 500, 3038.6*0.00030,NA_real_), 
    `Wolf et al 2022 - Model 18` = if_else(x <= 500, 749.6*0.00050,NA_real_), 
    `Wolf et al 2022 - Model 19` = if_else(x <= 500, 976.9*0.00210,NA_real_), 
    `Wolf et al 2022 - Model 20` = if_else(x <= 500, 1376.1*0.00090,NA_real_), 
    `Wolf et al 2022 - Model 21` = if_else(x <= 500, 259.8*0.00940,NA_real_), 
    `Wolf et al 2022 - Model 22` = if_else(x <= 500, 38.8*0.09800,NA_real_),
    `Wolf et al 2022 - Model 23` = if_else(x <= 500, 90.4*0.02600,NA_real_), 
    `Wolf et al 2022 - Model 24` = if_else(x <= 500, 1277.8*0.00010,NA_real_), 
    `Wolf et al 2022 - Model 25` = if_else(x <= 500, 1327.7*-0.00010,NA_real_), 
    `Wolf et al 2022 - Model 26` = if_else(x <= 500, 688.5*0.00090,NA_real_), 
    `Wolf et al 2022 - Model 27` = if_else(x <= 500, 688.5*0.00350,NA_real_),
    `Wolf et al 2022 - Model 1` = if_else(x <= 500, 2704.7*0.00040,NA_real_), 
    `Wolf et al 2022 - Model 2` = if_else(x <= 500, 1750.7*0.00030,NA_real_), 
    `Wolf et al 2022 - Model 3` = if_else(x <= 500, 782.4*0.00120,NA_real_), 
    `Wolf et al 2022 - Model 4` = if_else(x <= 500, 3196.1*0.00040,NA_real_), 
    `Wolf et al 2022 - Model 5` = if_else(x <= 500, 2479.9*0.00070,NA_real_), 
    `Wolf et al 2022 - Model 6` = if_else(x <= 500, 1301.7*0.00540,NA_real_), 
    `Wolf et al 2022 - Model 7` = if_else(x <= 500, 1424.9*-0.00010,NA_real_), 
    `Wolf et al 2022 - Model 8` = if_else(x <= 500, 410.4*-0.01180,NA_real_), 
    `Wolf et al 2022 - Model 9` = if_else(x <= 500, 312.7*0.00040,NA_real_), 
    `Wolf et al 2022 - Model 10` = if_else(x <= 500, 1071.3*-0.00010,NA_real_), 
    `Wolf et al 2022 - Model 11` = if_else(x <= 500, 2006.9*0.00040,NA_real_), 
    `Wolf et al 2022 - Model 12` = if_else(x <= 500, 4367.5*-0.00010,NA_real_), 
    `Wolf et al 2022 - Model 13` = if_else(x <= 500, 1220.4*0.00510,NA_real_), 
    `Wolf et al 2022 - Model 14` = if_else(x <= 500, 282.8*0.00340,NA_real_), 
    `Wolf et al 2022 - Model 15` = if_else(x <= 500, 2357.8*0.00010,NA_real_), 
    `Wolf et al 2022 - Model 16` = if_else(x <= 500, 1115.7*0.00180,NA_real_), 
    `Wolf et al 2022 - Model 17` = if_else(x <= 500, 3038.6*0.00070,NA_real_), 
    `Wolf et al 2022 - Model 18` = if_else(x <= 500, 749.6*-0.00010,NA_real_), 
    `Wolf et al 2022 - Model 19` = if_else(x <= 500, 976.9*0.00210,NA_real_), 
    `Wolf et al 2022 - Model 20` = if_else(x <= 500, 1376.1*0.00090,NA_real_), 
    `Wolf et al 2022 - Model 21` = if_else(x <= 500, 259.8*0.00850,NA_real_), 
    `Wolf et al 2022 - Model 22` = if_else(x <= 500, 38.8*0.08330,NA_real_), 
    `Wolf et al 2022 - Model 23` = if_else(x <= 500, 90.4*0.03240,NA_real_), 
    `Wolf et al 2022 - Model 24` = if_else(x <= 500, 1277.8*0.00000,NA_real_), 
    `Wolf et al 2022 - Model 25` = if_else(x <= 500, 1327.7*-0.00010,NA_real_), 
    `Wolf et al 2022 - Model 26` = if_else(x <= 500, 688.5*0.00150,NA_real_),
    `Wolf et al 2022 - Model 27` = if_else(x <= 500, 688.5*0.00590,NA_real_), 
    `Wolf et al 2022 - Model 1` = if_else(x <= 500, 2704.7*0.00010,NA_real_), 
    `Wolf et al 2022 - Model 2` = if_else(x <= 500, 1750.7*0.00060,NA_real_), 
    `Wolf et al 2022 - Model 3` = if_else(x <= 500, 782.4*0.00190,NA_real_), 
    `Wolf et al 2022 - Model 4` = if_else(x <= 500, 3196.1*0.00050,NA_real_), 
    `Wolf et al 2022 - Model 5` = if_else(x <= 500, 2479.9*0.00060,NA_real_), 
    `Wolf et al 2022 - Model 6` = if_else(x <= 500, 1301.7*0.00280,NA_real_), 
    `Wolf et al 2022 - Model 7` = if_else(x <= 500, 1424.9*0.00000,NA_real_), 
    `Wolf et al 2022 - Model 8` = if_else(x <= 500, 410.4*-0.00630,NA_real_), 
    `Wolf et al 2022 - Model 9` = if_else(x <= 500, 312.7*0.00180,NA_real_), 
    `Wolf et al 2022 - Model 10` = if_else(x <= 500, 1071.3*0.00160,NA_real_), 
    `Wolf et al 2022 - Model 11` = if_else(x <= 500, 2006.9*0.00030,NA_real_), 
    `Wolf et al 2022 - Model 12` = if_else(x <= 500, 4367.5*-0.00010,NA_real_), 
    `Wolf et al 2022 - Model 13` = if_else(x <= 500, 1220.4*0.00030,NA_real_), 
    `Wolf et al 2022 - Model 14` = if_else(x <= 500, 282.8*0.00470,NA_real_), 
    `Wolf et al 2022 - Model 15` = if_else(x <= 500, 2357.8*0.00010,NA_real_), 
    `Wolf et al 2022 - Model 16` = if_else(x <= 500, 1115.7*0.00120,NA_real_), 
    `Wolf et al 2022 - Model 17` = if_else(x <= 500, 3038.6*0.00010,NA_real_), 
    `Wolf et al 2022 - Model 18` = if_else(x <= 500, 749.6*0.00000,NA_real_),
    `Wolf et al 2022 - Model 19` = if_else(x <= 500, 976.9*0.00210,NA_real_), 
    `Wolf et al 2022 - Model 20` = if_else(x <= 500, 1376.1*0.00090,NA_real_), 
    `Wolf et al 2022 - Model 21` = if_else(x <= 500, 259.8*0.00850,NA_real_), 
    `Wolf et al 2022 - Model 22` = if_else(x <= 500, 38.8*0.08330,NA_real_), 
    `Wolf et al 2022 - Model 23` = if_else(x <= 500, 90.4*0.03240,NA_real_), 
    `Wolf et al 2022 - Model 24` = if_else(x <= 500, 1277.8*0.00000,NA_real_), 
    `Wolf et al 2022 - Model 25` = if_else(x <= 500, 1327.7*-0.00060,NA_real_), 
    `Wolf et al 2022 - Model 26` = if_else(x <= 500, 688.5*0.00050,NA_real_), 
    `Wolf et al 2022 - Model 27` = if_else(x <= 500, 688.5*0.00420,NA_real_), 
  )


# Assuming 'results' dataframe is already created as shown in the previous example
results_Distdum <- results %>%
  pivot_longer(
    cols = -x,  # Select all columns except 'x'
    names_to = "Function",  # New column name for the variable/function names
    values_to = "y_value"  # New column name for the y values
  )%>%
  mutate(distclass="Buf")



# Plot using geom_point
ggplot(results_Distdum, aes(x = x, y = y_value)) +
  geom_point(position = position_jitter(width = 0.2), size = 3, alpha = 0.7) + # Add jitter for visibility
  labs(title = "Dot Plot of Extracted Y Values",
       x = "Groups",
       y = "Y Values") +
  theme_minimal() +
  theme(legend.title = element_blank())


df <- rbind(results_nodist,results_Distdum,results_linear,results_linearAndDistdum)%>%
  rename(id=Function)



# Extract and mutate columns, including handling 'et al.'
df <- df %>%
  mutate(
    author_year = str_extract(id, "[A-Za-z]+( et al\\.?| & [A-Za-z]+)? \\d{4}"),  
    author = str_extract(id, "[A-Za-z]+( et al\\.?| & [A-Za-z]+)?"),           
    year = str_extract(id, "\\d{4}"),                                       
    study_name = paste(author, year)                                             
  )%>%
  select(x,id,y_value,distclass,study_name)%>%
  mutate(study_name = if_else(study_name == "TJ NA", "TJ, Pat, Richard", study_name))%>%
  mutate(study_name = if_else(study_name == "Boyle 2001", "Boyle and Taylor 2001", study_name))%>%
  mutate(study_name = if_else(study_name == "Calder 2019", "Calder & Arrieta 2019", study_name))%>%
  mutate(study_name = if_else(study_name == "Swedberg 2020", "Swedberg el al 2020", study_name))%>%
  mutate(study_name = if_else(study_name == "Walsh et al 2011", "Walsh et al 2011a", study_name))


#load metadata csv
df1 <- read.csv("./metadata/Meta_dataset_water_clarity_TJ.csv")%>%
  select(studyname,geog,sampsize,specname,pubtype,wbtype,funcform,wbsize,avgwqvar,west, midwest, northeast, south, multireg,canada,state, region,pubtype)%>%
  rename("model.name" = "specname",
         "study_name" = "studyname")%>%
  filter(study_name != "Steinnes 1992")

df1$id <- paste(df1$study_name, "-", df1$model.name)




# Function to match names
find_best_match <- function(name, reference_names) {
  distances <- stringdist::stringdist(name, reference_names, method = "jw")  # Jaro-Winkler method
  best_match <- reference_names[which.min(distances)]  # Find the best match
  return(best_match)
}



# Apply the function to df2 to get standardized names
df <- df %>%
  mutate(id = sapply(id, find_best_match, reference_names = df1$id))

t <- df%>%
  distinct(id)

t1 <- df1%>%
  distinct(id, .keep_all = T)

# Find rows in df2 that are not in df1
mismatch_in_t1 <- anti_join(t1, t, by = c("id"))


df <- df%>%
  left_join(t1)


df <- df[df$y_value >= -2.58 & df$y_value <= 2.208, ]



write_csv(df, "./metadata/Meta_data_at_50m_distcal_elast.csv")




