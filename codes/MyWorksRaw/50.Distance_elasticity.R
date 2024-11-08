
# clear memory
rm(list = ls())

# library

library(tidyverse)


# Withould distance involvement 



base <-
  ggplot() +
  xlim(0, 2000)


base + geom_function(fun =~ if_else(.x < 300, 0.009,NA)) + # Ara 2007 - IH Cluster 5 - OLS
  geom_function(fun =~ if_else(.x < 300, 0.172,NA)) + # Ara 2007 - IH Cluster 6 - OLS
  geom_function(fun =~ if_else(.x < 300, -0.033,NA)) + # Ara 2007 - IH Cluster 7 - OLS
  geom_function(fun =~ if_else(.x < 300, -0.033,NA)) + # Ara 2007 - IH Cluster 11 - OLS
  geom_function(fun =~ if_else(.x < 300, 0.012,NA)) + # Ara 2007 - IH Cluster 5 - GMM
  geom_function(fun =~ if_else(.x < 300, 0.163,NA)) + # Ara 2007 - IH Cluster 6 - GMM
  geom_function(fun =~ if_else(.x < 300, -0.007,NA)) + # Ara 2007 - IH Cluster 7 - GMM
  geom_function(fun =~ if_else(.x < 300, -0.029,NA)) + # Ara 2007 - IH Cluster 11 - GMM
  geom_function(fun =~ if_else(.x < 300, 0.204,NA)) + # Ara 2007 - CBG Cluster 2 - OLS
  geom_function(fun =~ if_else(.x < 300, 0.029,NA)) + # Ara 2007 - CBG Cluster 3 - OLS
  geom_function(fun =~ if_else(.x < 300, 0.029,NA)) + # Ara 2007 - CBG Cluster 3 - OLS
  geom_function(fun =~ if_else(.x < 300, 0.12,NA)) + # Ara 2007 - CBG Cluster 4 - OLS
  geom_function(fun =~ if_else(.x < 300, 0.037,NA)) + # Ara 2007 - CBG Cluster 7 - OLS
  geom_function(fun =~ if_else(.x < 300, 0.012,NA)) + # Ara 2007 - CBG Cluster 9 - OLS
  geom_function(fun =~ if_else(.x < 300, 0.203,NA)) + # Ara 2007 - CBG Cluster 2 - GMM
  geom_function(fun =~ if_else(.x < 300, 0.026,NA)) + # Ara 2007 - CBG Cluster 3 - GMM
  geom_function(fun =~ if_else(.x < 300, 0.056,NA)) + # Ara 2007 - CBG Cluster 4 - GMM
  geom_function(fun =~ if_else(.x < 300, 0.052,NA)) + # Ara 2007 - CBG Cluster 7 - GMM
  geom_function(fun =~ if_else(.x < 300, 0.016,NA)) + # Ara 2007 - CBG Cluster 9 - GMM
  geom_function(fun =~ if_else(.x < 300, 4.369999886*3515/102746,NA))+ # Boyle and Taylor 2001 - G1-Town Data
  geom_function(fun =~ if_else(.x < 300, 3.920000076*3515/102746,NA))+ # Boyle and Taylor 2001 - G1-Survey Data
  geom_function(fun =~ if_else(.x < 300, 2.099999905*3515/85197,NA))+ # Boyle and Taylor 2001 - G2-Town Data
  geom_function(fun =~ if_else(.x < 300, 1.389999986*3515/85197,NA))+ # Boyle and Taylor 2001 - G2-Survey Data
  geom_function(fun =~ if_else(.x < 300, 4.730000019*3515/32779,NA))+ # Boyle and Taylor 2001 - G3-Town Data
  geom_function(fun =~ if_else(.x < 300, 6.130000114*3515/32779,NA))+ # Boyle and Taylor 2001 - G3-Survey Data
  geom_function(fun =~ if_else(.x < 300, 40.02999878*3515/97482,NA))+ # Boyle and Taylor 2001 - G4-Town Data
  geom_function(fun =~ if_else(.x < 300, 36.13999939*3515/97482,NA))+ # Boyle and Taylor 2001 - G4-Survey Data
  geom_function(fun =~ if_else(.x < 300, 3514/104069*7.375697136,NA)) + # Boyle et al 1999 -model 1
  geom_function(fun =~ if_else(.x < 300, 3514/85880*3.167430162,NA)) + # Boyle et al 1999 - model 2
  geom_function(fun =~ if_else(.x < 300, 3514/73938*3.584641457,NA)) + # Boyle et al 1999 - model 3
  geom_function(fun =~ if_else(.x < 300, 3514/100350*13.07054615,NA))+  # Boyle et al 1999 - model 4
  geom_function(fun =~ if_else(.x < 300, 178.3*3.83/2239.2,NA))+  # Calder¢n-Arrieta 2019 - model 1
  geom_function(fun =~ if_else(.x < 300, 185.9*3.83/2239.2,NA))+  # Calder¢n-Arrieta 2019 - model 2
  geom_function(fun =~ if_else(.x < 300, 43520.43*3.851/648415.8,NA))+  # Clapper & Caudill 2014 - liner - sale price
  geom_function(fun =~ if_else(.x < 300, 0.065*3.851,NA))+  # Clapper & Caudill 2014 - log-liner - sale price
  geom_function(fun =~ if_else(.x < 300, 0.269,NA))+  # Clapper & Caudill 2014 - log-log - sale price
  geom_function(fun =~ if_else(.x < 300, 30.988*3.851/443.566,NA))+  # Clapper & Caudill 2014 - liner - sale price per square foot
  geom_function(fun =~ if_else(.x < 300, 0.064*3.851,NA))+  # Clapper & Caudill 2014 - log-liner - sale price per square foot
  geom_function(fun =~ if_else(.x < 300, 0.268,NA))+  # Clapper & Caudill 2014 - log-log - sale price per square foot
  geom_function(fun =~ if_else(.x < 300, 4.48059988*1235.77002/138763.05,NA))+  # Gibbs et al 2002 - Conway/Milton
  geom_function(fun =~ if_else(.x < 300, 17.34000015*1879.099976/175157.73,NA))+  # Gibbs et al 2002 - Winnipesaukee
  geom_function(fun =~ if_else(.x < 300, 76.76999664*213.5800018/132162.84,NA))+  # Gibbs et al 2002 - Derry/Amherst
  geom_function(fun =~ if_else(.x < 300, 149.6000061*283.6900024/167104.7,NA))+  # Gibbs et al 2002 - Derry/Amherst
  geom_function(fun =~ if_else(.x < 300, 13893.84961*3.28*0.926591992/268034.57,NA))+  # Horsch & Lewis 2009 - Model 1
  geom_function(fun =~ if_else(.x < 300, 14355.2998*3.28*0.926591992/268034.57,NA))+  # Horsch & Lewis 2009 - Model 2
  geom_function(fun =~ if_else(.x < 300, 13367.92969*3.28*0.926591992/268034.57,NA))+  # Horsch & Lewis 2009 - Model 3
  geom_function(fun =~ if_else(.x < 300, 7072.709961*3.28*0.926591992/268034.57,NA))+  # Horsch & Lewis 2009 - Random Effects with Year Dummies
  geom_function(fun =~ if_else(.x < 300, 6443.779785*3.28*0.926591992/268034.57,NA))+  # Horsch & Lewis 2009 - Random Effects with Year Trend Variable
  geom_function(fun =~ if_else(.x < 300, 8.75*866/65363.49,NA))+  # Hsu 2000 - Northeast Kingdom
  geom_function(fun =~ if_else(.x < 300, 12.84000015*2021/120063.58,NA))+  # Hsu 2000 - Lake Champlain
  geom_function(fun =~ if_else(.x < 300, 10.48999977*1552/104471.03,NA))+  # Hsu 2000 - Rutland County
  geom_function(fun =~ if_else(.x < 300, 10.60999966*1581/105410.9,NA))+  # Hsu 2000 - Milfoil
  geom_function(fun =~ if_else(.x < 300, 13.32999992*1581/105410.9,NA))+  # Hsu 2000 - exp(milfoil)
  geom_function(fun =~ if_else(.x < 300, 44.31000137*1581/105410.9,NA))+  # Hsu 2000 - Total Weed
  geom_function(fun =~ if_else(.x < 300, 22.92000008*1581/105410.9,NA))+  # Hsu 2000 - exp(Total Weed)
  geom_function(fun =~ if_else(.x < 2000, 0.059,NA))+  # Irwin & Wolf 2022 - Model 1
  geom_function(fun =~ if_else(.x < 2000, 0.093,NA))+  # Irwin & Wolf 2022 - Model 2
  geom_function(fun =~ if_else(.x < 2000, 0.103,NA))+  # Irwin & Wolf 2022 - Model 3
  geom_function(fun =~ if_else(.x < 2000, 0.086,NA))+  # Irwin & Wolf 2022 - Model 4
  geom_function(fun =~ if_else(.x < 2000, 0.079,NA))+  # Irwin & Wolf 2022 - Model 5
  geom_function(fun =~ if_else(.x < 300, 5207*3.28*3.627120018/184892,NA))+  # Kashian et al 2006 - Hedonic Equation (3)
  geom_function(fun =~ if_else(.x < 300, 46459.33/(218562*3.28),NA))+  # Kemp et al 2017 - Model 1 (SD)
  geom_function(fun =~ if_else(.x < 300, 45292.89/(238142*3.28),NA))+  # Kemp et al 2017 - Model 2 (SD)
  geom_function(fun =~ if_else(.x < 300, 66046.22/(175331*3.28),NA))+  # Kemp et al 2017 - Model 3 (SD)
  geom_function(fun =~ if_else(.x < 300, 13.22999954*702.6099854/100313,NA))+  # Krysel et al 2003  - Aitkin Lake Group
  geom_function(fun =~ if_else(.x < 300, 4.71999979*985.7600098/176461,NA))+  # Krysel et al 2003 - Brainerd Lake Group
  geom_function(fun =~ if_else(.x < 300, 1.100000024*852.2600098/135905,NA))+  # Krysel et al 2003 - Grand Rapids Lake Group
  geom_function(fun =~ if_else(.x < 300, 2.150000095*1031.869995/179621,NA))+  # Krysel et al 2003 - Walker Lake Group
  geom_function(fun =~ if_else(.x < 300, 21.75*882.9099731/124390,NA))+  # Krysel et al 2003 - Park Rapids Lake Group
  geom_function(fun =~ if_else(.x < 300, 7.309999943*702.6099854/100313,NA))+  # Krysel et al 2003 - MN - Aitkin Lake Group
  geom_function(fun =~ if_else(.x < 300, 1.940000057*985.7600098/176461,NA))+  # Krysel et al 2003 - MN - Brainerd Lake Group
  geom_function(fun =~ if_else(.x < 300, 1.730000019*852.2600098/135905,NA))+  # Krysel et al 2003 - MN - Grand Rapids Lake Group
  geom_function(fun =~ if_else(.x < 300, 1.909999967*1031.869995/179621,NA))+  # Krysel et al 2003 - MN - Walker Lake Group
  geom_function(fun =~ if_else(.x < 300, 19.95000076*882.9099731/124390,NA))+  # Krysel et al 2003 - MN - Park Rapids Lake Group
  geom_function(fun =~ if_else(.x < 300, 9.720000267*1101.5/142829,NA))+  # Krysel et al 2003 - MN - Bemidji Lake Group
  geom_function(fun =~ if_else(.x < 300, 0.275000006,NA))+  # Liao et al 2016 - Model 1
  geom_function(fun =~ if_else(.x < 300, 0.231999993,NA))+  # Liao et al 2016 - Model 3
  geom_function(fun =~ if_else(.x < 300, 0.219999999,NA))+  # Liao et al 2016 - Spatial Regime Model 1
  geom_function(fun =~ if_else(.x < 300, 0.165000007,NA))+  # Liao et al 2016 - Spatial Regime Model 1
  geom_function(fun =~ if_else(.x < 300, 0.223000005,NA))+  # Liao et al 2016 - Spatial Regime Model 3
  geom_function(fun =~ if_else(.x < 300, 0.165999994,NA))+  # Liao et al 2016 - Spatial Regime Model 3
  geom_function(fun =~ if_else(.x < 2000, 0.30340001*0.709999979,NA))+  # Liu et al 2014 - Sale Price - semi-log
  geom_function(fun =~ if_else(.x < 2000, -0.0145, NA)) +  # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:1
  geom_function(fun =~ if_else(.x < 2000, 0.024, NA)) + # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:2
  geom_function(fun =~ if_else(.x < 2000, -0.0094, NA)) + # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:3
  geom_function(fun =~ if_else(.x < 2000, 0.028, NA)) + # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:4
  geom_function(fun =~ if_else(.x < 300, 0.413004667, NA)) + # Michael et al 2000 - Model 1 - Group 1 - CMIN
  geom_function(fun =~ if_else(.x < 300, 0.093727589, NA)) + # Michael et al 2000 - Model 1 - Group 2 -  CMIN
  geom_function(fun =~ if_else(.x < 300, 0.491040945, NA)) + # Michael et al 2000 - Model 1 - Group 3 -  CMIN
  geom_function(fun =~ if_else(.x < 300, 0.285336018, NA)) + # Michael et al 2000 - Model 2 - Group 1 -  PMIN 
  geom_function(fun =~ if_else(.x < 300, 0.10351032, NA)) + # Michael et al 2000 - Model 2 - Group 2 -  PMIN  
  geom_function(fun =~ if_else(.x < 300, 0.664476693, NA)) + # Michael et al 2000 - Model 2 - Group 3 - PMIN 
  geom_function(fun =~ if_else(.x < 300, 0.376048774, NA)) + # Michael et al 2000 - Model 3 - Group 1 - HMIN   
  geom_function(fun =~ if_else(.x < 300, 0.138936102, NA)) + # Michael et al 2000 - Model 3 - Group 2 - HMIN 
  geom_function(fun =~ if_else(.x < 300, 0.916609764, NA)) + # Michael et al 2000 -Model 3 - Group 3 - HMIN  
  geom_function(fun =~ if_else(.x < 300, 0.385807753, NA)) + # Michael et al 2000 -Model 4 - Group 1 - CMIN * HMIN 
  geom_function(fun =~ if_else(.x < 300, 0.079734102, NA)) + # Michael et al 2000 -Model 4 - Group 2 - CMIN * HMIN  
  geom_function(fun =~ if_else(.x < 300, 0.431535065, NA)) + # Michael et al 2000 - Model 4 - Group 3 - CMIN * HMIN 
  geom_function(fun =~ if_else(.x < 300, -0.062146027, NA)) + # Michael et al 2000 - Model 5 - Group 1 - CMIN * HMIN +/-  
  geom_function(fun =~ if_else(.x < 300, 0.015297231, NA)) + # Michael et al 2000 - Model 5 - Group 2 - CMIN * HMIN +/-  
  geom_function(fun =~ if_else(.x < 300, -0.054691911, NA)) + # Michael et al 2000 - Model 5 - Group 3 - CMIN * HMIN +/-  
  geom_function(fun =~ if_else(.x < 300, 0.040301826, NA)) + # Michael et al 2000 - Model 6 - Group 1 - CMAX/CMIN
  geom_function(fun =~ if_else(.x < 300, 0.004697338, NA)) + # Michael et al 2000 - Model 6 - Group 2 - CMAX/CMIN
  geom_function(fun =~ if_else(.x < 300, 0.193003386, NA)) + # Michael et al 2000 - Model 6 - Group 3 - CMAX/CMIN
  geom_function(fun =~ if_else(.x < 300, 0.432574719, NA)) + # Michael et al 2000 - Model 7 - Group 1 - CMAX/CMIN%
  geom_function(fun =~ if_else(.x < 300, 0.104469016, NA)) + # Michael et al 2000 - Model 7 - Group 2 - CMAX/CMIN%
  geom_function(fun =~ if_else(.x < 300, 0.542586148, NA)) + # Michael et al 2000 - Model 7 - Group 3 - CMAX/CMIN%
  geom_function(fun =~ if_else(.x < 300, 0.452805698, NA)) + # Michael et al 2000 - Model 8 - Group 1 - CMIN-HMIN
  geom_function(fun =~ if_else(.x < 300, 0.114391185, NA)) + # Michael et al 2000 - Model 8 - Group 2 - CMIN-HMIN
  geom_function(fun =~ if_else(.x < 300, 0.589391351, NA)) + # Michael et al 2000 - Model 8 - Group 3 - CMIN-HMIN
  geom_function(fun =~ if_else(.x < 300, 0.325085133, NA)) + # Michael et al 2000 - Model 9 - Group 1 - HMIN+ and HMIN -
  geom_function(fun =~ if_else(.x < 300, 0.098492384, NA)) + # Michael et al 2000 - Model 9 - Group 2 - HMIN+ and HMIN -
  geom_function(fun =~ if_else(.x < 300, 0.401052326, NA)) + # Michael et al 2000 - Model 9 - Group 3 - HMIN+ and HMIN -
  geom_function(fun =~ if_else(.x < 160, 2.1*0.151, NA)) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.154, NA)) + # Moore et al 2020 - IV-GMM - 1a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.114, NA)) + # Moore et al 2020 - OLS - 2a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.117, NA)) + # Moore et al 2020 - IV-GMM - 2b
  geom_function(fun =~ if_else(.x < 160, 2.1*0.121, NA)) + # Moore et al 2020 - OLS - 3a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.126, NA)) + # Moore et al 2020 - IV-GMM - 3b
  geom_function(fun =~ if_else(.x < 160, 2.1*0.099, NA)) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.101, NA)) + # Moore et al 2020 - IV-GMM - 4b
  geom_function(fun =~ if_else(.x < 160, 2.1*0.151, NA)) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.151, NA)) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.151, NA)) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.096, NA)) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.094, NA)) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.098, NA)) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x < 160, 2.1*(0.153 + -0.118*1 + -0.068*0 + -0.017*0), NA)) + # Moore et al 2020 - OLS - 1a - Florida
  geom_function(fun =~ if_else(.x < 160, 2.1*(0.153 + -0.118*0 + -0.068*1 + -0.017*0), NA)) + # Moore et al 2020 - OLS - 1a - Indiana
  geom_function(fun =~ if_else(.x < 160, 2.1*(0.153 + -0.118*0 + -0.068*0 + -0.017*1), NA)) + # Moore et al 2020 - OLS - 1a - Washington
  geom_function(fun =~ if_else(.x < 160, 2.1*(0.128 + 0.407*1 + -0.029*0 + -0.02*0), NA)) + # Moore et al 2020 - OLS - 2a - Florida
  geom_function(fun =~ if_else(.x < 160, 2.1*(0.128 + 0.407*0 + -0.029*1 + -0.02*0), NA)) + # Moore et al 2020 - OLS - 2a - Indiana
  geom_function(fun =~ if_else(.x < 160, 2.1*(0.128 + 0.407*0 + -0.029*0 + -0.02*1), NA)) + # Moore et al 2020 - OLS - 2a - Washington
  geom_function(fun =~ if_else(.x < 160, 2.1*(0.138 + 0.358*1 + -0.041*0 + -0.03*0), NA)) + # Moore et al 2020 - OLS - 3a - Florida
  geom_function(fun =~ if_else(.x < 160, 2.1*(0.138 + 0.358*0 + -0.041*1 + -0.03*0), NA)) + # Moore et al 2020 - OLS - 3a - Indiana
  geom_function(fun =~ if_else(.x < 160, 2.1*(0.138 + 0.358*0 + -0.041*0 + -0.03*1), NA)) + # Moore et al 2020 - OLS - 3a - Washington
  geom_function(fun =~ if_else(.x < 160, 2.1*(0.111 + 0.251*1 + -0.072*0 + -0.007*0), NA)) + # Moore et al 2020 - OLS - 3a - Florida
  geom_function(fun =~ if_else(.x < 160, 2.1*(0.111 + 0.251*0 + -0.072*1 + -0.007*0), NA)) + # Moore et al 2020 - OLS - 3a - Indiana
  geom_function(fun =~ if_else(.x < 160, 2.1*(0.111 + 0.251*0 + -0.072*0 + -0.007*1), NA)) + # Moore et al 2020 - OLS - 3a - Washington
  geom_function(fun =~ if_else(.x < 160, 2.1*0.21, NA)) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.09, NA)) + # Moore et al 2020 - OLS - 2a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.103, NA)) + # Moore et al 2020 - OLS - 3a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.085, NA)) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.186, NA)) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.07, NA)) + # Moore et al 2020 - OLS - 2a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.084, NA)) + # Moore et al 2020 - OLS - 3a
  geom_function(fun =~ if_else(.x < 160, 2.1*0.089, NA)) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x < 300, -7430.29*3.4/502312.8, NA)) + # Olden & Tamayo 2014 - Model 1
  geom_function(fun =~ if_else(.x < 300, -6962.7*3.4/502312.8, NA)) + # Olden & Tamayo 2014 - Model 2
  geom_function(fun =~ if_else(.x < 300, -6954.8*3.4/502312.8, NA)) + # Olden & Tamayo 2014 - Model 3
  geom_function(fun =~ if_else(.x < 300, 10.36999989*3209/103853, NA)) + # Poor et al 2001 - Lewiston
  geom_function(fun =~ if_else(.x < 300, 2.25*4391/86880, NA)) + # Poor et al 2001 - Augusta
  geom_function(fun =~ if_else(.x < 300, 1.46*2517/67881, NA)) + # Poor et al 2001 - Bangor
  geom_function(fun =~ if_else(.x < 300, 0.17+6.80, NA)) + # Swedberg el al 2020 - Multi-state model- ME ###Outlier
  geom_function(fun =~ if_else(.x < 300, 0.17+ -1.04, NA)) + # Swedberg el al 2020 - Multi-state model- MI
  geom_function(fun =~ if_else(.x < 300, 0.17+ 0.08, NA)) + # Swedberg el al 2020 - Multi-state model-  NY
  geom_function(fun =~ if_else(.x < 300, 0.17+ 0.14, NA)) + # Swedberg el al 2020 - Multi-state model-  VT
  geom_function(fun =~ if_else(.x < 300, 0.17+ -0.11, NA)) + # Swedberg el al 2020 - Multi-state model-   WI
  geom_function(fun =~ if_else(.x < 300, 0.17, NA)) + # Swedberg el al 2020 - Multi-state model
  geom_function(fun =~ if_else(.x < 300, 0.17, NA)) + # Swedberg el al 2020 - Multi-state model - drop ME
  geom_function(fun =~ if_else(.x < 300, 0.17, NA)) + # Swedberg el al 2020 - Multi-state model - drop MI
  geom_function(fun =~ if_else(.x < 300, 0.13, NA)) + # Swedberg el al 2020 - Multi-state model - drop MN
  geom_function(fun =~ if_else(.x < 300, 0.16, NA)) + # Swedberg el al 2020 - Multi-state model - drop NY
  geom_function(fun =~ if_else(.x < 300, 0.17, NA)) + # Swedberg el al 2020 - Multi-state model - drop VT
  geom_function(fun =~ if_else(.x < 300, 0.17, NA)) + # Swedberg el al 2020 - Multi-state model - drop WI
  geom_function(fun =~ if_else(.x < 300, -2.71, NA)) + # Swedberg el al 2020 - State level model - Maine
  geom_function(fun =~ if_else(.x < 300, 9.20, NA)) + # Swedberg el al 2020 - State level model - Michigan
  geom_function(fun =~ if_else(.x < 300, 0.17, NA)) + # Swedberg el al 2020 - State level model - Minnesota
  geom_function(fun =~ if_else(.x < 300, 0.13, NA)) + # Swedberg el al 2020 - State level model - New York
  geom_function(fun =~ if_else(.x < 300, -0.32, NA)) + # Swedberg el al 2020 - State level model - Vermont
  geom_function(fun =~ if_else(.x < 300, 0.1, NA)) + # Swedberg el al 2020 - State level model - Wisconsin
  geom_function(fun =~ if_else(.x < 300, 0.5, NA)) + # Swedberg el al 2020 - Regional model - Otter Tail
  geom_function(fun =~ if_else(.x < 300, 0.1, NA)) + # Swedberg el al 2020 - Regional model - Twin Cities
  geom_function(fun =~ if_else(.x < 300, -0.21, NA)) + # Swedberg el al 2020 - Regional model - Adirondacks
  geom_function(fun =~ if_else(.x < 300, 0.41, NA)) + # Swedberg el al 2020 - Regional model - Regional model - Finger Lakes
  geom_function(fun =~ if_else(.x < 300, 0.129, NA)) + # Weng et al 2020 - Secci depth
  geom_function(fun =~ if_else(.x < 139, 0.0352 + 0.0755 * log(2.39), NA)) + # Wolf & Kemp 2021 - Model 1
  geom_function(fun =~ if_else(.x < 139, 0.0136 + 0.0543 * log(2.39), NA)) + # Wolf & Kemp 2021 - Model 1
  geom_function(fun =~ if_else(.x < 139, 0.0548 * log(2.39), NA)) + # Wolf & Kemp 2021 - Model 2
  geom_function(fun =~ if_else(.x < 139, 0.0291 * log(2.39), NA)) + # Wolf & Kemp 2021 - Model 2
  geom_function(fun =~ if_else(.x < 139, 0.0092 + 0.0498 * log(2.39), NA)) + # Wolf & Kemp 2021 - Model 3
  geom_function(fun =~ if_else(.x < 139, -0.0205 + 0.0444 * log(2.39), NA)) + # Wolf & Kemp 2021 - Model 3
  geom_function(fun =~ if_else(.x < 139, -0.0147 + 0.0447 * log(2.39), NA)) + # Wolf & Kemp 2021 - Model 4
  geom_function(fun =~ if_else(.x < 139, -0.0242 + 0.0421 * log(2.39), NA)) + # Wolf & Kemp 2021 - Model 4
  geom_function(fun =~ if_else(.x < 139, -0.0086 + 0.0432 * log(2.39), NA)) + # Wolf & Kemp 2021 - Model 5
  geom_function(fun =~ if_else(.x < 139, 0.022 + 0.0407 * log(2.39), NA))+ # Wolf & Kemp 2021 - Model 5
  geom_function(fun =~ if_else(.x < 300, -0.647772014, NA)) + # Zhang & Boyle 2010 - Milfoil - Quadratic
  geom_function(fun =~ if_else(.x < 300, -0.647772014, NA)) + # Zhang & Boyle 2010 - Milfoil - Exponential
  geom_function(fun =~ if_else(.x < 300, -0.323886007, NA)) + # Zhang & Boyle 2010 - Total macrophytes - Quadratic
  geom_function(fun =~ if_else(.x < 300, -0.323886007, NA)) + # Zhang & Boyle 2010 - Total macrophytes - Exponential
  geom_function(fun =~ if_else(.x < 300, -0.161943004, NA)) + # Zhang & Boyle 2010 - Best Model - Quadratic
  geom_function(fun =~ if_else(.x < 300, -0.323886007, NA)) + # Zhang & Boyle 2010 - Best Model - Exponential
  
  geom_function(fun =~ if_else(.x < 300, 0.18130137, NA)) + # Zhang et al 2015 - VT1
  geom_function(fun =~ if_else(.x < 300, 0.168578461, NA)) + # Zhang et al 2015 - VT2
  geom_function(fun =~ if_else(.x < 300, 0.031807259, NA)) + # Zhang et al 2015 - VT3
  geom_function(fun =~ if_else(.x < 300, 0.368520737, NA)) + # Zhang et al 2015 - ME1
  geom_function(fun =~ if_else(.x < 300, -0.044222489, NA)) + # Zhang et al 2015 - ME2
  geom_function(fun =~ if_else(.x < 300, 0.078617759, NA)) + # Zhang et al 2015 - ME3
  geom_function(fun =~ if_else(.x < 300, 0.560151517, NA)) + # Zhang et al 2015 - ME4
  geom_function(fun =~ if_else(.x < 300, -0.00491361, NA)) + # Zhang et al 2015 - ME5
  geom_function(fun =~ if_else(.x < 300, -0.01965444, NA)) + # Zhang et al 2015 - ME6
  geom_function(fun =~ if_else(.x < 300, 1.719763517, NA)) + # Zhang et al 2015 - ME7
  geom_function(fun =~ if_else(.x < 300, 0.035835754, NA)) + # Zhang et al 2015 - NH1
  geom_function(fun =~ if_else(.x < 300, 0.04596325, NA)) + # Zhang et al 2015 - NH2
  geom_function(fun =~ if_else(.x < 300, 1.505880713, NA)) + # Zhang et al 2015 - NH3
  geom_function(fun =~ if_else(.x < 300, 0.971460581, NA)) + # Zhang et al 2015 - NH4
  geom_function(fun =~ if_else(.x < 300, 0.185411081, NA)) # Zhang et al 2015 - NH5

  ggtitle("Hedonic function without distance involvement")+
  xlab("Distance (Meters)") + ylab("Elasticity")

  

  
# Linear distance



base_linear <-
  ggplot() +
  xlim(0, 2000)


base_linear + geom_function(fun =~ if_else(.x < 2000, 0.248 + -0.076*(log(.x/100)),NA)) + # # Irwin & Wolf 2022 - Model 5 (2km)
  geom_function(fun =~ if_else(.x < 2000, (0.0964 + -0.0305*(.x/1609.34))*0.542544*3.28,NA)) + # Liu et al 2019 - without distance threshhold interaction( sample within 5 miles)
  geom_function(fun =~ if_else(.x < 2000, (0.2096 + -0.0426*(.x/1609.34))*0.542544*3.28,NA)) + # Liu et al 2019 - property fixed effect model - 2 or more sale
  geom_function(fun =~ if_else(.x < 2000, (0.2638 + -0.0682*(.x/1609.34))*0.542544*3.28,NA)) + # Liu et al 2019 - property fixed effect model - 3 or more sale
  geom_function(fun =~ if_else(.x < 2000, (0.4503 + -0.1117*(.x/1609.34))*0.542544*3.28,NA)) + # Liu et al 2019 - property fixed effect model - 4 or more sale
  geom_function(fun =~ if_else(.x < 2000, (0.0841 + -0.025*(.x/1609.34))*0.542544*3.28,NA)) + # Liu et al 2019 - property fixed effect model - 4 or more sale
  geom_function(fun =~ if_else(.x < 2000, (6966.6554 + -469.5857*(.x/1609.34))*(0.542544*3.28/217951.2),NA)) + # Liu et al 2019 - linear model
  ggtitle("Hedonic function with linear distance interation")+
  xlab("Distance (Meters)") + ylab("Elasticity")


# Linear distance + dummy distance

base_linear_dummy <-
  ggplot() +
  xlim(0, 2000)

base_linear_dummy + geom_function(fun =~ if_else(.x < 300,(0.0876 + 0.1973*1 + (-0.0287* .x/1609.34))*0.542544*3.28,NA))+ # Liu et al 2019 ( sample within 5 miles, waterfron adjucent)
  geom_function(fun =~ if_else(.x > 300 & .x < 2000,(0.0876 + 0.1973*0 + (-0.0287* .x/1609.34))*0.542544*3.28,NA))+
  
  geom_function(fun =~ if_else(.x < 161,(0.0914 + 0.1568*1 + (-0.0292* .x/1609.34))*0.542544*3.28,NA))+ # Liu et al 2019 ( sample within 5 miles, 0.1 miles)
  geom_function(fun =~ if_else(.x > 161 & .x < 2000,(0.0914 + 0.1568*0 + (-0.0292* .x/1609.34))*0.542544*3.28,NA)) +

  geom_function(fun =~ if_else(.x < 482,(0.0937 + 0.0772*1 + (-0.0294* .x/1609.34))*0.542544*3.28,NA))+ # Liu et al 2019 ( sample within 5 miles, waterfront - 0.3 miles)
  geom_function(fun =~ if_else(.x > 482 & .x < 2000,(0.0937 + 0.0772*0 + (-0.0294* .x/1609.34))*0.542544*3.28,NA)) +
  
  geom_function(fun =~ if_else(.x < 804,(0.0951 + 0.02*1 + (-0.0302* .x/1609.34))*0.542544*3.28,NA))+ # Liu et al 2019 ( sample within 5 miles, waterfront - 0.5 miles)
  geom_function(fun =~ if_else(.x > 804 & .x < 2000,(0.0951 + 0.02*0 + (-0.0302* .x/1609.34))*0.542544*3.28,NA)) +
  
  geom_function(fun =~ if_else(.x < 965,(0.0961 + 0.0055*1 + (-0.0304* .x/1609.34))*0.542544*3.28,NA))+ # Liu et al 2019 ( sample within 5 miles, waterfront - 0.6 miles)
  geom_function(fun =~ if_else(.x > 965 & .x < 2000,(0.0961 + 0.0055*0 + (-0.0304* .x/1609.34))*0.542544*3.28,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,(0.0742 + 0.1978*1 + (-0.0237* .x/1609.34))*0.542544*3.28,NA))+ # Liu et al 2019 ( sample within 5 miles, waterfron adjucent)
  geom_function(fun =~ if_else(.x > 300 & .x < 2000, (0.0742 + 0.1978*0 + (-0.023* .x/1609.34))*0.542544*3.28,NA)) +
  
  geom_function(fun =~ if_else(.x < 161,(0.0783 + 0.1559*1 + (-0.0236* .x/1609.34))*0.542544*3.28,NA))+ # Liu et al 2019 ( sample within 5 miles, 0.1 miles)
  geom_function(fun =~ if_else(.x > 161 & .x < 2000,(0.0783 + 0.1559*0 + (-0.0236* .x/1609.34))*0.542544*3.28,NA)) +
  
  geom_function(fun =~ if_else(.x < 482,(0.0809 + 0.077*1 + (-0.0238* .x/1609.34))*0.542544*3.28,NA))+ # Liu et al 2019 ( sample within 5 miles, waterfront - 0.3 miles)
  geom_function(fun =~ if_else(.x > 482 & .x < 2000,(0.0809 + 0.077*0 + (-0.0238* .x/1609.34))*0.542544*3.28,NA)) +
  
  geom_function(fun =~ if_else(.x < 804,(0.0822 + 0.0285*1 + (-0.0245* .x/1609.34))*0.542544*3.28,NA))+ # Liu et al 2019 ( sample within 5 miles, waterfront - 0.5 miles)
  geom_function(fun =~ if_else(.x > 804 & .x < 2000,(0.0822 + 0.0285*0 + (-0.0245* .x/1609.34))*0.542544*3.28,NA)) +
  
  geom_function(fun =~ if_else(.x < 965,(0.0834 + 0.0148*1 + (-0.0249* .x/1609.34))*0.542544*3.28,NA))+ # Liu et al 2019 ( sample within 5 miles, waterfront - 0.6 miles)
  geom_function(fun =~ if_else(.x > 965 & .x < 2000,(0.0834 + 0.0148*0 + (-0.0249* .x/1609.34))*0.542544*3.28,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,(4165.1918 + 63099.4773*1 + (96.6563* .x/1609.34))*(0.542544*3.28/217951.2),NA))+ # Liu et al 2019 waterfron adjucent - linear)
  geom_function(fun =~ if_else(.x > 300 & .x < 2000,(4165.1918 + 63099.4773*0 + (96.6563* .x/1609.34))*(0.542544*3.28/217951.2),NA)) +
  
  geom_function(fun =~ if_else(.x < 161,(5450.1438 + 48099.1068*1 + (-80.7421* .x/1609.34))*(0.542544*3.28/217951.2),NA))+ # Liu et al 2019 ( sample within 5 miles, 0.1 miles - linear)
  geom_function(fun =~ if_else(.x > 161 & .x < 2000,(5450.1438 + 48099.1068*0 + (-80.7421* .x/1609.34))*(0.542544*3.28/217951.2),NA)) +
  
  geom_function(fun =~ if_else(.x < 482,(6215.4558 + 21609.3494*1 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA))+ # Liu et al 2019 ( sample within 5 miles, waterfront - 0.3 miles - linear)
  geom_function(fun =~ if_else(.x > 482 & .x < 2000, (6215.4558 + 21609.3494*0 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA)) +
  
  geom_function(fun =~ if_else(.x < 804,(6215.4558 + 21609.3494*1 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA))+# Liu et al 2019 ( sample within 5 miles, waterfront - 0.5 miles - linear)
  geom_function(fun =~ if_else(.x > 804 & .x < 2000, (6215.4558 + 21609.3494*0 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA)) +
  
  geom_function(fun =~ if_else(.x < 965,(6807.1986 + 3826.6165*1 + (-437.7854* .x/1609.34))*(0.542544*3.28/217951.2),NA))+# Liu et al 2019 ( sample within 5 miles, waterfront - 0.5 miles - linear)
  geom_function(fun =~ if_else(.x > 965 & .x < 2000, (6807.1986 + 3826.6165*0 + (-437.7854* .x/1609.34))*(0.542544*3.28/217951.2),NA)) +
  
  geom_function(fun =~ if_else(.x < 300,(0.0084 + 0.0099*1 + (-0.0016*.x)/1000)*1.862328*3.28,NA))+# Nepf et al 2022 - Model 1 - continious distance 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000, (0.0084 + 0.0099*0 + (-0.0016*.x)/1000)*1.862328*3.28,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,(0.0208 + 0.001*1 + (-0.0029*.x)/1000)*1.862328*3.28,NA))+ # # Nepf et al 2022 - Model 2 - continious distance 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000,(0.0208 + 0.001*0 + (-0.0029*.x)/1000)*1.862328*3.28 ,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,(0.0191 + 0.004*1 + (-0.0022*.x)/1000)*1.862328*3.28,NA))+ # Nepf et al 2022 - Model 3 - continious distance 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000, (0.0191 + 0.004*0 + (-0.0022*.x)/1000)*1.862328*3.28 ,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, (0.0221 + -0.0003*1 + (-0.0031*.x)/1000)*1.862328*3.28,NA))+ # Nepf et al 2022 - Model 4 - continious distance  
  geom_function(fun =~ if_else(.x > 300 & .x < 2000, (0.0221 + 0.004*0 + (-0.0003*.x)/1000)*1.862328*3.28,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, (0.0026 + 0.0277*1 + (0.0005*.x)/1000)*1.862328*3.28,NA))+ # Nepf et al 2022 - Model 9 - continious distance/regional analysis 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000, (0.0026 + 0.0277*0  + (0.0005*.x)/1000)*1.862328*3.28,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,(-0.0016 + 0.0262*1 + (0.0012*.x)/1000)*1.862328*3.28,NA))+ # Nepf et al 2022 - Model 10 - continious distance/regional analysis
  geom_function(fun =~ if_else(.x > 300 & .x < 2000, (-0.0016 + 0.0262*0  + (0.0012*.x)/1000)*1.862328*3.28,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,(0.007 + 0.0263*1 + (0.0004*.x)/1000)*1.862328*3.28,NA))+ # Nepf et al 2022 - Model 11 - continious distance 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000, (0.007 + 0.0263*0  + (0.0004*.x)/1000)*1.862328*3.28,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,(-0.0009 + 0.0266*1 + (0.0016*.x)/1000)*1.862328*3.28,NA))+ #  Nepf et al 2022 - Model 12 - continious distance
  geom_function(fun =~ if_else(.x > 300 & .x < 2000,  (-0.0009 + 0.0266*0   (0.0016*.x)/1000)*1.862328*3.28 ,NA)) +
  
  geom_function(fun =~ if_else(.x < 50,0.117 + 0.08*1 + -0.017* log(.x),NA))+ #  Walsh et al 2011a - Model 2
  geom_function(fun =~ if_else(.x > 50 & .x < 467,  0.117 + 0.08*0 + -0.017* log(.x) ,NA)) +
  
  geom_function(fun =~ if_else(.x < 50,0.117 + 0.08*1 + -0.017* log(.x) + 0.011*log(519.2999878),NA))+ #  Walsh et al 2011a - Model 3
  geom_function(fun =~ if_else(.x > 50 & .x < 467,  0.117 + 0.08*0 + -0.017* log(.x) + 0.011*log(519.2999878) ,NA)) +
  
  geom_function(fun =~ if_else(.x < 50,0.118 + 0.081*1 + -0.017* log(.x),NA))+ #  Walsh et al 2011a - Model 2S
  geom_function(fun =~ if_else(.x > 50 & .x < 467,  0.118 + 0.08*0 + -0.017* log(.x) ,NA)) +
  
  geom_function(fun =~ if_else(.x < 50,-0.03 + 0.079*1 + -0.017* log(.x) + 0.012*log(519.2999878),NA))+ #  Walsh et al 2011a - Model 3S
  geom_function(fun =~ if_else(.x > 50 & .x < 467,  0.117 + 0.08*0 + -0.017* log(.x) + 0.011*log(519.2999878) ,NA))
  
        


  ggtitle("Hedonic function without distance and distance dummy for waterfront ")+
  xlab("Distance (Meters)") + ylab("Elasticity")
  


# Distance Dummy

base_dummy <-
  ggplot() +
  xlim(0, 2000)


base_dummy + geom_function(fun =~ if_else(.x < 200, -0.099399999*-1,NA))+ # Guignet et al 2017 - 2.C - 0-200m
  geom_function(fun =~ if_else(.x > 200 & .x < 500, -0.0058*-1,NA))+ # Guignet et al 2017 - 2.C - 200-500m
  
  geom_function(fun =~ if_else(.x < 100,0.0076 + 0.1673, NA))+# Mamun et al 2023 (Model 1 - discrete distance bins/FE:Tract*year)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0076 + 0.0405,NA))+
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0076,NA))+
  
  geom_function(fun =~ if_else(.x < 100, 0.0199 + 0.1778, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:County*year)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0199 + 0.0435, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0199,NA))+

  geom_function(fun =~ if_else(.x < 100, -0.0383 + 0.1758, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:State*year)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , -0.0383 + 0.0477, NA))+
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , -0.0383,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, -0.0105 + 0.1674, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:Tract+year)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , -0.0105 + 0.0437, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , -0.0105,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0141 + 0.1804, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:County+year)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0141 + 0.0475, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0141,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, -0.0343 + 0.1797, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:State+year)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , -0.0343 + 0.0514, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , -0.0343,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0287 + 0.1581, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:Block*year)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0287 + 0.0388, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0287,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0108 + 0.1568, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:Block+year)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0108 + 0.0413, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0108,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, -0.0051 + 0.2315, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:year)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , -0.0051 + 0.0726, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , -0.0051,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, -0.0123 + 0.1678, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:tract)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , -0.0123 + 0.0439, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , -0.0123,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0085 + 0.1573, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S4:2)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0085 + 0.0414, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0085,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0072 + 0.1532, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S4:3)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0072 + 0.0401, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0072,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0136 + 0.1586, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S4:5)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0136 + 0.0455, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0136,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0136 + 0.1586, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S4:6)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0136 + 0.0455, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0136,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0101 + 0.1666, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S4:7)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0101 + 0.055, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0101,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, -0.0018 + 0.1377, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S4:10)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , -0.0018 + 0.0497, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , -0.0018,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0108 + 0.1654, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S5:10)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0108 + 0.0417, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0108,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0225 + 0.1582, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S5:30)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0225 + 0.0472, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0225,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0231 + 0.129, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S5:100)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0231 + 0.0461, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0231,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0104 + 0.1461, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S6:5)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0104 + 0.0364, NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0104,NA))+ 

  geom_function(fun =~ if_else(.x < 300, 0.0123 + 0.1563, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S7:1) waterfront we define 300m
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0123,NA))+ 
  
  geom_function(fun =~ if_else(.x < 50, 0.0424 + 0.0953, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S7:2) 
  geom_function(fun =~ if_else(.x > 50 & .x < 2000 , 0.0424,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0157 + 0.151, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S7:3) 
  geom_function(fun =~ if_else(.x > 100 & .x < 2000 , 0.0157,NA))+ 
  
  geom_function(fun =~ if_else(.x < 200, 0.0076 + 0.1296, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S7:4) 
  geom_function(fun =~ if_else(.x > 200 & .x < 2000 , 0.0076,NA))+ 
  
  geom_function(fun =~ if_else(.x < 50, 0.022 + 0.1439, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S7:5)
  geom_function(fun =~ if_else(.x > 50 & .x < 200 , 0.022 + 0.1134,NA))+ 
  geom_function(fun =~ if_else(.x > 200 & .x < 2000 , 0.022, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0104 + 0.1636, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S7:6)
  geom_function(fun =~ if_else(.x > 100 & .x < 200 , 0.0104 + 0.0507,NA))+ 
  geom_function(fun =~ if_else(.x > 200 & .x < 2000 , 0.0104, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0076 + 0.1673, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S7:7)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0076 + 0.0405,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0076, NA))+ 
  
  geom_function(fun =~ if_else(.x < 200, 0.0037 + 0.1359, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S7:8)
  geom_function(fun =~ if_else(.x > 200 & .x < 500 , 0.0037 + 0.0132,NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 2000 , 0.0037, NA))+ 
  
  geom_function(fun =~ if_else(.x < 300, 0.0019 + 0.1089, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S7:9)
  geom_function(fun =~ if_else(.x > 300 & .x < 1000 , 0.0019 + 0.0031,NA))+ 
  geom_function(fun =~ if_else(.x > 1000 & .x < 2000 , 0.0019, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0041 + 0.155, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S8:0)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0041 + 0.041,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0041, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0166 + 0.1579, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S8:1)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0166 + 0.0375,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0166, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0121 + 0.1645, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S8:3)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0121 + 0.0393,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0121, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0076 + 0.1673, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S8:5)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0076 + 0.0405,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0076, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0072 + 0.1688, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S8:10)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0072 + 0.041,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0072, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0078 + 0.1728, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S8:anytime)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0078 + 0.0382,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0078, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0076 + 0.1673, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S9:customer price index)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0076 + 0.0405,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0076, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0072 + 0.1532, NA))+ # Mamun et al 2023 (Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0072 + 0.0401,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0072, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0126 + 0.1148, NA))+ # Mamun et al 2023 (Model 2 - discrete distance bins/Table S10:1)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0126 + 0.0184,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0126, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0173 + 0.1676, NA))+ # Mamun et al 2023 (Model 2 - discrete distance bins/Table S10:2)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0173 + 0.0401,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0401, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0076 + 0.1673, NA))+ # Mamun et al 2023 (Model 2 - discrete distance bins/Table S10:4)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0076 + 0.0405,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0076, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.007 + 0.0789, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S11:ln(housing price))
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.007 + 0.0227,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.007, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0113 + 0.1285 + -0.0469, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S12:ecoregion:CPL)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0113 + 0.0204  + 0.0155,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0113, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0113 + 0.1285 + 0.0824, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S12:ecoregion:NAP)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0113 + 0.0204 + 0.0333,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0113, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0113 + 0.1285 + -0.2392, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S12:ecoregion:NPL)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0113 + 0.0204 + -0.0445,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0113, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0113 + 0.1285 + 0.1601, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S12:ecoregion:SAP)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0113 + 0.0204 + 0.1002,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0113, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0113 + 0.1285 + -0.1651, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S12:ecoregion:SPL)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0113 + 0.0204 + -0.0836,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0113, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0113 + 0.1285 + -0.1553, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S12:ecoregion:TPL)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0113 + 0.0204 + -0.0477,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0113, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0113 + 0.1285 + 0.0551, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S12:ecoregion:WMT)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0113 + 0.0204 + 0.0303,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0113, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0113 + 0.1285 + -0.0765, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S12:ecoregion:XER)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0113 + 0.0204 + 0.0002,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0113, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.1364, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:AL)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0949,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.0547, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:AR)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.207,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
 
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.1572, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:AZ)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0368,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.1312, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:CA)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0552,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.2389, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:CO)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0367,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.1809, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:CT)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.065,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.5047, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:DE)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.2419,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.1284, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:FL)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0018,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.5163, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:GA)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.2384,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.0758, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:IA)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.1,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.3359, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:IL)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.2071,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0794, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:IN)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.039,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 1.2532, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:KY)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0888,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0273, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:LA)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0462,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0285, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:MA)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.059,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.1581, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:MD)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0346,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.1299, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:ME)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.1871,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.0913, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:MI)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0146,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0672, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:MO)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.2849,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.3935, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:MS)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.6851,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.4651, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:MT)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.3235,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.108, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:NC)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.047,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0542, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:NE)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.1447,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0968, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:NH)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0315,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.2329, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:NJ)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0775,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0915, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:NV)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0197,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.1677, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:NY)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.074,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.3565, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:OH)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.051,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.3811, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:OK)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.3099,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.0729, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:OR)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0604,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0623, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:PA)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0366,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.4243, NA))+ # Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:RI)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0761,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.2388, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:SC)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.1485,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0699, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:SD)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0298,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.1608, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:TN)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.1244,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.1807, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:TX)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0074,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.9602, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:UT)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0612,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.3189, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:VA)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0059,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0231, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:VT)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0491,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.0623, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:WA)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0217,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.0069, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:WI)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0208,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 

  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.0314, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:WV)
  geom_function(fun =~ if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.1154,NA))+ 
  geom_function(fun =~ if_else(.x > 300 & .x < 2000 , 0.0134, NA))+ 
  
  geom_function(fun =~ if_else(.x < 300,(-0.0015 + 0.0190*1 + 0.0138*0 + 0.0083*0 + 0.0095*0 + -0.0003*0)*3.28*1.86 , NA))+# Nepf et al 2022 - Model 5   - discrete distance bins - Tract FE
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , (0.0190*0 + 0.0138*1 + 0.0083*0 + 0.0095*0 + -0.0003*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , (0.0190*0 + 0.0138*0 + 0.0083*1 + 0.0095*0 + -0.0003*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1000 & .x < 1500 , (0.0190*0 + 0.0138*0 + 0.0083*0 + 0.0095*1 + -0.0003*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1500 & .x < 2000 , (0.0190*0 + 0.0138*0 + 0.0083*0 + 0.0095*1 + -0.0003*0)*3.28*1.86,NA))+ 
  
  geom_function(fun =~ if_else(.x < 300,(0.0088 + 0.0108*1 + 0.0110*0 + 0.0077*0 + 0.0088*0 + 0.0040*0)*3.28*1.86 , NA))+# Nepf et al 2022 - Model 6   - discrete distance bins - Town FE
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , (0.0088 + 0.0108*0 + 0.0110*1 + 0.0077*0 + 0.0088*0 + 0.0040*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*1 + 0.0088*0 + 0.0040*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1000 & .x < 1500 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*0 + 0.0088*1 + 0.0040*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1500 & .x < 2000 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*0 + 0.0088*0 + 0.0040*1)*3.28*1.86,NA))+ 
  

  geom_function(fun =~ if_else(.x < 300,(0.0051 + 0.0155*1 + 0.0139*0 + 0.0116*0 + 0.0132*0 + 0.0046*0)*3.28*1.86 , NA))+# Nepf et al 2022 - Model 7   - discrete distance bins - School FE
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , (0.0051 + 0.0155*0 + 0.0139*1 + 0.0116*0 + 0.0132*0 + 0.0046*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*1 + 0.0132*0 + 0.0046*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1000 & .x < 1500 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*0 + 0.0132*1 + 0.0046*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1500 & .x < 2000 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*0 + 0.0132*0 + 0.0046*1)*3.28*1.86,NA))+ 
  
  geom_function(fun =~ if_else(.x < 300,(0.0090 + 0.0103*1 + 0.0118*0 + 0.0084*0 + 0.0094*0 + 0.0051*0)*3.28*1.86 , NA))+# Nepf et al 2022 - Model 8   - discrete distance bins - Town x Year FE
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , (0.0090 + 0.0103*0 + 0.0118*1 + 0.0084*0 + 0.0094*0 + 0.0051*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*1 + 0.0094*0 + 0.0051*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1000 & .x < 1500 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*0 + 0.0094*1 + 0.0051*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1500 & .x < 2000 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*0 + 0.0094*0 + 0.0051*1)*3.28*1.86,NA))+ 
  
  geom_function(fun =~ if_else(.x < 300,(0.0050 + 0.0260*1 + 0.0096*0 + -0.0141*0 + -0.0056*0 + -0.0079*0)*3.28*1.86 , NA))+# Nepf et al 2022 - Model 8   - discrete distance bins - regional analysis - Tract FE
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , (0.0050 + 0.0260*0 + 0.0096*1 + -0.0141*0 + -0.0056*0 + -0.0079*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*1 + -0.0056*0 + -0.0079*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1000 & .x < 1500 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*0 + -0.0056*1 + -0.0079*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1500 & .x < 2000 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*0 + -0.0056*0 + -0.0079*1)*3.28*1.86,NA))+ 
  
  geom_function(fun =~ if_else(.x < 300,(0.0072 + 0.0170*1 + -0.0048*0 + -0.0193*0 + -0.0007*0 + -0.0167*0)*3.28*1.86 , NA))+# Nepf et al 2022 - Model 8   - discrete distance bins - regional analysis - Town FE
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , (0.0072 + 0.0170*0 + -0.0048*1 + -0.0193*0 + -0.0007*0 + -0.0167*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*1 + -0.0007*0 + -0.0167*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1000 & .x < 1500 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*0 + -0.0007*1 + -0.0167*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1500 & .x < 2000 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*0 + -0.0007*0 + -0.0167*1)*3.28*1.86,NA))+ 
  
  geom_function(fun =~ if_else(.x < 300,(0.0098 + 0.0248*1 + 0.0114*0 + -0.0144*0 + -0.0011*0 + -0.0144*0)*3.28*1.86 , NA))+# Nepf et al 2022 - Model 8   - discrete distance bins - regional analysis - School FE
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , (0.0098 + 0.0248*0 + 0.0114*1 + -0.0144*0 + -0.0011*0 + -0.0144*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*1 + -0.0011*0 + -0.0144*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1000 & .x < 1500 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*0 + -0.0011*1 + -0.0144*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1500 & .x < 2000 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*0 + -0.0011*0 + -0.0144*1)*3.28*1.86,NA))+ 
  
  geom_function(fun =~ if_else(.x < 300,(0.0095 + 0.0156*1 + -0.0063*0 + -0.0205*0 + -0.0027*0 + -0.0184*0)*3.28*1.86 , NA))+# Nepf et al 2022 - Model 8   - discrete distance bins - regional analysis - TownxYear FE
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , (0.0095 + 0.0156*0 + -0.0063*1 + -0.0205*0 + -0.0027*0 + -0.0184*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*1 + -0.0027*0 + -0.0184*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1000 & .x < 1500 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*0 + -0.0027*1 + -0.0184*0)*3.28*1.86,NA))+ 
  geom_function(fun =~ if_else(.x > 1500 & .x < 2000 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*0 + -0.0027*0 + -0.0184*1)*3.28*1.86,NA))+ 
  
  geom_function(fun =~ if_else(.x < 500, 0.06, NA))+ # TJ, Pat, Richard - Model 1 - discrete distance bin-500m
  
  geom_function(fun =~ if_else(.x < 250, 0.063, NA))+# TJ, Pat, Richard - Model 2 - discrete distance bin: 0-250m & 250m-500m
  geom_function(fun =~ if_else(.x > 250 & .x < 500 , 0.056,NA))+ 
  
  geom_function(fun =~ if_else(.x < 250, 0.056, NA))+# TJ, Pat, Richard - Model 3 - discrete distance bin: 0-250m & 250m-500m
  geom_function(fun =~ if_else(.x > 250 & .x < 500 , 0.007,NA))+ 
  
  geom_function(fun =~ if_else(.x < 500, 0.06, NA))+ # TJ, Pat, Richard - Model 4 - discrete distance bin-500m
  
  geom_function(fun =~ if_else(.x < 250, 0.061, NA))+# TJ, Pat, Richard - Model 5 - discrete distance bin: 0-250m & 250m-500m
  geom_function(fun =~ if_else(.x > 250 & .x < 500 , 0.059,NA))+ 
  
  geom_function(fun =~ if_else(.x < 250, 0.087, NA))+# TJ, Pat, Richard - Model 6 - discrete distance bin: 0-250m & 250m-500m
  geom_function(fun =~ if_else(.x > 250 & .x < 500 , 0.063,NA))+ 
  
  geom_function(fun =~ if_else(.x < 250, 0.063, NA))+# TJ, Pat, Richard - Model 7 - discrete distance bin: 0-250m & 250m-500m
  geom_function(fun =~ if_else(.x > 250 & .x < 500 , 0.056,NA))+ 
  
  geom_function(fun =~ if_else(.x < 500, 0.044, NA))+ # TJ, Pat, Richard - Model 8 - discrete distance bin-500m
  
  geom_function(fun =~ if_else(.x < 250, 0.041, NA))+# TJ, Pat, Richard - Model 9 - discrete distance bin: 0-250m & 250m-500m
  geom_function(fun =~ if_else(.x > 250 & .x < 500 ,0.048,NA))+ 
  
  geom_function(fun =~ if_else(.x < 250, 0.041, NA))+# TJ, Pat, Richard - Model 9 - discrete distance bin: 0-250m & 250m-500m
  geom_function(fun =~ if_else(.x > 250 & .x < 500 ,0.048,NA))+ 
  
  geom_function(fun =~ if_else(.x < 250, 0.152, NA))+# TJ, Pat, Richard - Model 10 - discrete distance bin: 0-250m & 250m-500m
  geom_function(fun =~ if_else(.x > 250 & .x < 500 ,0.083,NA))+ 
  
  geom_function(fun =~ if_else(.x < 100, 0.072, NA))+# TJ, Pat, Richard - Model 11 - discrete distance bins from 0-500m by 100m
  geom_function(fun =~ if_else(.x > 100 & .x < 200 , 0.062,NA))+ 
  geom_function(fun =~ if_else(.x >200 & .x < 300 , 0.053,NA))+ 
  geom_function(fun =~ if_else(.x >300 & .x < 400 , 0.056,NA))+ 
  geom_function(fun =~ if_else(.x >400 & .x < 500 , 0.062,NA))+ 
  
  geom_function(fun =~ if_else(.x < 250, 0.058, NA))+# TJ, Pat, Richard - Model 12 - discrete distance bin: 0-250m & 250m-500m
  geom_function(fun =~ if_else(.x > 250 & .x < 500 ,0.056,NA))+ 
  
  geom_function(fun =~ if_else(.x < 250, 0.124, NA))+# TJ, Pat, Richard - Model 13 - discrete distance bin: 0-250m & 250m-500m
  geom_function(fun =~ if_else(.x > 250 & .x < 500 ,0.074,NA))+ 
  
  geom_function(fun =~ if_else(.x < 500, 0.031, NA))+ # TJ, Pat, Richard - Model 14 - discrete distance bin-500m - different fixed effects
  
  geom_function(fun =~ if_else(.x < 500, 0.024, NA))+ # TJ, Pat, Richard - Model 14 - discrete distance bin-500m - different fixed effects
  
  geom_function(fun =~ if_else(.x < 500, 0.029, NA))+ # TJ, Pat, Richard - Model 14 - discrete distance bin-500m - different fixed effects
  
  geom_function(fun =~ if_else(.x < 500, 0.018, NA))+ # TJ, Pat, Richard - Model 14 - discrete distance bin-500m - different fixed effects
  
  geom_function(fun =~ if_else(.x < 500, 0.064, NA))+ # TJ, Pat, Richard - Model 14 - discrete distance bin-500m - different fixed effects
  
  geom_function(fun =~ if_else(.x < 500, 0.06, NA))+ # TJ, Pat, Richard - Model 14 - discrete distance bin-500m - different fixed effects
  
  geom_function(fun =~ if_else(.x < 500, -0.04, NA))+ # TJ, Pat, Richard - Model 14 - discrete distance bin-500m - different fixed effects
  
  geom_function(fun =~ if_else(.x < 500, 0.01, NA))+ # TJ, Pat, Richard - Model 14 - discrete distance bin-500m - different fixed effects
  
  geom_function(fun =~ if_else(.x < 500, 0.03, NA)) + # TJ, Pat, Richard - Model 14 - discrete distance bin-500m - different fixed effects
 
  geom_function(fun =~ if_else(.x < 50, 0.017 + 0.115, NA))+# Walsh et al 2011a - Model 1
  geom_function(fun =~ if_else(.x > 50 & .x < 467 ,0.017,NA))+
  
  geom_function(fun =~ if_else(.x < 50, 0.017 + 0.11, NA))+# Walsh et al 2011a - Model 1S
  geom_function(fun =~ if_else(.x > 50 & .x < 467 ,0.017,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.126, NA))+ #Walsh et al 2017 - Anne Arundel 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , 0.023, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.009,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.090, NA))+ #Walsh et al 2017 - Baltimore county 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , -0.009, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  0.015,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.033, NA))+ #Walsh et al 2017 - Calvert 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , -0.001, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  -0.021,NA))+
  
  geom_function(fun =~ if_else(.x < 300, -0.010, NA))+ #Walsh et al 2017 - Cecil 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , 0.001, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  -0.003,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.058, NA))+ #Walsh et al 2017 - Charles 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , 0.056, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  0.107,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.078, NA))+ #Walsh et al 2017 - Dorchester 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.008, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   0.013,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.096, NA))+ #Walsh et al 2017 - Harford 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.001, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   -0.012,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.142, NA))+ #Walsh et al 2017 - Kent 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.008, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   -0.002,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.062, NA))+ #Walsh et al 2017 - Prince George's 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.001, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   -0.022,NA))+
  
  geom_function(fun =~ if_else(.x < 300, -0.017, NA))+ #Walsh et al 2017 - Queen Anne's 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.060, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   0.068,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.091, NA))+ #Walsh et al 2017 - Somerset 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.055, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.141,NA))+
  
  geom_function(fun =~ if_else(.x < 300, -0.014, NA))+ #Walsh et al 2017 - St Mary's 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.015, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.017,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.156, NA))+ #Walsh et al 2017 - Talbot 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.014, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.031,NA))+
  
  geom_function(fun =~ if_else(.x < 300, -0.046, NA))+ #Walsh et al 2017 - Wicomico 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.015, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.010,NA)) +
  
  
  geom_function(fun =~ if_else(.x < 300, 0.0585*1.45/0.759, NA))+ #Walsh et al 2017 - Anne Arundel 1 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0249*1.45/0.759, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.0089*1.45/0.759,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.0293*1.45/0.472, NA))+ #Walsh et al 2017 - Baltimore county 1 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0032*1.45/0.472, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.0060*1.45/0.472,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.0088*1.45/0.929, NA))+ #Walsh et al 2017 - Calvert 1 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0174*1.45/0.929, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0196*1.45/0.929,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, -0.0024*1.45/0.472, NA))+ #Walsh et al 2017 - Cecil 1 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0086*1.45/0.472, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0012*1.45/0.472,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.041*1.45/0.557, NA))+ #Walsh et al 2017 - Charles 1 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0252*1.45/0.557, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.0335*1.45/0.557,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.0557*1.45/0.728, NA))+ #Walsh et al 2017 - Dorchester 1 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0076*1.45/0.728, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.0079*1.45/0.728,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.0243*1.45/0.379, NA))+ #Walsh et al 2017 - Harford 1 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0022*1.45/0.379, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.0022*1.45/0.379,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.0289*1.45/0.406, NA))+ #Walsh et al 2017 - Kent 1 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0120*1.45/0.406, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0049*1.45/0.406,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.0093*1.45/0.470, NA))+ #Walsh et al 2017 - Prince George's 1 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0018*1.45/0.470, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0023*1.45/0.470,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,  0.0151*1.45/0.783, NA))+ #Walsh et al 2017 - Queen Anne's 1 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.041*1.45/0.783, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0470*1.45/0.783,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,  0.0300*1.45/0.683, NA))+ #Walsh et al 2017 - Somerset 1 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0207*1.45/0.683, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0498*1.45/0.683,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,  -0.0375*1.45/0.833, NA))+ #Walsh et al 2017 - St Mary's 1 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0082*1.45/0.833, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , -0.0115*1.45/0.833,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,  0.0631*1.45/1.02, NA))+ #Walsh et al 2017 - St Talbot 1 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0122*1.45/1.02, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0190*1.45/1.02,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,  0.0018*1.45/0.399, NA))+ #Walsh et al 2017 - Wicomico 1 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0130*1.45/0.399, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0116*1.45/0.399,NA)) +
  
  
  
  geom_function(fun =~ if_else(.x < 300, 0.3058, NA))+ #Walsh et al 2017 - Anne Arundel 3 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , 0.1020, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0123,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.05560, NA))+ #Walsh et al 2017 - Baltimore county 3 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , -0.0386, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  0.0077,NA))+
  
  geom_function(fun =~ if_else(.x < 300, -0.0134, NA))+ #Walsh et al 2017 - Calvert 3 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , -0.0779, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  -0.0653,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.0010, NA))+ #Walsh et al 2017 - Cecil 3 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , -0.1257, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  -0.0362,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.6413, NA))+ #Walsh et al 2017 - Charles 3 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , 0.1764, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  0.3021,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.0607, NA))+ #Walsh et al 2017 - Dorchester 3 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0429, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0053,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.2600, NA))+ #Walsh et al 2017 - Harford 3 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0213, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   -0.0370,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.0745, NA))+ #Walsh et al 2017 - Kent 3 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.1147, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   -0.1083,NA))+
  
  geom_function(fun =~ if_else(.x < 300, -0.0090, NA))+ #Walsh et al 2017 - Prince George's 3 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.1411, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   -0.1427,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.1310, NA))+ #Walsh et al 2017 - Queen Anne's 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.1838, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.1983,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.0839, NA))+ #Walsh et al 2017 - Somerset 3 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0632, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.1635,NA))+
  
  geom_function(fun =~ if_else(.x < 300, -0.1265, NA))+ #Walsh et al 2017 - St Mary's 3 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0855, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.1324,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.0793, NA))+ #Walsh et al 2017 - Talbot 3 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.1082, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , -0.0984,NA))+
  
  geom_function(fun =~ if_else(.x < 300, 0.0751, NA))+ #Walsh et al 2017 - Wicomico 1 Year Average - Log
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0869, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0878,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.1660*1.45/0.759, NA))+ #Walsh et al 2017 - Anne Arundel 3 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0586*1.45/0.759, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.0103*1.45/0.759,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.0191*1.45/0.472, NA))+ #Walsh et al 2017 - Baltimore county 3 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0117*1.45/0.472, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.0015*1.45/0.472,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.0133*1.45/0.929, NA))+ #Walsh et al 2017 - Calvert 3 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0247*1.45/0.929, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0237*1.45/0.929,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.0023*1.45/0.472, NA))+ #Walsh et al 2017 - Cecil 3 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0329*1.45/0.472, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0128*1.45/0.472,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.2421*1.45/0.557, NA))+ #Walsh et al 2017 - Charles 3 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , 0.0670*1.45/0.557, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.1037*1.45/0.557,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.0309*1.45/0.728, NA))+ #Walsh et al 2017 - Dorchester 3 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0284*1.45/0.728, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0040*1.45/0.728,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.0760*1.45/0.379, NA))+ #Walsh et al 2017 - Harford 3 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0066*1.45/0.379, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0109*1.45/0.379,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, 0.0277*1.45/0.406, NA))+ #Walsh et al 2017 - Kent 3 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0349*1.45/0.406, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0306*1.45/0.406,NA)) +
  
  geom_function(fun =~ if_else(.x < 300, -0.0227*1.45/0.470, NA))+ #Walsh et al 2017 - Prince George's 3 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0399*1.45/0.470, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0439*1.45/0.470,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,  0.0402*1.45/0.783, NA))+ #Walsh et al 2017 - Queen Anne's 3 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , 0.0633*1.45/0.783, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0664*1.45/0.783,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,  0.0547*1.45/0.683, NA))+ #Walsh et al 2017 - Somerset 3 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0499*1.45/0.683, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0761*1.45/0.683,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,  -0.0839*1.45/0.833, NA))+ #Walsh et al 2017 - St Mary's 3 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0476*1.45/0.833, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , -0.0665*1.45/0.833,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,  0.0473*1.45/1.02, NA))+ #Walsh et al 2017 - St Talbot 3 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0149*1.45/1.02, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0226*1.45/1.02,NA)) +
  
  geom_function(fun =~ if_else(.x < 300,  0.0053*1.45/0.399, NA))+ #Walsh et al 2017 - Wicomico 3 Year Average
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0187*1.45/0.399, NA))+ 
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0190*1.45/0.399,NA)) +
  
  geom_function(fun =~ if_else(.x < 500, 2704.7*0.00030,NA))+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x < 500, 1750.7*0.00050,NA))+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x < 500, 782.4*0.00190,NA))+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x < 500, 3196.1*0.00060,NA))+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x < 500, 2479.9*0.00060,NA))+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x < 500, 1301.7*0.00280,NA))+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x < 500, 1424.9*-0.00010,NA))+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x < 500, 410.4*-0.00500,NA))+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x < 500, 312.7*0.00240,NA))+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x < 500, 1071.3*0.00160,NA))+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x < 500, 2006.9*0.00020,NA))+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x < 500, 4367.5*0.00000,NA))+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x < 500, 1220.4*0.00370,NA))+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x < 500, 282.8*0.00440,NA))+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x < 500, 2357.8*0.00020,NA))+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x < 500, 1115.7*0.00100,NA))+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x < 500, 3038.6*0.00020,NA))+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x < 500, 749.6*-0.00010,NA))+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x < 500, 976.9*-0.00210,NA))+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x < 500, 1376.1*0.00100,NA))+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x < 500, 259.8*0.00820,NA))+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x < 500, 38.8*0.09360,NA))+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x < 500, 90.4*0.02720,NA))+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x < 500, 1277.8*0.00000,NA))+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x < 500, 1327.7*-0.00020,NA))+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00090,NA))+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00480,NA))+ # Wolf et al 2022 - Model 27
  
  geom_function(fun =~ if_else(.x < 500, 2704.7*0.00030,NA))+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x < 500, 1750.7*0.00060,NA))+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x < 500, 782.4*0.00180,NA))+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x < 500, 3196.1*0.00070,NA))+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x < 500, 2479.9*0.00070,NA))+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x < 500, 1301.7*0.00290,NA))+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x < 500, 1424.9*-0.00020,NA))+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x < 500, 410.4*-0.00570,NA))+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x < 500, 312.7*0.00200,NA))+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x < 500, 1071.3*0.00100,NA))+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x < 500, 2006.9*0.00040,NA))+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x < 500, 4367.5*0.00000,NA))+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x < 500, 1220.4*0.00370,NA))+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x < 500, 282.8*0.00520,NA))+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x < 500, 2357.8*0.00030,NA))+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x < 500, 1115.7*0.00140,NA))+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x < 500, 3038.6*0.00030,NA))+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x < 500, 749.6*0.00050,NA))+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x < 500, 976.9*0.00210,NA))+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x < 500, 1376.1*0.00090,NA))+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x < 500, 259.8*0.00940,NA))+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x < 500, 38.8*0.09800,NA))+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x < 500, 90.4*0.02600,NA))+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x < 500, 1277.8*0.00010,NA))+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x < 500, 1327.7*-0.00010,NA))+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00090,NA))+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00350,NA))+ # Wolf et al 2022 - Model 27
  
  geom_function(fun =~ if_else(.x < 500, 2704.7*0.00040,NA))+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x < 500, 1750.7*0.00030,NA))+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x < 500, 782.4*0.00120,NA))+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x < 500, 3196.1*0.00040,NA))+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x < 500, 2479.9*0.00070,NA))+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x < 500, 1301.7*0.00540,NA))+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x < 500, 1424.9*-0.00010,NA))+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x < 500, 410.4*-0.01180,NA))+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x < 500, 312.7*0.00040,NA))+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x < 500, 1071.3*-0.00010,NA))+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x < 500, 2006.9*0.00040,NA))+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x < 500, 4367.5*-0.00010,NA))+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x < 500, 1220.4*0.00510,NA))+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x < 500, 282.8*0.00340,NA))+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x < 500, 2357.8*0.00010,NA))+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x < 500, 1115.7*0.00180,NA))+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x < 500, 3038.6*0.00070,NA))+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x < 500, 749.6*-0.00010,NA))+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x < 500, 976.9*0.00210,NA))+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x < 500, 1376.1*0.00090,NA))+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x < 500, 259.8*0.00850,NA))+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x < 500, 38.8*0.08330,NA))+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x < 500, 90.4*0.03240,NA))+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x < 500, 1277.8*0.00000,NA))+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x < 500, 1327.7*-0.00010,NA))+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00150,NA))+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00590,NA))+ # Wolf et al 2022 - Model 27
  
  geom_function(fun =~ if_else(.x < 500, 2704.7*0.00010,NA))+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x < 500, 1750.7*0.00060,NA))+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x < 500, 782.4*0.00190,NA))+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x < 500, 3196.1*0.00050,NA))+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x < 500, 2479.9*0.00060,NA))+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x < 500, 1301.7*0.00280,NA))+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x < 500, 1424.9*0.00000,NA))+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x < 500, 410.4*-0.00630,NA))+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x < 500, 312.7*0.00180,NA))+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x < 500, 1071.3*0.00160,NA))+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x < 500, 2006.9*0.00030,NA))+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x < 500, 4367.5*-0.00010,NA))+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x < 500, 1220.4*0.00030,NA))+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x < 500, 282.8*0.00470,NA))+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x < 500, 2357.8*0.00010,NA))+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x < 500, 1115.7*0.00120,NA))+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x < 500, 3038.6*0.00010,NA))+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x < 500, 749.6*0.00000,NA))+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x < 500, 976.9*0.00210,NA))+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x < 500, 1376.1*0.00090,NA))+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x < 500, 259.8*0.00850,NA))+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x < 500, 38.8*0.08330,NA))+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x < 500, 90.4*0.03240,NA))+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x < 500, 1277.8*0.00000,NA))+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x < 500, 1327.7*-0.00060,NA))+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00050,NA))+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00420,NA)) # Wolf et al 2022 - Model 27
  
  ggtitle("Hedonic function with distance dummies")+
  xlab("Distance (Meters)") + ylab("Elasticity")              
 
  
base_dummy  +    geom_function(fun =~ if_else(.x < 250, 0.063, NA))+# Mamun et al 2023 (Model 3 - discrete distance bins/Table S13:state:WV)
  geom_function(fun =~ if_else(.x > 250 & .x < 500 , 0.056,NA))





