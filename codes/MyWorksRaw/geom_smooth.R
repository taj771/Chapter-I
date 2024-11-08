
# clear memory
rm(list = ls())

# library

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,
  tidyverse,
  ggplot2,
  tidyr
)





# Withould distance involvement 

base <-
  ggplot() +
  xlim(0, 2000)
x=0.1

p1 <- base + geom_function(fun =~ if_else(.x <= 300, 0.009,NA), size = x) + # Ara 2007-IH Cluster 5 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.172,NA), size = x) + # Ara 2007 - IH Cluster 6 - OLS
  geom_function(fun =~ if_else(.x <= 500, -0.033,NA), size = x) + # Ara 2007 - IH Cluster 7 - OLS
  geom_function(fun =~ if_else(.x <= 500, -0.033,NA), size = x) + # Ara 2007 - IH Cluster 11 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.012,NA), size = x) + # Ara 2007 - IH Cluster 5 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.163,NA), size = x) + # Ara 2007 - IH Cluster 6 - GMM
  geom_function(fun =~ if_else(.x <= 500, -0.007,NA), size = x) + # Ara 2007 - IH Cluster 7 - GMM
  geom_function(fun =~ if_else(.x <= 500, -0.029,NA), size = x) + # Ara 2007 - IH Cluster 11 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.204,NA), size = x) + # Ara 2007 - CBG Cluster 2 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.029,NA), size = x) + # Ara 2007 - CBG Cluster 3 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.029,NA), size = x) + # Ara 2007 - CBG Cluster 3 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.12,NA), size = x) + # Ara 2007 - CBG Cluster 4 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.037,NA), size = x) + # Ara 2007 - CBG Cluster 7 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.012,NA), size = x) + # Ara 2007 - CBG Cluster 9 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.203,NA), size = x) + # Ara 2007 - CBG Cluster 2 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.026,NA), size = x) + # Ara 2007 - CBG Cluster 3 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.056,NA), size = x) + # Ara 2007 - CBG Cluster 4 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.052,NA), size = x) + # Ara 2007 - CBG Cluster 7 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.016,NA), size = x) + # Ara 2007 - CBG Cluster 9 - GMM
  geom_function(fun =~ if_else(.x <= 500, 4.369999886*3515/102746,NA), size = x)+ # Boyle and Taylor 2001 - G1-Town Data
  geom_function(fun =~ if_else(.x <= 500, 3.920000076*3515/102746,NA), size = x)+ # Boyle and Taylor 2001 - G1-Survey Data
  geom_function(fun =~ if_else(.x <= 500, 2.099999905*3515/85197,NA), size = x)+ # Boyle and Taylor 2001 - G2-Town Data
  geom_function(fun =~ if_else(.x <= 500, 1.389999986*3515/85197,NA), size = x)+ # Boyle and Taylor 2001 - G2-Survey Data
  geom_function(fun =~ if_else(.x <= 500, 4.730000019*3515/32779,NA), size = x)+ # Boyle and Taylor 2001 - G3-Town Data
  geom_function(fun =~ if_else(.x <= 500, 6.130000114*3515/32779,NA), size = x)+ # Boyle and Taylor 2001 - G3-Survey Data
  geom_function(fun =~ if_else(.x <= 500, 40.02999878*3515/97482,NA), size = x)+ # Boyle and Taylor 2001 - G4-Town Data
  geom_function(fun =~ if_else(.x <= 500, 36.13999939*3515/97482,NA), size = x)+ # Boyle and Taylor 2001 - G4-Survey Data
  geom_function(fun =~ if_else(.x <= 500, 3514/104069*7.375697136,NA), size = x) + # Boyle et al 1999 -model 1
  geom_function(fun =~ if_else(.x <= 500, 3514/85880*3.167430162,NA), size = x) + # Boyle et al 1999 - model 2
  geom_function(fun =~ if_else(.x <= 500, 3514/73938*3.584641457,NA), size = x) + # Boyle et al 1999 - model 3
  geom_function(fun =~ if_else(.x <= 500, 3514/100350*13.07054615,NA), size = x)+  # Boyle et al 1999 - model 4
  geom_function(fun =~ if_else(.x <= 500, 178.3*3.83/2239.2,NA), size = x)+  # Calder¢n-Arrieta 2019 - model 1
  geom_function(fun =~ if_else(.x <= 500, 185.9*3.83/2239.2,NA), size = x)+  # Calder¢n-Arrieta 2019 - model 2
  geom_function(fun =~ if_else(.x <= 500, 43520.43*3.851/648415.8,NA), size = x)+  # Clapper & Caudill 2014 - liner - sale price
  geom_function(fun =~ if_else(.x <= 500, 0.065*3.851,NA), size = x)+  # Clapper & Caudill 2014 - log-liner - sale price
  geom_function(fun =~ if_else(.x <= 500, 0.269,NA), size = x)+  # Clapper & Caudill 2014 - log-log - sale price
  geom_function(fun =~ if_else(.x <= 500, 30.988*3.851/443.566,NA), size = x)+  # Clapper & Caudill 2014 - liner - sale price per square foot
  geom_function(fun =~ if_else(.x <= 500, 0.064*3.851,NA), size = x)+  # Clapper & Caudill 2014 - log-liner - sale price per square foot
  geom_function(fun =~ if_else(.x <= 500, 0.268,NA), size = x)+  # Clapper & Caudill 2014 - log-log - sale price per square foot
  geom_function(fun =~ if_else(.x <= 500, 4.48059988*1235.77002/138763.05,NA), size = x)+  # Gibbs et al 2002 - Conway/Milton
  geom_function(fun =~ if_else(.x <= 500, 17.34000015*1879.099976/175157.73,NA), size = x)+  # Gibbs et al 2002 - Winnipesaukee
  geom_function(fun =~ if_else(.x <= 500, 76.76999664*213.5800018/132162.84,NA), size = x)+  # Gibbs et al 2002 - Derry/Amherst
  geom_function(fun =~ if_else(.x <= 500, 149.6000061*283.6900024/167104.7,NA), size = x)+  # Gibbs et al 2002 - Spofford/Greenfield
  geom_function(fun =~ if_else(.x <= 500, 13893.84961*3.28*0.926591992/268034.57,NA), size = x)+  # Horsch & Lewis 2009 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 14355.2998*3.28*0.926591992/268034.57,NA), size = x)+  # Horsch & Lewis 2009 - Model 2
  geom_function(fun =~ if_else(.x <= 500, 13367.92969*3.28*0.926591992/268034.57,NA), size = x)+  # Horsch & Lewis 2009 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 7072.709961*3.28*0.926591992/268034.57,NA), size = x)+  # Horsch & Lewis 2009 - Random Effects with Year Dummies
  geom_function(fun =~ if_else(.x <= 500, 6443.779785*3.28*0.926591992/268034.57,NA), size = x)+  # Horsch & Lewis 2009 - Random Effects with Year Trend Variable
  geom_function(fun =~ if_else(.x <= 500, 8.75*866/65363.49,NA), size = x)+  # Hsu 2000 - Northeast Kingdom
  geom_function(fun =~ if_else(.x <= 500, 12.84000015*2021/120063.58,NA), size = x)+  # Hsu 2000 - Lake Champlain
  geom_function(fun =~ if_else(.x <= 500, 10.48999977*1552/104471.03,NA), size = x)+  # Hsu 2000 - Rutland County
  geom_function(fun =~ if_else(.x <= 500, 10.60999966*1581/105410.9,NA), size = x)+  # Hsu 2000 - Milfoil
  geom_function(fun =~ if_else(.x <= 500, 13.32999992*1581/105410.9,NA), size = x)+  # Hsu 2000 - exp(milfoil)
  geom_function(fun =~ if_else(.x <= 500, 44.31000137*1581/105410.9,NA), size = x)+  # Hsu 2000 - Total Weed
  geom_function(fun =~ if_else(.x <= 500, 22.92000008*1581/105410.9,NA), size = x)+  # Hsu 2000 - exp(Total Weed)
  geom_function(fun =~ if_else(.x <= 2000, 0.059,NA), size = x)+  # Irwin & Wolf 2022 - Model 1
  geom_function(fun =~ if_else(.x <= 2000, 0.093,NA), size = x)+  # Irwin & Wolf 2022 - Model 2
  geom_function(fun =~ if_else(.x <= 2000, 0.103,NA), size = x)+  # Irwin & Wolf 2022 - Model 3
  geom_function(fun =~ if_else(.x <= 2000, 0.086,NA), size = x)+  # Irwin & Wolf 2022 - Model 4
  geom_function(fun =~ if_else(.x <= 2000, 0.079,NA), size = x)+  # Irwin & Wolf 2022 - Model 6
  geom_function(fun =~ if_else(.x <= 500, 5207*3.28*3.627120018/184892,NA), size = x)+  # Kashian et al 2006 - Hedonic Equation (3)
  geom_function(fun =~ if_else(.x <= 500, 46459.33/(218562*3.28),NA), size = x)+  # Kemp et al 2017 - Model 1 (SD)
  geom_function(fun =~ if_else(.x <= 500, 45292.89/(238142*3.28),NA), size = x)+  # Kemp et al 2017 - Model 2 (SD)
  geom_function(fun =~ if_else(.x <= 500, 66046.22/(175331*3.28),NA), size = x)+  # Kemp et al 2017 - Model 3 (SD)
  geom_function(fun =~ if_else(.x <= 500, 13.22999954*702.6099854/100313,NA), size = x)+  # Krysel et al 2003  - Aitkin Lake Group
  geom_function(fun =~ if_else(.x <= 500, 4.71999979*985.7600098/176461,NA), size = x)+  # Krysel et al 2003 - Brainerd Lake Group
  geom_function(fun =~ if_else(.x <= 500, 1.100000024*852.2600098/135905,NA), size = x)+  # Krysel et al 2003 - Grand Rapids Lake Group
  geom_function(fun =~ if_else(.x <= 500, 2.150000095*1031.869995/179621,NA), size = x)+  # Krysel et al 2003 - Walker Lake Group
  geom_function(fun =~ if_else(.x <= 500, 21.75*882.9099731/124390,NA), size = x)+  # Krysel et al 2003 - Park Rapids Lake Group
  geom_function(fun =~ if_else(.x <= 500, 7.309999943*702.6099854/100313,NA), size = x)+  # Krysel et al 2003 - MN - Aitkin Lake Group
  geom_function(fun =~ if_else(.x <= 500, 1.940000057*985.7600098/176461,NA), size = x)+  # Krysel et al 2003 - MN - Brainerd Lake Group
  geom_function(fun =~ if_else(.x <= 500, 1.730000019*852.2600098/135905,NA), size = x)+  # Krysel et al 2003 - MN - Grand Rapids Lake Group
  geom_function(fun =~ if_else(.x <= 500, 1.909999967*1031.869995/179621,NA), size = x)+  # Krysel et al 2003 - MN - Walker Lake Group
  geom_function(fun =~ if_else(.x <= 500, 19.95000076*882.9099731/124390,NA), size = x)+  # Krysel et al 2003 - MN - Park Rapids Lake Group
  geom_function(fun =~ if_else(.x <= 500, 9.720000267*1101.5/142829,NA), size = x)+  # Krysel et al 2003 - MN - Bemidji Lake Group
  geom_function(fun =~ if_else(.x <= 500, 0.275000006,NA), size = x)+  # Liao et al 2016 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 0.231999993,NA), size = x)+  # Liao et al 2016 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 0.219999999,NA), size = x)+  # Liao et al 2016 - Spatial Regime Model 1
  geom_function(fun =~ if_else(.x <= 500, 0.165000007,NA), size = x)+  # Liao et al 2016 - Spatial Regime Model 1
  geom_function(fun =~ if_else(.x <= 500, 0.223000005,NA), size = x)+  # Liao et al 2016 - Spatial Regime Model 3
  geom_function(fun =~ if_else(.x <= 500, 0.165999994,NA), size = x)+  # Liao et al 2016 - Spatial Regime Model 3
  geom_function(fun =~ if_else(.x <= 2000, 0.30340001*0.709999979,NA), size = x)+  # Liu et al 2014 - Sale Price - semi-log
  geom_function(fun =~ if_else(.x <= 2000, -0.0145, NA), size = x) +  # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:1
  geom_function(fun =~ if_else(.x <= 2000, 0.024, NA), size = x) + # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:2
  geom_function(fun =~ if_else(.x <= 2000, -0.0094, NA), size = x) + # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:3
  geom_function(fun =~ if_else(.x <= 2000, 0.028, NA), size = x) + # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:4
  geom_function(fun =~ if_else(.x <= 500, 0.413004667, NA), size = x) + # Michael et al 2000 - Model 1 - Group 1 - CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.093727589, NA), size = x) + # Michael et al 2000 - Model 1 - Group 2 -  CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.491040945, NA), size = x) + # Michael et al 2000 - Model 1 - Group 3 -  CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.285336018, NA), size = x) + # Michael et al 2000 - Model 2 - Group 1 -  PMIN 
  geom_function(fun =~ if_else(.x <= 500, 0.10351032, NA), size = x) + # Michael et al 2000 - Model 2 - Group 2 -  PMIN  
  geom_function(fun =~ if_else(.x <= 500, 0.664476693, NA), size = x) + # Michael et al 2000 - Model 2 - Group 3 - PMIN 
  geom_function(fun =~ if_else(.x <= 500, 0.376048774, NA), size = x) + # Michael et al 2000 - Model 3 - Group 1 - HMIN   
  geom_function(fun =~ if_else(.x <= 500, 0.138936102, NA), size = x) + # Michael et al 2000 - Model 3 - Group 2 - HMIN 
  geom_function(fun =~ if_else(.x <= 500, 0.916609764, NA), size = x) + # Michael et al 2000 -Model 3 - Group 3 - HMIN  
  geom_function(fun =~ if_else(.x <= 500, 0.385807753, NA), size = x) + # Michael et al 2000 -Model 4 - Group 1 - CMIN * HMIN 
  geom_function(fun =~ if_else(.x <= 500, 0.079734102, NA), size = x) + # Michael et al 2000 -Model 4 - Group 2 - CMIN * HMIN  
  geom_function(fun =~ if_else(.x <= 500, 0.431535065, NA), size = x) + # Michael et al 2000 - Model 4 - Group 3 - CMIN * HMIN 
  geom_function(fun =~ if_else(.x <= 500, -0.062146027, NA), size = x) + # Michael et al 2000 - Model 5 - Group 1 - CMIN * HMIN +/-  
  geom_function(fun =~ if_else(.x <= 500, 0.015297231, NA), size = x) + # Michael et al 2000 - Model 5 - Group 2 - CMIN * HMIN +/-  
  geom_function(fun =~ if_else(.x <= 500, -0.054691911, NA), size = x) + # Michael et al 2000 - Model 5 - Group 3 - CMIN * HMIN +/-  
  geom_function(fun =~ if_else(.x <= 500, 0.040301826, NA), size = x) + # Michael et al 2000 - Model 6 - Group 1 - CMAX/CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.004697338, NA), size = x) + # Michael et al 2000 - Model 6 - Group 2 - CMAX/CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.193003386, NA), size = x) + # Michael et al 2000 - Model 6 - Group 3 - CMAX/CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.432574719, NA), size = x) + # Michael et al 2000 - Model 7 - Group 1 - CMAX/CMIN%
  geom_function(fun =~ if_else(.x <= 500, 0.104469016, NA), size = x) + # Michael et al 2000 - Model 7 - Group 2 - CMAX/CMIN%
  geom_function(fun =~ if_else(.x <= 500, 0.542586148, NA), size = x) + # Michael et al 2000 - Model 7 - Group 3 - CMAX/CMIN%
  geom_function(fun =~ if_else(.x <= 500, 0.452805698, NA), size = x) + # Michael et al 2000 - Model 8 - Group 1 - CMIN-HMIN
  geom_function(fun =~ if_else(.x <= 500, 0.114391185, NA), size = x) + # Michael et al 2000 - Model 8 - Group 2 - CMIN-HMIN
  geom_function(fun =~ if_else(.x <= 500, 0.589391351, NA), size = x) + # Michael et al 2000 - Model 8 - Group 3 - CMIN-HMIN
  geom_function(fun =~ if_else(.x <= 500, 0.325085133, NA), size = x) + # Michael et al 2000 - Model 9 - Group 1 - HMIN+ and HMIN -
  geom_function(fun =~ if_else(.x <= 500, 0.098492384, NA), size = x) + # Michael et al 2000 - Model 9 - Group 2 - HMIN+ and HMIN -
  geom_function(fun =~ if_else(.x <= 500, 0.401052326, NA), size = x) + # Michael et al 2000 - Model 9 - Group 3 - HMIN+ and HMIN -
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.151, NA), size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.154, NA), size = x) + # Moore et al 2020 - IV-GMM - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.114, NA), size = x) + # Moore et al 2020 - OLS - 2a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.117, NA), size = x) + # Moore et al 2020 - IV-GMM - 2b
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.121, NA), size = x) + # Moore et al 2020 - OLS - 3a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.126, NA), size = x) + # Moore et al 2020 - IV-GMM - 3b
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.099, NA), size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.101, NA), size = x) + # Moore et al 2020 - IV-GMM - 4b
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.151, NA),size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.151, NA),size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.151, NA),size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.096, NA),size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.094, NA),size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.098, NA),size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.153 + -0.118*1 + -0.068*0 + -0.017*0), NA),size = x) + # Moore et al 2020 - OLS - 1a - Florida
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.153 + -0.118*0 + -0.068*1 + -0.017*0), NA),size = x) + # Moore et al 2020 - OLS - 1a - Indiana
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.153 + -0.118*0 + -0.068*0 + -0.017*1), NA),size = x) + # Moore et al 2020 - OLS - 1a - Washington
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.128 + 0.407*1 + -0.029*0 + -0.02*0), NA),size = x) + # Moore et al 2020 - OLS - 2a - Florida
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.128 + 0.407*0 + -0.029*1 + -0.02*0), NA),size = x) + # Moore et al 2020 - OLS - 2a - Indiana
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.128 + 0.407*0 + -0.029*0 + -0.02*1), NA),size = x) + # Moore et al 2020 - OLS - 2a - Washington
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.138 + 0.358*1 + -0.041*0 + -0.03*0), NA),size = x) + # Moore et al 2020 - OLS - 3a - Florida
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.138 + 0.358*0 + -0.041*1 + -0.03*0), NA),size = x) + # Moore et al 2020 - OLS - 3a - Indiana
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.138 + 0.358*0 + -0.041*0 + -0.03*1), NA),size = x) + # Moore et al 2020 - OLS - 3a - Washington
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.111 + 0.251*1 + -0.072*0 + -0.007*0), NA),size = x) + # Moore et al 2020 - OLS - 3a - Florida
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.111 + 0.251*0 + -0.072*1 + -0.007*0), NA),size = x) + # Moore et al 2020 - OLS - 3a - Indiana
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.111 + 0.251*0 + -0.072*0 + -0.007*1), NA),size = x) + # Moore et al 2020 - OLS - 3a - Washington
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.21, NA),size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.09, NA),size = x) + # Moore et al 2020 - OLS - 2a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.103, NA),size = x) + # Moore et al 2020 - OLS - 3a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.085, NA),size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.186, NA),size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.07, NA),size = x) + # Moore et al 2020 - OLS - 2a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.084, NA),size = x) + # Moore et al 2020 - OLS - 3a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.089, NA),size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 500, -7430.29*3.4/502312.8, NA),size = x) + # Olden & Tamayo 2014 - Model 1
  geom_function(fun =~ if_else(.x <= 500, -6962.7*3.4/502312.8, NA),size = x) + # Olden & Tamayo 2014 - Model 2
  geom_function(fun =~ if_else(.x <= 500, -6954.8*3.4/502312.8, NA),size = x) + # Olden & Tamayo 2014 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 10.36999989*3209/103853, NA),size = x) + # Poor et al 2001 - Lewiston
  geom_function(fun =~ if_else(.x <= 500, 2.25*4391/86880, NA),size = x) + # Poor et al 2001 - Augusta
  geom_function(fun =~ if_else(.x <= 500, 1.46*2517/67881, NA),size = x) + # Poor et al 2001 - Bangor
  geom_function(fun =~ if_else(.x <= 500, -0.88*5161/31287, NA),size = x) + # Poor et al 2001 - Northern Maine
  geom_function(fun =~ if_else(.x <= 500, 0.17+6.80, NA),size = x) + # Swedberg el al 2020 - Multi-state model- ME ###Outlier
  geom_function(fun =~ if_else(.x <= 500, 0.17+ -1.04, NA),size = x) + # Swedberg el al 2020 - Multi-state model- MI
  geom_function(fun =~ if_else(.x <= 500, 0.17+ 0.08, NA),size = x) + # Swedberg el al 2020 - Multi-state model-  NY
  geom_function(fun =~ if_else(.x <= 500, 0.17+ 0.14, NA),size = x) + # Swedberg el al 2020 - Multi-state model-  VT
  geom_function(fun =~ if_else(.x <= 500, 0.17+ -0.11, NA),size = x) + # Swedberg el al 2020 - Multi-state model-   WI
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - Multi-state model
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop ME
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop MI
  geom_function(fun =~ if_else(.x <= 500, 0.13, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop MN
  geom_function(fun =~ if_else(.x <= 500, 0.16, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop NY
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop VT
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop WI
  geom_function(fun =~ if_else(.x <= 500, -2.71, NA),size = x) + # Swedberg el al 2020 - State level model - Maine
  geom_function(fun =~ if_else(.x <= 500, 9.20, NA),size = x) + # Swedberg el al 2020 - State level model - Michigan
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - State level model - Minnesota
  geom_function(fun =~ if_else(.x <= 500, 0.13, NA),size = x) + # Swedberg el al 2020 - State level model - New York
  geom_function(fun =~ if_else(.x <= 500, -0.32, NA),size = x) + # Swedberg el al 2020 - State level model - Vermont
  geom_function(fun =~ if_else(.x <= 500, 0.1, NA),size = x) + # Swedberg el al 2020 - State level model - Wisconsin
  geom_function(fun =~ if_else(.x <= 500, 0.5, NA),size = x) + # Swedberg el al 2020 - Regional model - Otter Tail
  geom_function(fun =~ if_else(.x <= 500, 0.1, NA),size = x) + # Swedberg el al 2020 - Regional model - Twin Cities
  geom_function(fun =~ if_else(.x <= 500, -0.21, NA),size = x) + # Swedberg el al 2020 - Regional model - Adirondacks
  geom_function(fun =~ if_else(.x <= 500, 0.41, NA),size = x) + # Swedberg el al 2020 - Regional model - Regional model - Finger Lakes
  geom_function(fun =~ if_else(.x <= 500, 0.129, NA),size = x) + # Weng et al 2020 - Secci depth
  geom_function(fun =~ if_else(.x <= 139, 0.0352 + 0.0755 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 1
  geom_function(fun =~ if_else(.x <= 139, 0.0136 + 0.0543 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 1
  geom_function(fun =~ if_else(.x <= 139, 0.0548 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 2
  geom_function(fun =~ if_else(.x <= 139, 0.0291 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 2
  geom_function(fun =~ if_else(.x <= 139, 0.0092 + 0.0498 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 3
  geom_function(fun =~ if_else(.x <= 139, -0.0205 + 0.0444 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 3
  geom_function(fun =~ if_else(.x <= 139, -0.0147 + 0.0447 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 4
  geom_function(fun =~ if_else(.x <= 139, -0.0242 + 0.0421 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 4
  geom_function(fun =~ if_else(.x <= 139, -0.0086 + 0.0432 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 5
  geom_function(fun =~ if_else(.x <= 139, 0.022 + 0.0407 * log(2.39), NA),size = x)+ # Wolf & Kemp 2021 - Model 5
  geom_function(fun =~ if_else(.x <= 500, -0.647772014, NA),size = x) + # Zhang & Boyle 2010 - Milfoil - Quadratic
  geom_function(fun =~ if_else(.x <= 500, -0.647772014, NA),size = x) + # Zhang & Boyle 2010 - Milfoil - Exponential
  geom_function(fun =~ if_else(.x <= 500, -0.323886007, NA),size = x) + # Zhang & Boyle 2010 - Total macrophytes - Quadratic
  geom_function(fun =~ if_else(.x <= 500, -0.323886007, NA),size = x) + # Zhang & Boyle 2010 - Total macrophytes - Exponential
  geom_function(fun =~ if_else(.x <= 500, -0.161943004, NA),size = x) + # Zhang & Boyle 2010 - Best Model - Quadratic
  geom_function(fun =~ if_else(.x <= 500, -0.323886007, NA),size = x) + # Zhang & Boyle 2010 - Best Model - Exponential
  geom_function(fun =~ if_else(.x <= 500, 0.18130137, NA),size = x) + # Zhang et al 2015 - VT1
  geom_function(fun =~ if_else(.x <= 500, 0.168578461, NA),size = x) + # Zhang et al 2015 - VT2
  geom_function(fun =~ if_else(.x <= 500, 0.031807259, NA),size = x) + # Zhang et al 2015 - VT3
  geom_function(fun =~ if_else(.x <= 500, 0.368520737, NA),size = x) + # Zhang et al 2015 - ME1
  geom_function(fun =~ if_else(.x <= 500, -0.044222489, NA),size = x) + # Zhang et al 2015 - ME2
  geom_function(fun =~ if_else(.x <= 500, 0.078617759, NA),size = x) + # Zhang et al 2015 - ME3
  geom_function(fun =~ if_else(.x <= 500, 0.560151517, NA),size = x) + # Zhang et al 2015 - ME4
  geom_function(fun =~ if_else(.x <= 500, -0.00491361, NA),size = x) + # Zhang et al 2015 - ME5
  geom_function(fun =~ if_else(.x <= 500, -0.01965444, NA),size = x) + # Zhang et al 2015 - ME6
  geom_function(fun =~ if_else(.x <= 500, 1.719763517, NA),size = x) + # Zhang et al 2015 - ME7
  geom_function(fun =~ if_else(.x <= 500, 0.035835754, NA),size = x) + # Zhang et al 2015 - NH1
  geom_function(fun =~ if_else(.x <= 500, 0.04596325, NA),size = x) + # Zhang et al 2015 - NH2
  geom_function(fun =~ if_else(.x <= 500, 1.505880713, NA),size = x) + # Zhang et al 2015 - NH3
  geom_function(fun =~ if_else(.x <= 500, 0.971460581, NA),size = x) + # Zhang et al 2015 - NH4
  geom_function(fun =~ if_else(.x <= 500, 0.185411081, NA),size = x) + # Zhang et al 2015 - NH5
  ggtitle("Hedonic function without distance involvement")+
  xlab("Distance to waterbody (meters)") + ylab("Elasticity")



p1

p_build_1 = ggplot_build(p1)


plot_data_1 <-  as.data.frame(p_build_1$data[1:208])



df <- plot_data_1%>%
  select(matches('x|y'))%>%
  rename("dist" = "x")%>%
  select(!matches('linetype'))%>%
  select(!matches('x'))


df1 <- df %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "model",
    names_prefix = "e",
    values_to = "elast",
    values_drop_na = TRUE
  )


df1$model[which(df1$model == "y")] = 0

df1$model <- str_remove(df1$model, "y.")

model <- c(0:207)
study_name <- c("Ara 2007-IH Cluster 5 - OLS",
                "Ara 2007-IH Cluster 6 - OLS",
                "Ara 2007-IH Cluster 7 - OLS",
                "Ara 2007-IH Cluster 11 - OLS",
                "Ara 2007-IH Cluster 5 - GMM",
                "Ara 2007-IH Cluster 6 - GMM",
                "Ara 2007-IH Cluster 7 - GMM",
                "Ara 2007-IH Cluster 11 - GMM",
                "Ara 2007-CBG Cluster 2 - OLS",
                "Ara 2007-CBG Cluster 3 - OLS",
                "Ara 2007-CBG Cluster 3 - OLS",
                "Ara 2007-CBG Cluster 4 - OLS",
                "Ara 2007-CBG Cluster 7 - OLS",
                "Ara 2007-CBG Cluster 9 - OLS",
                "Ara 2007-CBG Cluster 2 - GMM",
                "Ara 2007-CBG Cluster 3 - GMM",
                "Ara 2007-CBG Cluster 4 - GMM",
                "Ara 2007-CBG Cluster 7 - GMM",
                "Ara 2007-CBG Cluster 9 - GMM",
                "Boyle and Taylor 2001-G1-Town Data",
                "Boyle and Taylor 2001-G1-Survey Data",
                "Boyle and Taylor 2001-G2-Town Data",
                "Boyle and Taylor 2001-G2-Survey Data",
                "Boyle and Taylor 2001-G3-Town Data",
                "Boyle and Taylor 2001-G3-Survey Data",
                "Boyle and Taylor 2001-G4-Town Data",
                "Boyle and Taylor 2001-G4-Survey Data",
                "Boyle et al 1999-Lewiston/Auburn",
                "Boyle et al 1999-Waterville",
                "Boyle et al 1999-Bangor",
                "Boyle et al 1999-Camden",
                "Calder & Arrieta 2019-Model 1",
                "Calder & Arrieta 2019-Model 2",
                "Clapper & Caudill 2014-liner - sale price",
                "Clapper & Caudill 2014-log-liner - sale price",
                "Clapper & Caudill 2014-log-log - sale price",
                "Clapper & Caudill 2014-liner - sale price per square foot",
                "Clapper & Caudill 2014-log-liner - sale price per square foot",
                "Clapper & Caudill 2014-log-log - sale price per square foot",
                "Gibbs et al 2002-Conway/Milton",
                "Gibbs et al 2002-Winnipesaukee",
                "Gibbs et al 2002-Derry/Amherst",
                "Gibbs et al 2002-Spofford/Greenfield",
                "Horsch & Lewis 2009-Model 1",
                "Horsch & Lewis 2009-Model 2",
                "Horsch & Lewis 2009-Model 3",
                "Horsch & Lewis 2009-Random Effects with Year Dummies",
                "Horsch & Lewis 2009-Random Effects with Year Trend Variable",
                "Hsu 2000-Northeast Kingdom",
                "Hsu 2000-Lake Champlain",
                "Hsu 2000-Rutland County",
                "Hsu 2000-Milfoil",
                "Hsu 2000-exp(milfoil)",
                "Hsu 2000-Total Weed",
                "Hsu 2000-exp(Total Weed)",
                "Irwin & Wolf 2022-Model 1",
                "Irwin & Wolf 2022-Model 2",
                "Irwin & Wolf 2022-Model 3",
                "Irwin & Wolf 2022-Model 4",
                "Irwin & Wolf 2022-Model 6",
                "Kashian et al 2006-Hedonic Equation (3)",
                "Kemp et al 2017-Model 1 (SD)",
                "Kemp et al 2017-Model 2 (SD)",
                "Kemp et al 2017-Model 3 (SD)",
                "Krysel et al 2003-Boyle et al - Aitkin Lake Group",
                "Krysel et al 2003-Boyle et al - Brainerd Lake Group",
                "Krysel et al 2003-Boyle et al - Grand Rapids Lake Group",
                "Krysel et al 2003-Boyle et al - Walker Lake Group",
                "Krysel et al 2003-Boyle et al - Park Rapids Lake Group",
                "Krysel et al 2003-MN - Aitkin Lake Group",
                "Krysel et al 2003-MN - Brainerd Lake Group",
                "Krysel et al 2003-MN - Grand Rapids Lake Group",
                "Krysel et al 2003-MN - Walker Lake Group",
                "Krysel et al 2003-MN - Park Rapids Lake Group",
                "Krysel et al 2003-MN - Bemidji Lake Group",
                "Liao et al 2016-Model 1",
                "Liao et al 2016-Model 3",
                "Liao et al 2016-Spatial Regime Model 1",
                "Liao et al 2016-Spatial Regime Model 1",
                "Liao et al 2016-Spatial Regime Model 3",
                "Liao et al 2016-Spatial Regime Model 3",
                "Liu et al 2014-Sale Price - semi-log",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S6:1",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S6:2",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S6:3",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S6:4",
                "Michael et al 2000-Model 1 - Group 1 - CMIN",
                "Michael et al 2000-Model 1 - Group 2 - CMIN",
                "Michael et al 2000-Model 1 - Group 3 - CMIN",
                "Michael et al 2000-Model 2 - Group 1 - PMIN",
                "Michael et al 2000-Model 2 - Group 2 - PMIN",  
                "Michael et al 2000-Model 2 - Group 3 - PMIN", 
                "Michael et al 2000-Model 3 - Group 1 - HMIN",   
                "Michael et al 2000-Model 3 - Group 2 - HMIN", 
                "Michael et al 2000-Model 3 - Group 3 - HMIN",  
                "Michael et al 2000-Model 4 - Group 1 - CMIN * HMIN", 
                "Michael et al 2000-Model 4 - Group 2 - CMIN * HMIN",  
                "Michael et al 2000-Model 4 - Group 3 - CMIN * HMIN", 
                "Michael et al 2000-Model 5 - Group 1 - CMIN * HMIN +/-",  
                "Michael et al 2000-Model 5 - Group 2 - CMIN * HMIN +/-",  
                "Michael et al 2000-Model 5 - Group 3 - CMIN * HMIN +/-",  
                "Michael et al 2000-Model 6 - Group 1 - CMAX/CMIN",
                "Michael et al 2000-Model 6 - Group 2 - CMAX/CMIN",
                "Michael et al 2000-Model 6 - Group 3 - CMAX/CMIN",
                "Michael et al 2000-Model 7 - Group 1 - CMAX/CMIN%",
                "Michael et al 2000-Model 7 - Group 2 - CMAX/CMIN%",
                "Michael et al 2000-Model 7 - Group 3 - CMAX/CMIN%",
                "Michael et al 2000-Model 8 - Group 1 - CMIN-HMIN",
                "Michael et al 2000-Model 8 - Group 2 - CMIN-HMIN",
                "Michael et al 2000-Model 8 - Group 3 - CMIN-HMIN",
                "Michael et al 2000-Model 9 - Group 1 - HMIN+ and HMIN -",
                "Michael et al 2000-Model 9 - Group 2 - HMIN+ and HMIN -",
                "Michael et al 2000-Model 9 - Group 3 - HMIN+ and HMIN -",
                "Moore et al 2020-OLS - 1a",
                "Moore et al 2020-IV-GMM - 1a",
                "Moore et al 2020-OLS - 2a",
                "Moore et al 2020-IV-GMM - 2b",
                "Moore et al 2020-OLS - 3a",
                "Moore et al 2020-IV-GMM - 3b",
                "Moore et al 2020-OLS - 4a",
                "Moore et al 2020-IV-GMM - 4b",
                "Moore et al 2020-OLS - 1a",
                "Moore et al 2020-OLS - 1a",
                "Moore et al 2020-OLS - 1a",
                "Moore et al 2020-OLS - 4a",
                "Moore et al 2020-OLS - 4a",
                "Moore et al 2020-OLS - 4a",
                "Moore et al 2020-OLS - 1a",
                "Moore et al 2020-OLS - 1a",
                "Moore et al 2020-OLS - 1a",
                "Moore et al 2020-OLS - 2a",
                "Moore et al 2020-OLS - 2a",
                "Moore et al 2020-OLS - 2a",
                "Moore et al 2020-OLS - 3a",
                "Moore et al 2020-OLS - 3a",
                "Moore et al 2020-OLS - 3a",
                "Moore et al 2020-OLS - 3a",
                "Moore et al 2020-OLS - 3a",
                "Moore et al 2020-OLS - 3a",
                "Moore et al 2020-OLS - 1a",
                "Moore et al 2020-OLS - 2a",
                "Moore et al 2020-OLS - 3a",
                "Moore et al 2020-OLS - 4a",
                "Moore et al 2020-OLS - 1a",
                "Moore et al 2020-OLS - 2a",
                "Moore et al 2020-OLS - 3a",
                "Moore et al 2020-OLS - 4a",
                "Olden & Tamayo 2014-Model 1",
                "Olden & Tamayo 2014-Model 2",
                "Olden & Tamayo 2014-Model 3",
                "Poor et al 2001-Lewiston",
                "Poor et al 2001-Augusta",
                "Poor et al 2001-Bangor",
                "Poor et al 2001-Northern Maine",
                "Swedberg el al 2020-Multi-state model",
                "Swedberg el al 2020-Multi-state model",
                "Swedberg el al 2020-Multi-state model",
                "Swedberg el al 2020-Multi-state model",
                "Swedberg el al 2020-Multi-state model",
                "Swedberg el al 2020-Multi-state model",
                "Swedberg el al 2020-Multi-state model - drop Maine",
                "Swedberg el al 2020-Multi-state model - drop Michigan",
                "Swedberg el al 2020-Multi-state model - drop Minnesota",
                "Swedberg el al 2020-Multi-state model - drop New York",
                "Swedberg el al 2020-Multi-state model - drop Vermont",
                "Swedberg el al 2020-Multi-state model - drop Wisconsin",
                "Swedberg el al 2020-State level model - Maine",
                "Swedberg el al 2020-State level model - Michigan",
                "Swedberg el al 2020-State level model - Minnesota",
                "Swedberg el al 2020-State level model - New York",
                "Swedberg el al 2020-State level model - Vermont",
                "Swedberg el al 2020-State level model - Wisconsin",
                "Swedberg el al 2020-Regional model - Otter Tail",
                "Swedberg el al 2020-Regional model - Twin Cities",
                "Swedberg el al 2020-Regional model - Adirondacks",
                "Swedberg el al 2020-Regional model - Finger Lakes",
                "Weng et al 2020-Secci depth",
                "Wolf & Kemp 2021-Model 1",
                "Wolf & Kemp 2021-Model 1",
                "Wolf & Kemp 2021-Model 2",
                "Wolf & Kemp 2021-Model 2",
                "Wolf & Kemp 2021-Model 3",
                "Wolf & Kemp 2021-Model 3",
                "Wolf & Kemp 2021-Model 4",
                "Wolf & Kemp 2021-Model 4",
                "Wolf & Kemp 2021-Model 5",
                "Wolf & Kemp 2021-Model 5",
                "Zhang & Boyle 2010-Milfoil - Quadratic",
                "Zhang & Boyle 2010-Milfoil - Exponential",
                "Zhang & Boyle 2010-Total macrophytes - Quadratic",
                "Zhang & Boyle 2010-Total macrophytes - Exponential",
                "Zhang & Boyle 2010-Best Model - Quadratic",
                "Zhang & Boyle 2010-Best Model - Exponential",
                'Zhang et al 2015-VT1',
                "Zhang et al 2015-VT2",
                "Zhang et al 2015-VT3",
                "Zhang et al 2015-ME1",
                "Zhang et al 2015-ME2",
                "Zhang et al 2015-ME3",
                "Zhang et al 2015-ME4",
                "Zhang et al 2015-ME5",
                "Zhang et al 2015-ME6",
                "Zhang et al 2015-ME7",
                "Zhang et al 2015-NH1",
                "Zhang et al 2015-NH2",
                "Zhang et al 2015-NH3",
                "Zhang et al 2015-NH4",
                "Zhang et al 2015-NH5")




df_studname <- data_frame(model, study_name)

df1$model <- as.numeric(as.character(df1$model))

df1 <- df1%>%
  left_join(df_studname)

library(stringr)

df1[c('model_name', 'study_name')] <- str_split_fixed(df1$study_name, '-', 2)


df1 <- df1%>%
  rename("study_name" = "model_name",
         "model name" = "study_name")%>%
  mutate(distance_spec = "No Distance Variable")



p2 <- ggplot(df1, aes(dist, elast)) +
  geom_point(size=0.005)+
  geom_smooth()


p_no_dist <- ggplot(df1, aes(dist, elast)) +
  geom_function(fun =~ if_else(.x <= 500, 0.009,NA), size = x) + # Ara 2007-IH Cluster 5 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.172,NA), size = x) + # Ara 2007 - IH Cluster 6 - OLS
  geom_function(fun =~ if_else(.x <= 500, -0.033,NA), size = x) + # Ara 2007 - IH Cluster 7 - OLS
  geom_function(fun =~ if_else(.x <= 500, -0.033,NA), size = x) + # Ara 2007 - IH Cluster 11 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.012,NA), size = x) + # Ara 2007 - IH Cluster 5 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.163,NA), size = x) + # Ara 2007 - IH Cluster 6 - GMM
  geom_function(fun =~ if_else(.x <= 500, -0.007,NA), size = x) + # Ara 2007 - IH Cluster 7 - GMM
  geom_function(fun =~ if_else(.x <= 500, -0.029,NA), size = x) + # Ara 2007 - IH Cluster 11 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.204,NA), size = x) + # Ara 2007 - CBG Cluster 2 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.029,NA), size = x) + # Ara 2007 - CBG Cluster 3 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.029,NA), size = x) + # Ara 2007 - CBG Cluster 3 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.12,NA), size = x) + # Ara 2007 - CBG Cluster 4 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.037,NA), size = x) + # Ara 2007 - CBG Cluster 7 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.012,NA), size = x) + # Ara 2007 - CBG Cluster 9 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.203,NA), size = x) + # Ara 2007 - CBG Cluster 2 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.026,NA), size = x) + # Ara 2007 - CBG Cluster 3 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.056,NA), size = x) + # Ara 2007 - CBG Cluster 4 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.052,NA), size = x) + # Ara 2007 - CBG Cluster 7 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.016,NA), size = x) + # Ara 2007 - CBG Cluster 9 - GMM
  geom_function(fun =~ if_else(.x <= 500, 4.369999886*3515/102746,NA), size = x)+ # Boyle and Taylor 2001 - G1-Town Data
  geom_function(fun =~ if_else(.x <= 500, 3.920000076*3515/102746,NA), size = x)+ # Boyle and Taylor 2001 - G1-Survey Data
  geom_function(fun =~ if_else(.x <= 500, 2.099999905*3515/85197,NA), size = x)+ # Boyle and Taylor 2001 - G2-Town Data
  geom_function(fun =~ if_else(.x <= 500, 1.389999986*3515/85197,NA), size = x)+ # Boyle and Taylor 2001 - G2-Survey Data
  geom_function(fun =~ if_else(.x <= 500, 4.730000019*3515/32779,NA), size = x)+ # Boyle and Taylor 2001 - G3-Town Data
  geom_function(fun =~ if_else(.x <= 500, 6.130000114*3515/32779,NA), size = x)+ # Boyle and Taylor 2001 - G3-Survey Data
  geom_function(fun =~ if_else(.x <= 500, 40.02999878*3515/97482,NA), size = x)+ # Boyle and Taylor 2001 - G4-Town Data
  geom_function(fun =~ if_else(.x <= 500, 36.13999939*3515/97482,NA), size = x)+ # Boyle and Taylor 2001 - G4-Survey Data
  geom_function(fun =~ if_else(.x <= 500, 3514/104069*7.375697136,NA), size = x) + # Boyle et al 1999 -model 1
  geom_function(fun =~ if_else(.x <= 500, 3514/85880*3.167430162,NA), size = x) + # Boyle et al 1999 - model 2
  geom_function(fun =~ if_else(.x <= 500, 3514/73938*3.584641457,NA), size = x) + # Boyle et al 1999 - model 3
  geom_function(fun =~ if_else(.x <= 500, 3514/100350*13.07054615,NA), size = x)+  # Boyle et al 1999 - model 4
  geom_function(fun =~ if_else(.x <= 500, 178.3*3.83/2239.2,NA), size = x)+  # Calder¢n-Arrieta 2019 - model 1
  geom_function(fun =~ if_else(.x <= 500, 185.9*3.83/2239.2,NA), size = x)+  # Calder¢n-Arrieta 2019 - model 2
  geom_function(fun =~ if_else(.x <= 500, 43520.43*3.851/648415.8,NA), size = x)+  # Clapper & Caudill 2014 - liner - sale price
  geom_function(fun =~ if_else(.x <= 500, 0.065*3.851,NA), size = x)+  # Clapper & Caudill 2014 - log-liner - sale price
  geom_function(fun =~ if_else(.x <= 500, 0.269,NA), size = x)+  # Clapper & Caudill 2014 - log-log - sale price
  geom_function(fun =~ if_else(.x <= 500, 30.988*3.851/443.566,NA), size = x)+  # Clapper & Caudill 2014 - liner - sale price per square foot
  geom_function(fun =~ if_else(.x <= 500, 0.064*3.851,NA), size = x)+  # Clapper & Caudill 2014 - log-liner - sale price per square foot
  geom_function(fun =~ if_else(.x <= 500, 0.268,NA), size = x)+  # Clapper & Caudill 2014 - log-log - sale price per square foot
  geom_function(fun =~ if_else(.x <= 500, 4.48059988*1235.77002/138763.05,NA), size = x)+  # Gibbs et al 2002 - Conway/Milton
  geom_function(fun =~ if_else(.x <= 500, 17.34000015*1879.099976/175157.73,NA), size = x)+  # Gibbs et al 2002 - Winnipesaukee
  geom_function(fun =~ if_else(.x <= 500, 76.76999664*213.5800018/132162.84,NA), size = x)+  # Gibbs et al 2002 - Derry/Amherst
  geom_function(fun =~ if_else(.x <= 500, 149.6000061*283.6900024/167104.7,NA), size = x)+  # Gibbs et al 2002 - Spofford/Greenfield
  geom_function(fun =~ if_else(.x <= 500, 13893.84961*3.28*0.926591992/268034.57,NA), size = x)+  # Horsch & Lewis 2009 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 14355.2998*3.28*0.926591992/268034.57,NA), size = x)+  # Horsch & Lewis 2009 - Model 2
  geom_function(fun =~ if_else(.x <= 500, 13367.92969*3.28*0.926591992/268034.57,NA), size = x)+  # Horsch & Lewis 2009 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 7072.709961*3.28*0.926591992/268034.57,NA), size = x)+  # Horsch & Lewis 2009 - Random Effects with Year Dummies
  geom_function(fun =~ if_else(.x <= 500, 6443.779785*3.28*0.926591992/268034.57,NA), size = x)+  # Horsch & Lewis 2009 - Random Effects with Year Trend Variable
  geom_function(fun =~ if_else(.x <= 500, 8.75*866/65363.49,NA), size = x)+  # Hsu 2000 - Northeast Kingdom
  geom_function(fun =~ if_else(.x <= 500, 12.84000015*2021/120063.58,NA), size = x)+  # Hsu 2000 - Lake Champlain
  geom_function(fun =~ if_else(.x <= 500, 10.48999977*1552/104471.03,NA), size = x)+  # Hsu 2000 - Rutland County
  geom_function(fun =~ if_else(.x <= 500, 10.60999966*1581/105410.9,NA), size = x)+  # Hsu 2000 - Milfoil
  geom_function(fun =~ if_else(.x <= 500, 13.32999992*1581/105410.9,NA), size = x)+  # Hsu 2000 - exp(milfoil)
  geom_function(fun =~ if_else(.x <= 500, 44.31000137*1581/105410.9,NA), size = x)+  # Hsu 2000 - Total Weed
  geom_function(fun =~ if_else(.x <= 500, 22.92000008*1581/105410.9,NA), size = x)+  # Hsu 2000 - exp(Total Weed)
  geom_function(fun =~ if_else(.x <= 2000, 0.059,NA), size = x)+  # Irwin & Wolf 2022 - Model 1
  geom_function(fun =~ if_else(.x <= 2000, 0.093,NA), size = x)+  # Irwin & Wolf 2022 - Model 2
  geom_function(fun =~ if_else(.x <= 2000, 0.103,NA), size = x)+  # Irwin & Wolf 2022 - Model 3
  geom_function(fun =~ if_else(.x <= 2000, 0.086,NA), size = x)+  # Irwin & Wolf 2022 - Model 4
  geom_function(fun =~ if_else(.x <= 2000, 0.079,NA), size = x)+  # Irwin & Wolf 2022 - Model 6
  geom_function(fun =~ if_else(.x <= 500, 5207*3.28*3.627120018/184892,NA), size = x)+  # Kashian et al 2006 - Hedonic Equation (3)
  geom_function(fun =~ if_else(.x <= 500, 46459.33/(218562*3.28),NA), size = x)+  # Kemp et al 2017 - Model 1 (SD)
  geom_function(fun =~ if_else(.x <= 500, 45292.89/(238142*3.28),NA), size = x)+  # Kemp et al 2017 - Model 2 (SD)
  geom_function(fun =~ if_else(.x <= 500, 66046.22/(175331*3.28),NA), size = x)+  # Kemp et al 2017 - Model 3 (SD)
  geom_function(fun =~ if_else(.x <= 500, 13.22999954*702.6099854/100313,NA), size = x)+  # Krysel et al 2003  - Aitkin Lake Group
  geom_function(fun =~ if_else(.x <= 500, 4.71999979*985.7600098/176461,NA), size = x)+  # Krysel et al 2003 - Brainerd Lake Group
  geom_function(fun =~ if_else(.x <= 500, 1.100000024*852.2600098/135905,NA), size = x)+  # Krysel et al 2003 - Grand Rapids Lake Group
  geom_function(fun =~ if_else(.x <= 500, 2.150000095*1031.869995/179621,NA), size = x)+  # Krysel et al 2003 - Walker Lake Group
  geom_function(fun =~ if_else(.x <= 500, 21.75*882.9099731/124390,NA), size = x)+  # Krysel et al 2003 - Park Rapids Lake Group
  geom_function(fun =~ if_else(.x <= 500, 7.309999943*702.6099854/100313,NA), size = x)+  # Krysel et al 2003 - MN - Aitkin Lake Group
  geom_function(fun =~ if_else(.x <= 500, 1.940000057*985.7600098/176461,NA), size = x)+  # Krysel et al 2003 - MN - Brainerd Lake Group
  geom_function(fun =~ if_else(.x <= 500, 1.730000019*852.2600098/135905,NA), size = x)+  # Krysel et al 2003 - MN - Grand Rapids Lake Group
  geom_function(fun =~ if_else(.x <= 500, 1.909999967*1031.869995/179621,NA), size = x)+  # Krysel et al 2003 - MN - Walker Lake Group
  geom_function(fun =~ if_else(.x <= 500, 19.95000076*882.9099731/124390,NA), size = x)+  # Krysel et al 2003 - MN - Park Rapids Lake Group
  geom_function(fun =~ if_else(.x <= 500, 9.720000267*1101.5/142829,NA), size = x)+  # Krysel et al 2003 - MN - Bemidji Lake Group
  geom_function(fun =~ if_else(.x <= 500, 0.275000006,NA), size = x)+  # Liao et al 2016 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 0.231999993,NA), size = x)+  # Liao et al 2016 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 0.219999999,NA), size = x)+  # Liao et al 2016 - Spatial Regime Model 1
  geom_function(fun =~ if_else(.x <= 500, 0.165000007,NA), size = x)+  # Liao et al 2016 - Spatial Regime Model 1
  geom_function(fun =~ if_else(.x <= 500, 0.223000005,NA), size = x)+  # Liao et al 2016 - Spatial Regime Model 3
  geom_function(fun =~ if_else(.x <= 500, 0.165999994,NA), size = x)+  # Liao et al 2016 - Spatial Regime Model 3
  geom_function(fun =~ if_else(.x <= 2000, 0.30340001*0.709999979,NA), size = x)+  # Liu et al 2014 - Sale Price - semi-log
  geom_function(fun =~ if_else(.x <= 2000, -0.0145, NA), size = x) +  # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:1
  geom_function(fun =~ if_else(.x <= 2000, 0.024, NA), size = x) + # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:2
  geom_function(fun =~ if_else(.x <= 2000, -0.0094, NA), size = x) + # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:3
  geom_function(fun =~ if_else(.x <= 2000, 0.028, NA), size = x) + # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:4
  geom_function(fun =~ if_else(.x <= 500, 0.413004667, NA), size = x) + # Michael et al 2000 - Model 1 - Group 1 - CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.093727589, NA), size = x) + # Michael et al 2000 - Model 1 - Group 2 -  CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.491040945, NA), size = x) + # Michael et al 2000 - Model 1 - Group 3 -  CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.285336018, NA), size = x) + # Michael et al 2000 - Model 2 - Group 1 -  PMIN 
  geom_function(fun =~ if_else(.x <= 500, 0.10351032, NA), size = x) + # Michael et al 2000 - Model 2 - Group 2 -  PMIN  
  geom_function(fun =~ if_else(.x <= 500, 0.664476693, NA), size = x) + # Michael et al 2000 - Model 2 - Group 3 - PMIN 
  geom_function(fun =~ if_else(.x <= 500, 0.376048774, NA), size = x) + # Michael et al 2000 - Model 3 - Group 1 - HMIN   
  geom_function(fun =~ if_else(.x <= 500, 0.138936102, NA), size = x) + # Michael et al 2000 - Model 3 - Group 2 - HMIN 
  geom_function(fun =~ if_else(.x <= 500, 0.916609764, NA), size = x) + # Michael et al 2000 -Model 3 - Group 3 - HMIN  
  geom_function(fun =~ if_else(.x <= 500, 0.385807753, NA), size = x) + # Michael et al 2000 -Model 4 - Group 1 - CMIN * HMIN 
  geom_function(fun =~ if_else(.x <= 500, 0.079734102, NA), size = x) + # Michael et al 2000 -Model 4 - Group 2 - CMIN * HMIN  
  geom_function(fun =~ if_else(.x <= 500, 0.431535065, NA), size = x) + # Michael et al 2000 - Model 4 - Group 3 - CMIN * HMIN 
  geom_function(fun =~ if_else(.x <= 500, -0.062146027, NA), size = x) + # Michael et al 2000 - Model 5 - Group 1 - CMIN * HMIN +/-  
  geom_function(fun =~ if_else(.x <= 500, 0.015297231, NA), size = x) + # Michael et al 2000 - Model 5 - Group 2 - CMIN * HMIN +/-  
  geom_function(fun =~ if_else(.x <= 500, -0.054691911, NA), size = x) + # Michael et al 2000 - Model 5 - Group 3 - CMIN * HMIN +/-  
  geom_function(fun =~ if_else(.x <= 500, 0.040301826, NA), size = x) + # Michael et al 2000 - Model 6 - Group 1 - CMAX/CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.004697338, NA), size = x) + # Michael et al 2000 - Model 6 - Group 2 - CMAX/CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.193003386, NA), size = x) + # Michael et al 2000 - Model 6 - Group 3 - CMAX/CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.432574719, NA), size = x) + # Michael et al 2000 - Model 7 - Group 1 - CMAX/CMIN%
  geom_function(fun =~ if_else(.x <= 500, 0.104469016, NA), size = x) + # Michael et al 2000 - Model 7 - Group 2 - CMAX/CMIN%
  geom_function(fun =~ if_else(.x <= 500, 0.542586148, NA), size = x) + # Michael et al 2000 - Model 7 - Group 3 - CMAX/CMIN%
  geom_function(fun =~ if_else(.x <= 500, 0.452805698, NA), size = x) + # Michael et al 2000 - Model 8 - Group 1 - CMIN-HMIN
  geom_function(fun =~ if_else(.x <= 500, 0.114391185, NA), size = x) + # Michael et al 2000 - Model 8 - Group 2 - CMIN-HMIN
  geom_function(fun =~ if_else(.x <= 500, 0.589391351, NA), size = x) + # Michael et al 2000 - Model 8 - Group 3 - CMIN-HMIN
  geom_function(fun =~ if_else(.x <= 500, 0.325085133, NA), size = x) + # Michael et al 2000 - Model 9 - Group 1 - HMIN+ and HMIN -
  geom_function(fun =~ if_else(.x <= 500, 0.098492384, NA), size = x) + # Michael et al 2000 - Model 9 - Group 2 - HMIN+ and HMIN -
  geom_function(fun =~ if_else(.x <= 500, 0.401052326, NA), size = x) + # Michael et al 2000 - Model 9 - Group 3 - HMIN+ and HMIN -
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.151, NA), size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.154, NA), size = x) + # Moore et al 2020 - IV-GMM - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.114, NA), size = x) + # Moore et al 2020 - OLS - 2a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.117, NA), size = x) + # Moore et al 2020 - IV-GMM - 2b
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.121, NA), size = x) + # Moore et al 2020 - OLS - 3a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.126, NA), size = x) + # Moore et al 2020 - IV-GMM - 3b
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.099, NA), size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.101, NA), size = x) + # Moore et al 2020 - IV-GMM - 4b
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.151, NA),size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.151, NA),size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.151, NA),size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.096, NA),size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.094, NA),size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.098, NA),size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.153 + -0.118*1 + -0.068*0 + -0.017*0), NA),size = x) + # Moore et al 2020 - OLS - 1a - Florida
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.153 + -0.118*0 + -0.068*1 + -0.017*0), NA),size = x) + # Moore et al 2020 - OLS - 1a - Indiana
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.153 + -0.118*0 + -0.068*0 + -0.017*1), NA),size = x) + # Moore et al 2020 - OLS - 1a - Washington
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.128 + 0.407*1 + -0.029*0 + -0.02*0), NA),size = x) + # Moore et al 2020 - OLS - 2a - Florida
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.128 + 0.407*0 + -0.029*1 + -0.02*0), NA),size = x) + # Moore et al 2020 - OLS - 2a - Indiana
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.128 + 0.407*0 + -0.029*0 + -0.02*1), NA),size = x) + # Moore et al 2020 - OLS - 2a - Washington
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.138 + 0.358*1 + -0.041*0 + -0.03*0), NA),size = x) + # Moore et al 2020 - OLS - 3a - Florida
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.138 + 0.358*0 + -0.041*1 + -0.03*0), NA),size = x) + # Moore et al 2020 - OLS - 3a - Indiana
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.138 + 0.358*0 + -0.041*0 + -0.03*1), NA),size = x) + # Moore et al 2020 - OLS - 3a - Washington
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.111 + 0.251*1 + -0.072*0 + -0.007*0), NA),size = x) + # Moore et al 2020 - OLS - 3a - Florida
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.111 + 0.251*0 + -0.072*1 + -0.007*0), NA),size = x) + # Moore et al 2020 - OLS - 3a - Indiana
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.111 + 0.251*0 + -0.072*0 + -0.007*1), NA),size = x) + # Moore et al 2020 - OLS - 3a - Washington
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.21, NA),size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.09, NA),size = x) + # Moore et al 2020 - OLS - 2a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.103, NA),size = x) + # Moore et al 2020 - OLS - 3a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.085, NA),size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.186, NA),size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.07, NA),size = x) + # Moore et al 2020 - OLS - 2a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.084, NA),size = x) + # Moore et al 2020 - OLS - 3a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.089, NA),size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 500, -7430.29*3.4/502312.8, NA),size = x) + # Olden & Tamayo 2014 - Model 1
  geom_function(fun =~ if_else(.x <= 500, -6962.7*3.4/502312.8, NA),size = x) + # Olden & Tamayo 2014 - Model 2
  geom_function(fun =~ if_else(.x <= 500, -6954.8*3.4/502312.8, NA),size = x) + # Olden & Tamayo 2014 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 10.36999989*3209/103853, NA),size = x) + # Poor et al 2001 - Lewiston
  geom_function(fun =~ if_else(.x <= 500, 2.25*4391/86880, NA),size = x) + # Poor et al 2001 - Augusta
  geom_function(fun =~ if_else(.x <= 500, 1.46*2517/67881, NA),size = x) + # Poor et al 2001 - Bangor
  geom_function(fun =~ if_else(.x <= 500, -0.88*5161/31287, NA),size = x) + # Poor et al 2001 - Northern Maine
  geom_function(fun =~ if_else(.x <= 500, 0.17+6.80, NA),size = x) + # Swedberg el al 2020 - Multi-state model- ME ###Outlier
  geom_function(fun =~ if_else(.x <= 500, 0.17+ -1.04, NA),size = x) + # Swedberg el al 2020 - Multi-state model- MI
  geom_function(fun =~ if_else(.x <= 500, 0.17+ 0.08, NA),size = x) + # Swedberg el al 2020 - Multi-state model-  NY
  geom_function(fun =~ if_else(.x <= 500, 0.17+ 0.14, NA),size = x) + # Swedberg el al 2020 - Multi-state model-  VT
  geom_function(fun =~ if_else(.x <= 500, 0.17+ -0.11, NA),size = x) + # Swedberg el al 2020 - Multi-state model-   WI
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - Multi-state model
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop ME
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop MI
  geom_function(fun =~ if_else(.x <= 500, 0.13, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop MN
  geom_function(fun =~ if_else(.x <= 500, 0.16, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop NY
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop VT
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop WI
  geom_function(fun =~ if_else(.x <= 500, -2.71, NA),size = x) + # Swedberg el al 2020 - State level model - Maine
  geom_function(fun =~ if_else(.x <= 500, 9.20, NA),size = x) + # Swedberg el al 2020 - State level model - Michigan
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - State level model - Minnesota
  geom_function(fun =~ if_else(.x <= 500, 0.13, NA),size = x) + # Swedberg el al 2020 - State level model - New York
  geom_function(fun =~ if_else(.x <= 500, -0.32, NA),size = x) + # Swedberg el al 2020 - State level model - Vermont
  geom_function(fun =~ if_else(.x <= 500, 0.1, NA),size = x) + # Swedberg el al 2020 - State level model - Wisconsin
  geom_function(fun =~ if_else(.x <= 500, 0.5, NA),size = x) + # Swedberg el al 2020 - Regional model - Otter Tail
  geom_function(fun =~ if_else(.x <= 500, 0.1, NA),size = x) + # Swedberg el al 2020 - Regional model - Twin Cities
  geom_function(fun =~ if_else(.x <= 500, -0.21, NA),size = x) + # Swedberg el al 2020 - Regional model - Adirondacks
  geom_function(fun =~ if_else(.x <= 500, 0.41, NA),size = x) + # Swedberg el al 2020 - Regional model - Regional model - Finger Lakes
  geom_function(fun =~ if_else(.x <= 500, 0.129, NA),size = x) + # Weng et al 2020 - Secci depth
  geom_function(fun =~ if_else(.x <= 139, 0.0352 + 0.0755 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 1
  geom_function(fun =~ if_else(.x <= 139, 0.0136 + 0.0543 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 1
  geom_function(fun =~ if_else(.x <= 139, 0.0548 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 2
  geom_function(fun =~ if_else(.x <= 139, 0.0291 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 2
  geom_function(fun =~ if_else(.x <= 139, 0.0092 + 0.0498 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 3
  geom_function(fun =~ if_else(.x <= 139, -0.0205 + 0.0444 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 3
  geom_function(fun =~ if_else(.x <= 139, -0.0147 + 0.0447 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 4
  geom_function(fun =~ if_else(.x <= 139, -0.0242 + 0.0421 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 4
  geom_function(fun =~ if_else(.x <= 139, -0.0086 + 0.0432 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 5
  geom_function(fun =~ if_else(.x <= 139, 0.022 + 0.0407 * log(2.39), NA),size = x)+ # Wolf & Kemp 2021 - Model 5
  geom_function(fun =~ if_else(.x <= 500, -0.647772014, NA),size = x) + # Zhang & Boyle 2010 - Milfoil - Quadratic
  geom_function(fun =~ if_else(.x <= 500, -0.647772014, NA),size = x) + # Zhang & Boyle 2010 - Milfoil - Exponential
  geom_function(fun =~ if_else(.x <= 500, -0.323886007, NA),size = x) + # Zhang & Boyle 2010 - Total macrophytes - Quadratic
  geom_function(fun =~ if_else(.x <= 500, -0.323886007, NA),size = x) + # Zhang & Boyle 2010 - Total macrophytes - Exponential
  geom_function(fun =~ if_else(.x <= 500, -0.161943004, NA),size = x) + # Zhang & Boyle 2010 - Best Model - Quadratic
  geom_function(fun =~ if_else(.x <= 500, -0.323886007, NA),size = x) + # Zhang & Boyle 2010 - Best Model - Exponential
  geom_function(fun =~ if_else(.x <= 500, 0.18130137, NA),size = x) + # Zhang et al 2015 - VT1
  geom_function(fun =~ if_else(.x <= 500, 0.168578461, NA),size = x) + # Zhang et al 2015 - VT2
  geom_function(fun =~ if_else(.x <= 500, 0.031807259, NA),size = x) + # Zhang et al 2015 - VT3
  geom_function(fun =~ if_else(.x <= 500, 0.368520737, NA),size = x) + # Zhang et al 2015 - ME1
  geom_function(fun =~ if_else(.x <= 500, -0.044222489, NA),size = x) + # Zhang et al 2015 - ME2
  geom_function(fun =~ if_else(.x <= 500, 0.078617759, NA),size = x) + # Zhang et al 2015 - ME3
  geom_function(fun =~ if_else(.x <= 500, 0.560151517, NA),size = x) + # Zhang et al 2015 - ME4
  geom_function(fun =~ if_else(.x <= 500, -0.00491361, NA),size = x) + # Zhang et al 2015 - ME5
  geom_function(fun =~ if_else(.x <= 500, -0.01965444, NA),size = x) + # Zhang et al 2015 - ME6
  geom_function(fun =~ if_else(.x <= 500, 1.719763517, NA),size = x) + # Zhang et al 2015 - ME7
  geom_function(fun =~ if_else(.x <= 500, 0.035835754, NA),size = x) + # Zhang et al 2015 - NH1
  geom_function(fun =~ if_else(.x <= 500, 0.04596325, NA),size = x) + # Zhang et al 2015 - NH2
  geom_function(fun =~ if_else(.x <= 500, 1.505880713, NA),size = x) + # Zhang et al 2015 - NH3
  geom_function(fun =~ if_else(.x <= 500, 0.971460581, NA),size = x) + # Zhang et al 2015 - NH4
  geom_function(fun =~ if_else(.x <= 500, 0.185411081, NA),size = x, aes(col = "Primary Study Estimations")) + # Zhang et al 2015 - NH5
  #geom_point(aes(color = study_name), size=0.5)+
  geom_smooth(method = "gam", aes(color = "Smoothed line (GAM)"))+
  scale_colour_manual(values = c("Smoothed line (GAM)"="red",
                                 "Primary Study Estimations"="black"))+
  theme_bw()+
  labs(x = "", y = "Elasticity")+
  scale_x_continuous(breaks=seq(0, 2050, 250),expand=c(0, 0), limits=c(0, 2050))+
  scale_y_continuous(expand=c(0, 0), limits=c(0, 0.6))+
  labs(color='')+
  theme(legend.position = "bottom")+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.text=element_text(size=11))
  
  




##############################################################################
# Linear distance
base_linear <-
  ggplot() +
  xlim(0, 2000)
  


p1 <- base_linear + geom_function(fun =~ if_else(.x <= 2000, 0.248 + -0.076*(log(.x/100)),NA),size = x) + # # Irwin & Wolf 2022 - Model 5 (2km)
  geom_function(fun =~ if_else(.x <= 2000, (0.0964 + -0.0305*(.x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - without distance threshhold interaction( sample within 5 miles)
  geom_function(fun =~ if_else(.x <= 2000, (0.2096 + -0.0426*(.x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - property fixed effect model - 2 or more sale
  geom_function(fun =~ if_else(.x <= 2000, (0.2638 + -0.0682*(.x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - property fixed effect model - 3 or more sale
  geom_function(fun =~ if_else(.x <= 2000, (0.4503 + -0.1117*(.x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - property fixed effect model - 4 or more sale
  geom_function(fun =~ if_else(.x <= 2000, (0.0841 + -0.025*(.x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - property fixed effect model - 4 or more sale
  geom_function(fun =~ if_else(.x <= 2000, (6966.6554 + -469.5857*(.x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - linear model
  ggtitle("Hedonic function with linear distance interation")+
  xlab("Distance to waterbody (meters)") + ylab("Elasticity")

p_build_1 = ggplot_build(p1)


plot_data_1 <-  as.data.frame(p_build_1$data[1:7])

df <- plot_data_1%>%
  select(matches('x|y'))%>%
  rename("dist" = "x")%>%
  select(!matches('linetype'))%>%
  select(!matches('x'))


df2 <- df %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "model",
    names_prefix = "e",
    values_to = "elast",
    values_drop_na = TRUE
  )



df2$model[which(df2$model == "y")] = 0

df2$model <- str_remove(df2$model, "y.")


model <- c(0:6)
study_name <- c("Irwin & Wolf 2022-Model 5",
                "Liu et al 2019-without distance threshhold interaction",
                "Liu et al 2019-property fixed effect model - 2 or more sale",
                "Liu et al 2019-property fixed effect model - 3 or more sale",
                "Liu et al 2019-property fixed effect model - 4 or more sale",
                "Liu et al 2019-property fixed effect model - 4 or more sale",
                "Liu et al 2019-without distance threshhold interaction")



df_studname <- data_frame(model, study_name)

df2$model <- as.numeric(as.character(df2$model))

df2 <- df2%>%
  left_join(df_studname)

library(stringr)

df2[c('model_name', 'study_name')] <- str_split_fixed(df2$study_name, '-', 2)


df2 <- df2%>%
  rename("study_name" = "model_name",
         "model name" = "study_name")%>%
  mutate(distance_spec = "Continious Distance Variable")



ggplot(df2, aes(dist, elast)) +
  geom_point(size=0.005)+
  geom_smooth()

# Select subset where elasticity vary between 0-0.5

df_sub <- df2%>%
  filter(elast > 0 & elast < 0.8)

p_linear <- ggplot(df2, aes(dist, elast)) +
  geom_function(fun =~ if_else(.x <= 2000, 0.248 + -0.076*(log(.x/100)),NA),size = x) + # # Irwin & Wolf 2022 - Model 5 (2km)
  geom_function(fun =~ if_else(.x <= 2000, (0.0964 + -0.0305*(.x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - without distance threshhold interaction( sample within 5 miles)
  geom_function(fun =~ if_else(.x <= 2000, (0.2096 + -0.0426*(.x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - property fixed effect model - 2 or more sale
  geom_function(fun =~ if_else(.x <= 2000, (0.2638 + -0.0682*(.x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - property fixed effect model - 3 or more sale
  geom_function(fun =~ if_else(.x <= 2000, (0.4503 + -0.1117*(.x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - property fixed effect model - 4 or more sale
  geom_function(fun =~ if_else(.x <= 2000, (0.0841 + -0.025*(.x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - property fixed effect model - 4 or more sale
  geom_function(fun =~ if_else(.x <= 2000, (6966.6554 + -469.5857*(.x/1609.34))*(0.542544*3.28/217951.2),NA),size = x,aes(col = "Primary Study Estimations")) +
  #geom_point(aes(color = study_name), size=0.6)+
  geom_smooth(method = "gam", aes(color = "Smoothed line (GAM)"))+
  scale_colour_manual(values = c("Smoothed line (GAM)"="red",
                                 "Primary Study Estimations"="black"))+
  theme_bw()+
  labs(x = "", y = "")+
  scale_x_continuous(breaks=seq(0, 2050, 250), expand=c(0, 0), limits=c(0, 2050))+
  scale_y_continuous(expand=c(0, 0), limits=c(0, 0.85))+
  labs(color='')+
  theme(legend.position = "bottom")+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.text=element_text(size=11))




###########################################################################
# Linear distance + dummy distance

base_linear_dummy <-
  ggplot() +
  xlim(0, 2000)

p1 <- base_linear_dummy + geom_function(fun =~ if_else(.x <= 300,(0.0876 + 0.1973*1 + (-0.0287* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 301 & .x <= 2000,(0.0876 + 0.1973*0 + (-0.0287* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 161,(0.0914 + 0.1568*1 + (-0.0292* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 161m (1)
  geom_function(fun =~ if_else(.x >= 161 & .x <= 2000,(0.0914 + 0.1568*0 + (-0.0292* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 161m (0)
  geom_function(fun =~ if_else(.x <= 482,(0.0937 + 0.0772*1 + (-0.0294* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 482m (1)
  geom_function(fun =~ if_else(.x >= 482 & .x <= 2000,(0.0937 + 0.0772*0 + (-0.0294* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 482m (0)
  geom_function(fun =~ if_else(.x <= 804,(0.0951 + 0.02*1 + (-0.0302* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 804m (1)
  geom_function(fun =~ if_else(.x >= 804 & .x <= 2000,(0.0951 + 0.02*0 + (-0.0302* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 804m (0)
  geom_function(fun =~ if_else(.x <= 965,(0.0961 + 0.0055*1 + (-0.0304* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 965m (1)
  geom_function(fun =~ if_else(.x >= 965 & .x <= 2000,(0.0961 + 0.0055*0 + (-0.0304* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 965m (0)
  geom_function(fun =~ if_else(.x <= 300,(0.0742 + 0.1978*1 + (-0.0237* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.0742 + 0.1978*0 + (-0.023* .x/1609.34))*0.542544*3.28,NA),size = x) +# Liu et al 2019 - waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 161,(0.0783 + 0.1559*1 + (-0.0236* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 161m (1)
  geom_function(fun =~ if_else(.x >= 161 & .x <= 2000,(0.0783 + 0.1559*0 + (-0.0236* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 161m (0)
  geom_function(fun =~ if_else(.x <= 482,(0.0809 + 0.077*1 + (-0.0238* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 482m (1)
  geom_function(fun =~ if_else(.x >= 482 & .x <= 2000,(0.0809 + 0.077*0 + (-0.0238* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 482m (0)
  geom_function(fun =~ if_else(.x <= 804,(0.0822 + 0.0285*1 + (-0.0245* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 804m (1)
  geom_function(fun =~ if_else(.x >= 804 & .x <= 2000,(0.0822 + 0.0285*0 + (-0.0245* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 804m (0)
  geom_function(fun =~ if_else(.x <= 965,(0.0834 + 0.0148*1 + (-0.0249* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 965m (1)
  geom_function(fun =~ if_else(.x >= 965 & .x <= 2000,(0.0834 + 0.0148*0 + (-0.0249* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 965m (0)
  geom_function(fun =~ if_else(.x <= 300,(4165.1918 + 63099.4773*1 + (96.6563* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x)+ # Liu et al 2019 - waterfron adjucent 300m (1) linear
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000,(4165.1918 + 63099.4773*0 + (96.6563* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - waterfron adjucent 300m (0) linear
  geom_function(fun =~ if_else(.x <= 161,(5450.1438 + 48099.1068*1 + (-80.7421* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x)+ # Liu et al 2019 - waterfron adjucent 161m (1) linear
  geom_function(fun =~ if_else(.x >= 161 & .x <= 2000,(5450.1438 + 48099.1068*0 + (-80.7421* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - waterfron adjucent 161m (0) linear
  geom_function(fun =~ if_else(.x <= 482,(6215.4558 + 21609.3494*1 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x)+ # Liu et al 2019 - waterfron adjucent 482m (1) linear
  geom_function(fun =~ if_else(.x >= 482 & .x <= 2000, (6215.4558 + 21609.3494*0 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - waterfron adjucent 482m (0) linear
  geom_function(fun =~ if_else(.x <= 804,(6215.4558 + 21609.3494*1 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x)+ # Liu et al 2019 - waterfron adjucent 804m (1) linear
  geom_function(fun =~ if_else(.x >= 804 & .x <= 2000, (6215.4558 + 21609.3494*0 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - waterfron adjucent 804m (0) linear
  geom_function(fun =~ if_else(.x <= 965,(6807.1986 + 3826.6165*1 + (-437.7854* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x)+ # Liu et al 2019 - waterfron adjucent 965m (1) linear
  geom_function(fun =~ if_else(.x >= 965 & .x <= 2000, (6807.1986 + 3826.6165*0 + (-437.7854* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - waterfron adjucent 965m (0) linear
  geom_function(fun =~ if_else(.x <= 300,(0.0084 + 0.0099*1 + (-0.0016*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 1 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.0084 + 0.0099*0 + (-0.0016*.x)/1000)*1.862328*3.28,NA),size = x) + # Nepf et al 2022 - Model 1 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300,(0.0208 + 0.001*1 + (-0.0029*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 2 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000,(0.0208 + 0.001*0 + (-0.0029*.x)/1000)*1.862328*3.28 ,NA),size = x) + # Nepf et al 2022 - Model 2 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300,(0.0191 + 0.004*1 + (-0.0022*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 3 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.0191 + 0.004*0 + (-0.0022*.x)/1000)*1.862328*3.28 ,NA),size = x) + # Nepf et al 2022 - Model 3 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300, (0.0221 + -0.0003*1 + (-0.0031*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 4 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.0221 + 0.004*0 + (-0.0003*.x)/1000)*1.862328*3.28,NA),size = x) + # Nepf et al 2022 - Model 4 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300, (0.0026 + 0.0277*1 + (0.0005*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 9 - continious distance/regional analysis waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.0026 + 0.0277*0  + (0.0005*.x)/1000)*1.862328*3.28,NA),size = x) + # Nepf et al 2022 - Model 9 - continious distance/regional analysis waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300,(-0.0016 + 0.0262*1 + (0.0012*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 10 - continious distance/regional analysis waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (-0.0016 + 0.0262*0  + (0.0012*.x)/1000)*1.862328*3.28,NA),size = x) + # Nepf et al 2022 - Model 10 - continious distance/regional analysis waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300,(0.007 + 0.0263*1 + (0.0004*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 11 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.007 + 0.0263*0  + (0.0004*.x)/1000)*1.862328*3.28,NA),size = x) + # Nepf et al 2022 - Model 11 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300,(-0.009 + 0.0266*1 + (0.0016*.x)/1000)*1.862328*3.28,NA),size = x)+ #  Nepf et al 2022 - Model 12 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (-0.009 + 0.0266*0  + (0.0016*.x)/1000)*1.862328*3.28,NA),size = x) + #  Nepf et al 2022 - Model 12 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 50,0.117 + 0.08*1 + -0.017* log(.x),NA),size = x)+ #  Walsh et al 2011a - Model 2 waterfron adjucent 50m (1)
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467,  0.117 + 0.08*0 + -0.017* log(.x) ,NA),size = x) + #  Walsh et al 2011a - Model 2 waterfron adjucent 50m (0)
  geom_function(fun =~ if_else(.x <= 50,0.117 + 0.08*1 + -0.017* log(.x) + 0.011*log(519.2999878),NA),size = x)+ #  Walsh et al 2011a - Model 3 waterfron adjucent 50m (1)
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467,  0.117 + 0.08*0 + -0.017* log(.x) + 0.011*log(519.2999878) ,NA),size = x) + #  Walsh et al 2011a - Model 3 waterfron adjucent 50m (0)
  geom_function(fun =~ if_else(.x <= 50,0.118 + 0.081*1 + -0.017* log(.x),NA),size = x)+ #  Walsh et al 2011a - Model 2S waterfron adjucent 50m (1)
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467,  0.118 + 0.08*0 + -0.017* log(.x) ,NA),size = x) + #  Walsh et al 2011a - Model 2S waterfron adjucent 50m (0)
  geom_function(fun =~ if_else(.x <= 50,-0.03 + 0.079*1 + -0.017* log(.x) + 0.012*log(519.2999878),NA),size = x)+ #  Walsh et al 2011a - Model 3S waterfron adjucent 50m (1)
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467,  0.117 + 0.08*0 + -0.017* log(.x) + 0.011*log(519.2999878) ,NA),size = x)+ #  Walsh et al 2011a - Model 3S waterfron adjucent 50m (0)
ggtitle("Hedonic function without distance and distance dummy for waterfront ")+
  xlab("Distance to waterbody (meters)") + ylab("Elasticity")


p_build_1 = ggplot_build(p1)

plot_data_1 <-  as.data.frame(p_build_1$data[1:54])

df <- plot_data_1%>%
  select(matches('x|y'))%>%
  rename("dist" = "x")%>%
  select(!matches('linetype'))%>%
  select(!matches('x'))


df3 <- df %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "model",
    names_prefix = "e",
    values_to = "elast",
    values_drop_na = TRUE
  )


df3$model[which(df3$model == "y")] = 0

df3$model <- str_remove(df3$model, "y.")

model <- c(0:53)
study_name <- c("Liu et al 2019-with interaction of adjacent",
                "Liu et al 2019-with interaction of adjacent",
                "Liu et al 2019-with interaction of 0.1mile threshold",
                "Liu et al 2019-with interaction of 0.1mile threshold",
                "Liu et al 2019-with interaction of 0.3mile threshold",
                "Liu et al 2019-with interaction of 0.3mile threshold",
                "Liu et al 2019-with interaction of 0.5mile threshold",
                "Liu et al 2019-with interaction of 0.5mile threshold",
                "Liu et al 2019-with interaction of 0.6mile threshold",
                "Liu et al 2019-with interaction of 0.6mile threshold",
                "Liu et al 2019-with interaction of adjacent",
                "Liu et al 2019-with interaction of adjacent",
                "Liu et al 2019-with interaction of 0.1mile threshold",
                "Liu et al 2019-with interaction of 0.1mile threshold",
                "Liu et al 2019-with interaction of 0.3mile threshold",
                "Liu et al 2019-with interaction of 0.3mile threshold",
                "Liu et al 2019-with interaction of 0.5mile threshold",
                "Liu et al 2019-with interaction of 0.5mile threshold",
                "Liu et al 2019-with interaction of 0.6mile threshold",
                "Liu et al 2019-with interaction of 0.6mile threshold",
                "Liu et al 2019-with interaction of adjacent",
                "Liu et al 2019-with interaction of adjacent",
                "Liu et al 2019-with interaction of 0.1mile threshold",
                "Liu et al 2019-with interaction of 0.1mile threshold",
                "Liu et al 2019-with interaction of 0.3mile threshold",
                "Liu et al 2019-with interaction of 0.3mile threshold",
                "Liu et al 2019-with interaction of 0.5mile threshold",
                "Liu et al 2019-with interaction of 0.5mile threshold",
                "Liu et al 2019-with interaction of 0.6mile threshold",
                "Liu et al 2019-with interaction of 0.6mile threshold",
                "Nepf et al 2022-Model 1 - continious distance",
                "Nepf et al 2022-Model 1 - continious distance",
                "Nepf et al 2022-Model 2 - continious distance",
                "Nepf et al 2022-Model 2 - continious distance",
                "Nepf et al 2022-Model 3 - continious distance",
                "Nepf et al 2022-Model 3 - continious distance",
                "Nepf et al 2022-Model 4 - continious distance",
                "Nepf et al 2022-Model 4 - continious distance",
                "Nepf et al 2022-Model 9 - continious distance/regional analysis",
                "Nepf et al 2022-Model 9 - continious distance/regional analysis",
                "Nepf et al 2022-Model 10 - continious distance/regional analysis",
                "Nepf et al 2022-Model 10 - continious distance/regional analysis",
                "Nepf et al 2022-Model 11 - continious distance",
                "Nepf et al 2022-Model 11 - continious distance",
                "Nepf et al 2022-Model 12 - continious distance",
                "Nepf et al 2022-Model 12 - continious distance",
                "Walsh et al 2011a-Model 2",
                "Walsh et al 2011a-Model 2",
                "Walsh et al 2011a-Model 3",
                "Walsh et al 2011a-Model 3",
                "Walsh et al 2011a-Model 2S",
                "Walsh et al 2011a-Model 2S",
                "Walsh et al 2011a-Model 3S",
                "Walsh et al 2011a-Model 3S")


df_studname <- data_frame(model, study_name)

df3$model <- as.numeric(as.character(df3$model))

df3 <- df3%>%
  left_join(df_studname)

library(stringr)

df3[c('model_name', 'study_name')] <- str_split_fixed(df3$study_name, '-', 2)


df3 <- df3%>%
  rename("study_name" = "model_name",
         "model name" = "study_name")%>%
  mutate(distance_spec = "Continious Distance and Discrete Distance")

ggplot(df3, aes(dist, elast)) +
  geom_point(size=0.005)+
  geom_smooth()



# Select subset where elasticity vary between 0-0.5

df_sub <- df3%>%
  filter(elast > 0 & elast < 0.6)

p_lindist_dumdist <- ggplot(df3, aes(dist, elast)) +
  geom_function(fun =~ if_else(.x <= 300,(0.0876 + 0.1973*1 + (-0.0287* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000,(0.0876 + 0.1973*0 + (-0.0287* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 161,(0.0914 + 0.1568*1 + (-0.0292* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 161m (1)
  geom_function(fun =~ if_else(.x >= 161 & .x <= 2000,(0.0914 + 0.1568*0 + (-0.0292* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 161m (0)
  geom_function(fun =~ if_else(.x <= 482,(0.0937 + 0.0772*1 + (-0.0294* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 482m (1)
  geom_function(fun =~ if_else(.x >= 482 & .x <= 2000,(0.0937 + 0.0772*0 + (-0.0294* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 482m (0)
  geom_function(fun =~ if_else(.x <= 804,(0.0951 + 0.02*1 + (-0.0302* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 804m (1)
  geom_function(fun =~ if_else(.x >= 804 & .x <= 2000,(0.0951 + 0.02*0 + (-0.0302* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 804m (0)
  geom_function(fun =~ if_else(.x <= 965,(0.0961 + 0.0055*1 + (-0.0304* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 965m (1)
  geom_function(fun =~ if_else(.x >= 965 & .x <= 2000,(0.0961 + 0.0055*0 + (-0.0304* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 965m (0)
  geom_function(fun =~ if_else(.x <= 300,(0.0742 + 0.1978*1 + (-0.0237* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.0742 + 0.1978*0 + (-0.023* .x/1609.34))*0.542544*3.28,NA),size = x) +# Liu et al 2019 - waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 161,(0.0783 + 0.1559*1 + (-0.0236* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 161m (1)
  geom_function(fun =~ if_else(.x >= 161 & .x <= 2000,(0.0783 + 0.1559*0 + (-0.0236* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 161m (0)
  geom_function(fun =~ if_else(.x <= 482,(0.0809 + 0.077*1 + (-0.0238* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 482m (1)
  geom_function(fun =~ if_else(.x >= 482 & .x <= 2000,(0.0809 + 0.077*0 + (-0.0238* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 482m (0)
  geom_function(fun =~ if_else(.x <= 804,(0.0822 + 0.0285*1 + (-0.0245* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 804m (1)
  geom_function(fun =~ if_else(.x >= 804 & .x <= 2000,(0.0822 + 0.0285*0 + (-0.0245* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 804m (0)
  geom_function(fun =~ if_else(.x <= 965,(0.0834 + 0.0148*1 + (-0.0249* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 965m (1)
  geom_function(fun =~ if_else(.x >= 965 & .x <= 2000,(0.0834 + 0.0148*0 + (-0.0249* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 965m (0)
  geom_function(fun =~ if_else(.x <= 300,(4165.1918 + 63099.4773*1 + (96.6563* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x)+ # Liu et al 2019 - waterfron adjucent 300m (1) linear
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000,(4165.1918 + 63099.4773*0 + (96.6563* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - waterfron adjucent 300m (0) linear
  geom_function(fun =~ if_else(.x <= 161,(5450.1438 + 48099.1068*1 + (-80.7421* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x)+ # Liu et al 2019 - waterfron adjucent 161m (1) linear
  geom_function(fun =~ if_else(.x >= 161 & .x <= 2000,(5450.1438 + 48099.1068*0 + (-80.7421* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - waterfron adjucent 161m (0) linear
  geom_function(fun =~ if_else(.x <= 482,(6215.4558 + 21609.3494*1 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x)+ # Liu et al 2019 - waterfron adjucent 482m (1) linear
  geom_function(fun =~ if_else(.x >= 482 & .x <= 2000, (6215.4558 + 21609.3494*0 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - waterfron adjucent 482m (0) linear
  geom_function(fun =~ if_else(.x <= 804,(6215.4558 + 21609.3494*1 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x)+ # Liu et al 2019 - waterfron adjucent 804m (1) linear
  geom_function(fun =~ if_else(.x >= 804 & .x <= 2000, (6215.4558 + 21609.3494*0 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - waterfron adjucent 804m (0) linear
  geom_function(fun =~ if_else(.x <= 965,(6807.1986 + 3826.6165*1 + (-437.7854* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x)+ # Liu et al 2019 - waterfron adjucent 965m (1) linear
  geom_function(fun =~ if_else(.x >= 965 & .x <= 2000, (6807.1986 + 3826.6165*0 + (-437.7854* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - waterfron adjucent 965m (0) linear
  geom_function(fun =~ if_else(.x <= 300,(0.0084 + 0.0099*1 + (-0.0016*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 1 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.0084 + 0.0099*0 + (-0.0016*.x)/1000)*1.862328*3.28,NA),size = x) + # Nepf et al 2022 - Model 1 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300,(0.0208 + 0.001*1 + (-0.0029*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 2 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000,(0.0208 + 0.001*0 + (-0.0029*.x)/1000)*1.862328*3.28 ,NA),size = x) + # Nepf et al 2022 - Model 2 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300,(0.0191 + 0.004*1 + (-0.0022*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 3 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.0191 + 0.004*0 + (-0.0022*.x)/1000)*1.862328*3.28 ,NA),size = x) + # Nepf et al 2022 - Model 3 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300, (0.0221 + -0.0003*1 + (-0.0031*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 4 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.0221 + 0.004*0 + (-0.0003*.x)/1000)*1.862328*3.28,NA),size = x) + # Nepf et al 2022 - Model 4 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300, (0.0026 + 0.0277*1 + (0.0005*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 9 - continious distance/regional analysis waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.0026 + 0.0277*0  + (0.0005*.x)/1000)*1.862328*3.28,NA),size = x) + # Nepf et al 2022 - Model 9 - continious distance/regional analysis waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300,(-0.0016 + 0.0262*1 + (0.0012*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 10 - continious distance/regional analysis waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (-0.0016 + 0.0262*0  + (0.0012*.x)/1000)*1.862328*3.28,NA),size = x) + # Nepf et al 2022 - Model 10 - continious distance/regional analysis waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300,(0.007 + 0.0263*1 + (0.0004*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 11 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.007 + 0.0263*0  + (0.0004*.x)/1000)*1.862328*3.28,NA),size = x) + # Nepf et al 2022 - Model 11 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300,(-0.009 + 0.0266*1 + (0.0016*.x)/1000)*1.862328*3.28,NA),size = x)+ #  Nepf et al 2022 - Model 12 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (-0.009 + 0.0266*0  + (0.0016*.x)/1000)*1.862328*3.28,NA),size = x) + #  Nepf et al 2022 - Model 12 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 50,0.117 + 0.08*1 + -0.017* log(.x),NA),size = x)+ #  Walsh et al 2011a - Model 2 waterfron adjucent 50m (1)
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467,  0.117 + 0.08*0 + -0.017* log(.x) ,NA),size = x) + #  Walsh et al 2011a - Model 2 waterfron adjucent 50m (0)
  geom_function(fun =~ if_else(.x <= 50,0.117 + 0.08*1 + -0.017* log(.x) + 0.011*log(519.2999878),NA),size = x)+ #  Walsh et al 2011a - Model 3 waterfron adjucent 50m (1)
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467,  0.117 + 0.08*0 + -0.017* log(.x) + 0.011*log(519.2999878) ,NA),size = x) + #  Walsh et al 2011a - Model 3 waterfron adjucent 50m (0)
  geom_function(fun =~ if_else(.x <= 50,0.118 + 0.081*1 + -0.017* log(.x),NA),size = x)+ #  Walsh et al 2011a - Model 2S waterfron adjucent 50m (1)
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467,  0.118 + 0.08*0 + -0.017* log(.x) ,NA),size = x) + #  Walsh et al 2011a - Model 2S waterfron adjucent 50m (0)
  geom_function(fun =~ if_else(.x <= 50,-0.03 + 0.079*1 + -0.017* log(.x) + 0.012*log(519.2999878),NA),size = x)+ #  Walsh et al 2011a - Model 3S waterfron adjucent 50m (1)
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467,  0.117 + 0.08*0 + -0.017* log(.x) + 0.011*log(519.2999878) ,NA),size = x, aes(col = "Primary Study Estimations"))+ #  Walsh et al 2011a - Model 3S waterfron adjucent 50m (0)
  #geom_point(aes(color = study_name), size=0.5)+
  geom_smooth(method = "gam", aes(color = "Smoothed line (GAM)"))+
  scale_colour_manual(values = c("Smoothed line (GAM)"="red",
                                 "Primary Study Estimations"="black"))+
  theme_bw()+
  labs(x = "Distance to waterbody (meters)", y = "")+
  scale_x_continuous(breaks=seq(0, 2050, 250),expand=c(0, 0), limits=c(0, 2050))+
  scale_y_continuous(expand=c(0, 0), limits=c(0, 0.6))+
  labs(color='')+
  theme(legend.position = "bottom")+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(legend.text=element_text(size=11))



#######################################################################################################
# Distance Dummy

base_dummy <-
  ggplot() +
  xlim(0, 2000)

p1 <- base_dummy + geom_function(fun =~ if_else(.x <= 200, -0.099399999*-1,NA),size = x)+ # Guignet et al 2017 - 2.C:0_200m
  geom_function(fun =~ if_else(.x >= 200 & .x <= 500, -0.0058*-1,NA),size = x)+ # Guignet et al 2017 - 2.C:200_500m
  geom_function(fun =~ if_else(.x <= 100,0.0076 + 0.1673, NA),size = x)+# Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0199 + 0.1778, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County*year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0199 + 0.0435, NA),size = x)+ # Mamun et al 2023:Model 1 - discrete distance bins/FE:County*year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0199,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County*year:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0383 + 0.1758, NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/FE:State*year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0383 + 0.0477, NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/FE:State*year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0383,NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/FE:State*year:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0105 + 0.1674, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract+year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0105 + 0.0437, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract+year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0105,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract+year:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0141 + 0.1804, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County+year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0141 + 0.0475, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County+year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0141,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County+year:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0343 + 0.1797, NA),size = x)+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:State+year)
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0343 + 0.0514, NA),size = x)+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:State+year)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0343,NA),size = x)+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:State+year)
  geom_function(fun =~ if_else(.x <= 100, 0.0287 + 0.1581, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Block*year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0287 + 0.0388, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Block*year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0287,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Block*year:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0108 + 0.1568, NA),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/FE:Block+year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0108 + 0.0413, NA),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/FE:Block+year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0108,NA),size = x)+  # Mamun et al 2023- Model 1:discrete distance bins/FE:Block+year:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0051 + 0.2315, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0051 + 0.0726, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0051,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:year:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0123 + 0.1678, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:tract:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0123 + 0.0439, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:tract:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0123,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:tract:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0085 + 0.1573, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:2:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0085 + 0.0414, NA),size = x)+  # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:2:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0085,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:2:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0072 + 0.1532, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:3:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0072 + 0.0401, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:3:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0072,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:3:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0136 + 0.1586, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:5:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0136 + 0.0455, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:5:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0136,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:5:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0101 + 0.1666, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:7:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0101 + 0.055, NA),size = x)+  # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:7:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0101,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:7:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0018 + 0.1377, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:10:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0018 + 0.0497, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:10:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0018,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:10:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0108 + 0.1654, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:10:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0108 + 0.0417, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:10:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0108,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:10:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0225 + 0.1582, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:30:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0225 + 0.0472, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:30:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0225,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:30:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0231 + 0.129, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:100:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0231 + 0.0461, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:100:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0231,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:300:2000
  geom_function(fun =~ if_else(.x <= 100, 0.0104 + 0.1461, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S6:5:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0104 + 0.0364, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S6:5:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0104,NA),size = x)+  # Mamun et al 2023 - Model 1:discrete distance bins/Table S6:5:300_2000
  geom_function(fun =~ if_else(.x <= 300, 0.0123 + 0.1563, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:1:waterfront we define 300m(1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0123,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:1:waterfront we define 300m(0)
  geom_function(fun =~ if_else(.x <= 50, 0.0424 + 0.0953, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:2:watefront 50m(1)
  geom_function(fun =~ if_else(.x >= 50 & .x <= 2000 , 0.0424,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:2:watefront 50m(0)
  geom_function(fun =~ if_else(.x <= 100, 0.0157 + 0.151, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:3):watefront 100m(1)
  geom_function(fun =~ if_else(.x >= 100 & .x <= 2000 , 0.0157,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:3):watefront 100m(0)
  geom_function(fun =~ if_else(.x <= 200, 0.0076 + 0.1296, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:4:watefront 200m(1)
  geom_function(fun =~ if_else(.x >= 200 & .x <= 2000 , 0.0076,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:4:watefront 200m(0)
  geom_function(fun =~ if_else(.x <= 50, 0.022 + 0.1439, NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/Table S7:5:0_50
  geom_function(fun =~ if_else(.x >= 50 & .x <= 200 , 0.022 + 0.1134,NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/Table S7:5:50_200
  geom_function(fun =~ if_else(.x >= 200 & .x <= 2000 , 0.022, NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/Table S7:5:200_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0104 + 0.1636, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:6:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 200 , 0.0104 + 0.0507,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:6:100_200
  geom_function(fun =~ if_else(.x >= 200 & .x <= 2000 , 0.0104, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:6:200_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:7:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:7:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:7:300_2000
  geom_function(fun =~ if_else(.x <= 200, 0.0037 + 0.1359, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:8:0_200
  geom_function(fun =~ if_else(.x >= 200 & .x <= 500 , 0.0037 + 0.0132,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:8:200_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 2000 , 0.0037, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:8:500_2000
  geom_function(fun =~ if_else(.x <= 300, 0.0019 + 0.1089, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:9:0_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 1000 , 0.0019 + 0.0031,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:9:300_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 2000 , 0.0019, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:9:100_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0041 + 0.155, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:0:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0041 + 0.041,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:0:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0041, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:0:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0166 + 0.1579, NA),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/Table S8:1:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0166 + 0.0375,NA),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/Table S8:1:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0166, NA),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/Table S8:1:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0121 + 0.1645, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:3:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0121 + 0.0393,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:3:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0121, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:3:300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:5:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+  # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:5:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:5:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0072 + 0.1688, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:10:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0072 + 0.041,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:10:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0072, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:10:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0078 + 0.1728, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:anytime:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0078 + 0.0382,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:anytime:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0078, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:anytime:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S9:customer price index:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S9:customer price index:100-300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076, NA),size = x)+  # Mamun et al 2023 - Model 1:discrete distance bins/Table S9:customer price index:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0072 + 0.1532, NA),size = x)+ # Mamun et al 2023 - Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0072 + 0.0401,NA),size = x)+  # Mamun et al 2023 - Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0072, NA),size = x)+ # Mamun et al 2023 - Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0126 + 0.1148, NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:1:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0126 + 0.0184,NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:1:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0126, NA),size = x)+  # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:1:300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0173 + 0.1676, NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:2:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0173 + 0.0401,NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:2:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0401, NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:2:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:4:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:4:100-300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076, NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:4:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.007 + 0.0789, NA),size = x)+ # Mamun et al 2023 -Model 3:discrete distance bins/Table S11:ln(housing price)):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.007 + 0.0227,NA),size = x)+ # Mamun et al 2023 -Model 3:discrete distance bins/Table S11:ln(housing price)):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.007, NA),size = x)+ # Mamun et al 2023 -Model 3:discrete distance bins/Table S11:ln(housing price)):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.0469, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:CPL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204  + 0.0155,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:CPL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:CPL:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + 0.0824, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NAP:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + 0.0333,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NAP:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NAP:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.2392, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NPL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + -0.0445,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NPL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NPL:300_1000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + 0.1601, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SAP):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + 0.1002,NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SAP):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SAP):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.1651, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SPL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + -0.0836,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SPL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SPL:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.1553, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:TPL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + -0.0477,NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:TPL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:TPL:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + 0.0551, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:WMT:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + 0.0303,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:WMT:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:WMT:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.0765, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:XER:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + 0.0002,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:XER:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:XER:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1364, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0949,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AL:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0547, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AR:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.207,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AR:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AR:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.1572, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AZ:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0368,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AZ:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AZ:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1312, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CA):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0552,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CA):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CA):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.2389, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CO):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0367,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CO):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CO):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.1809, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CT):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.065,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CT):100-300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CT):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.5047, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:DE):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.2419,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:DE):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:DE):300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.1284, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:FL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0018,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:FL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:FL:300:2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.5163, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:GA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.2384,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:GA:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:GA:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0758, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.1,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IA:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IA:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.3359, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.2071,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IL:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0794, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IN:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.039,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IN:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IN:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 1.2532, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:KY:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0888,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:KY:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:KY:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0273, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:LA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0462,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:LA:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:LA:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0285, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.059,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MA:100-300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MA:300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.1581, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MD:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0346,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MD:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MD:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1299, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:ME:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.1871,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:ME:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:ME:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0913, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MI:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0146,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MI:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MI:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0672, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MO):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.2849,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MO):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MO):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.3935, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MS:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.6851,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MS:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MS:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.4651, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MT:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.3235,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MT:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MT:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.108, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NC):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.047,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NC):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NC):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0542, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NE:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.1447,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NE:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NE:300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0968, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NH:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0315,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NH:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NH:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.2329, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NJ:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0775,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NJ:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NJ:300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0915, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NV:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0197,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NV:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NV:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1677, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NY:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.074,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NY:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NY:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.3565, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OH:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.051,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OH:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OH:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.3811, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OK:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.3099,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OK:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OK:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0729, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OR:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0604,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OR:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OR:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0623, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:PA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0366,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:PA:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:PA:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.4243, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:RI:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0761,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:RI:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:RI:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.2388, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SC:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.1485,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SC:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SC:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0699, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SD:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0298,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SD:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SD:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1608, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TN:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.1244,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TN:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TN:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1807, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TX:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0074,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TX:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TX:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.9602, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:UT:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0612,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:UT:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:UT:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.3189, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VA):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0059,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VA):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VA):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0231, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VT:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0491,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VT:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VT:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0623, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0217,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WA:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WA:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0069, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WI:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0208,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WI:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WI:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0314, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WV:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.1154,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WV:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WV:300_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0190*1 + 0.0138*0 + 0.0083*0 + 0.0095*0 + -0.0003*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0190*0 + 0.0138*1 + 0.0083*0 + 0.0095*0 + -0.0003*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0190*0 + 0.0138*0 + 0.0083*1 + 0.0095*0 + -0.0003*0)*3.28*1.86,NA),size = x)+  # Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0190*0 + 0.0138*0 + 0.0083*0 + 0.0095*1 + -0.0003*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0190*0 + 0.0138*0 + 0.0083*0 + 0.0095*1 + -0.0003*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:1500:2000
  geom_function(fun =~ if_else(.x <= 300,(0.0088 + 0.0108*1 + 0.0110*0 + 0.0077*0 + 0.0088*0 + 0.0040*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 6:discrete distance bins:Town FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0088 + 0.0108*0 + 0.0110*1 + 0.0077*0 + 0.0088*0 + 0.0040*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 6:discrete distance bins:Town FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*1 + 0.0088*0 + 0.0040*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 6:discrete distance bins:Town FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*0 + 0.0088*1 + 0.0040*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 6:discrete distance bins:Town FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*0 + 0.0088*0 + 0.0040*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 6:discrete distance bins:Town FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0051 + 0.0155*1 + 0.0139*0 + 0.0116*0 + 0.0132*0 + 0.0046*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 7:discrete distance bins:School FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0051 + 0.0155*0 + 0.0139*1 + 0.0116*0 + 0.0132*0 + 0.0046*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 7:discrete distance bins:School FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*1 + 0.0132*0 + 0.0046*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 7:discrete distance bins:School FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*0 + 0.0132*1 + 0.0046*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 7:discrete distance bins:School FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*0 + 0.0132*0 + 0.0046*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 7:discrete distance bins:School FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0090 + 0.0103*1 + 0.0118*0 + 0.0084*0 + 0.0094*0 + 0.0051*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0090 + 0.0103*0 + 0.0118*1 + 0.0084*0 + 0.0094*0 + 0.0051*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*1 + 0.0094*0 + 0.0051*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*0 + 0.0094*1 + 0.0051*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*0 + 0.0094*0 + 0.0051*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0050 + 0.0260*1 + 0.0096*0 + -0.0141*0 + -0.0056*0 + -0.0079*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0050 + 0.0260*0 + 0.0096*1 + -0.0141*0 + -0.0056*0 + -0.0079*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*1 + -0.0056*0 + -0.0079*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*0 + -0.0056*1 + -0.0079*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*0 + -0.0056*0 + -0.0079*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0072 + 0.0170*1 + -0.0048*0 + -0.0193*0 + -0.0007*0 + -0.0167*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0072 + 0.0170*0 + -0.0048*1 + -0.0193*0 + -0.0007*0 + -0.0167*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*1 + -0.0007*0 + -0.0167*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*0 + -0.0007*1 + -0.0167*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*0 + -0.0007*0 + -0.0167*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0098 + 0.0248*1 + 0.0114*0 + -0.0144*0 + -0.0011*0 + -0.0144*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0098 + 0.0248*0 + 0.0114*1 + -0.0144*0 + -0.0011*0 + -0.0144*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*1 + -0.0011*0 + -0.0144*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*0 + -0.0011*1 + -0.0144*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*0 + -0.0011*0 + -0.0144*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0095 + 0.0156*1 + -0.0063*0 + -0.0205*0 + -0.0027*0 + -0.0184*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0095 + 0.0156*0 + -0.0063*1 + -0.0205*0 + -0.0027*0 + -0.0184*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*1 + -0.0027*0 + -0.0184*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*0 + -0.0027*1 + -0.0184*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*0 + -0.0027*0 + -0.0184*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:1500_2000
  geom_function(fun =~ if_else(.x <= 500, 0.06, NA),size = x)+ # TJ, Pat, Richard - Model 1:discrete distance bin:500
  geom_function(fun =~ if_else(.x <= 250, 0.063, NA),size = x)+# TJ, Pat, Richard - Model 2:discrete distance bin: 0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 , 0.056,NA),size = x)+ # TJ, Pat, Richard - Model 2:discrete distance bin: 250_500
  geom_function(fun =~ if_else(.x <= 250, 0.056, NA),size = x)+# TJ, Pat, Richard - Model 3:discrete distance bin: 0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 , 0.007,NA),size = x)+ # TJ, Pat, Richard - Model 3:discrete distance bin: 250_500
  geom_function(fun =~ if_else(.x <= 500, 0.06, NA),size = x)+ # TJ, Pat, Richard - Model 4:discrete distance bin:500
  geom_function(fun =~ if_else(.x <= 250, 0.061, NA),size = x)+# TJ, Pat, Richard - Model 5:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 , 0.059,NA),size = x)+ # TJ, Pat, Richard - Model 5:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 250, 0.087, NA),size = x)+# TJ, Pat, Richard - Model 6:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 , 0.063,NA),size = x)+ # TJ, Pat, Richard - Model 6:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 250, 0.063, NA),size = x)+# TJ, Pat, Richard - Model 7:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 , 0.056,NA),size = x)+ # TJ, Pat, Richard - Model 7:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 500, 0.044, NA),size = x)+ # TJ, Pat, Richard - Model 8:discrete distance bin:500
  geom_function(fun =~ if_else(.x <= 250, 0.041, NA),size = x)+# TJ, Pat, Richard - Model 9:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 ,0.048,NA),size = x)+ # TJ, Pat, Richard - Model 9:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 250, 0.041, NA),size = x)+# TJ, Pat, Richard - Model 9:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 ,0.048,NA),size = x)+ # TJ, Pat, Richard - Model 9:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 250, 0.152, NA),size = x)+# TJ, Pat, Richard - Model 10:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 ,0.083,NA),size = x)+# TJ, Pat, Richard - Model 10:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 100, 0.072, NA),size = x)+# TJ, Pat, Richard - Model 11:discrete distance bin:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 200 , 0.062,NA),size = x)+ # TJ, Pat, Richard - Model 11:discrete distance bin:100_200
  geom_function(fun =~ if_else(.x >=200 & .x <= 300 , 0.053,NA),size = x)+ # TJ, Pat, Richard - Model 11:discrete distance bin:200_300
  geom_function(fun =~ if_else(.x >=300 & .x <= 400 , 0.056,NA),size = x)+ # TJ, Pat, Richard - Model 11:discrete distance bin:300_400
  geom_function(fun =~ if_else(.x >=400 & .x <= 500 , 0.062,NA),size = x)+ # TJ, Pat, Richard - Model 11:discrete distance bin:400_500
  geom_function(fun =~ if_else(.x <= 250, 0.058, NA),size = x)+# TJ, Pat, Richard - Model 12:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 ,0.056,NA),size = x)+ # TJ, Pat, Richard - Model 12:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 250, 0.124, NA),size = x)+# TJ, Pat, Richard - Model 13:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 ,0.074,NA),size = x)+ # TJ, Pat, Richard - Model 13:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 500, 0.031, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.024, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.029, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.018, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.064, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.06, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, -0.04, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.01, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.03, NA),size = x) + # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 50, 0.017 + 0.115, NA),size = x)+# Walsh et al 2011a - Model 1:50
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467 ,0.017,NA),size = x)+# Walsh et al 2011a - Model 5-_467
  geom_function(fun =~ if_else(.x <= 50, 0.017 + 0.11, NA),size = x)+# Walsh et al 2011a - Model 1S:50
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467 ,0.017,NA),size = x) +# Walsh et al 2011a - Model 1S:50_467
  geom_function(fun =~ if_else(.x <= 300, 0.126, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.023, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.009,NA),size = x)+#Walsh et al 2017 - Anne Arundel 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.090, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , -0.009, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:Log:500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  0.015,NA),size = x)+#Walsh et al 2017 - Baltimore county 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.033, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , -0.001, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  -0.021,NA),size = x)+#Walsh et al 2017 - Calvert 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.010, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.001, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  -0.003,NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.058, NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.056, NA),size = x)+  #Walsh et al 2017 - Charles 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  0.107,NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.078, NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.008, NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   0.013,NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.096, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.001, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.012,NA),size = x)+#Walsh et al 2017 - Harford 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.142, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.008, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.002,NA),size = x)+#Walsh et al 2017 - Kent 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.062, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.001, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.022,NA),size = x)+#Walsh et al 2017 - Prince George's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.017, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.060, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   0.068,NA),size = x)+#Walsh et al 2017 - Queen Anne's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.091, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.055, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.141,NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.014, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.015, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.017,NA),size = x)+#Walsh et al 2017 - St Mary's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.156, NA),size = x)+ #Walsh et al 2017 - Talbot 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.014, NA),size = x)+ #Walsh et al 2017 - Talbot 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.031,NA),size = x)+#Walsh et al 2017 - Talbot 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.046, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.015, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.010,NA),size = x) +#Walsh et al 2017 - Wicomico 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0585*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0249*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0089*1.45/0.759,NA),size = x) +#Walsh et al 2017 - Anne Arundel 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0293*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0032*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0060*1.45/0.472,NA),size = x) + #Walsh et al 2017 - Baltimore county 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0088*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0174*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0196*1.45/0.929,NA),size = x) +#Walsh et al 2017 - Calvert 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.0024*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0086*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0012*1.45/0.472,NA),size = x) + #Walsh et al 2017 - Cecil 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.041*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0252*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0335*1.45/0.557,NA),size = x) +#Walsh et al 2017 - Charles 1 Year Average:500-1000
  geom_function(fun =~ if_else(.x <= 300, 0.0557*1.45/0.728, NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0076*1.45/0.728, NA),size = x)+  #Walsh et al 2017 - Dorchester 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0079*1.45/0.728,NA),size = x) +#Walsh et al 2017 - Dorchester 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0243*1.45/0.379, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0022*1.45/0.379, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0022*1.45/0.379,NA),size = x) +#Walsh et al 2017 - Harford 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0289*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0120*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0049*1.45/0.406,NA),size = x) +#Walsh et al 2017 - Kent 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0093*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0018*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0023*1.45/0.470,NA),size = x) + #Walsh et al 2017 - Prince George's 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0151*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.041*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0470*1.45/0.783,NA),size = x) +#Walsh et al 2017 - Queen Anne's 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0300*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0207*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0498*1.45/0.683,NA),size = x) +#Walsh et al 2017 - Somerset 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  -0.0375*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0082*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , -0.0115*1.45/0.833,NA),size = x) +#Walsh et al 2017 - St Mary's 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0631*1.45/1.02, NA),size = x)+ #Walsh et al 2017 - St Talbot 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0122*1.45/1.02, NA),size = x)+  #Walsh et al 2017 - St Talbot 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0190*1.45/1.02,NA),size = x) + #Walsh et al 2017 - St Talbot 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0018*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0130*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:0:500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0116*1.45/0.399,NA),size = x) +#Walsh et al 2017 - Wicomico 1 Year Average:500:100
  geom_function(fun =~ if_else(.x <= 300, 0.3058, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.1020, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0123,NA),size = x)+#Walsh et al 2017 - Anne Arundel 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.05560, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , -0.0386, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  0.0077,NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.0134, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , -0.0779, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  -0.0653,NA),size = x)+  #Walsh et al 2017 - Calvert 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0010, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , -0.1257, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  -0.0362,NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.6413, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.1764, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  0.3021,NA),size = x)+#Walsh et al 2017 - Charles 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0607, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0429, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0053,NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.2600, NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0213, NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.0370,NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0745, NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.1147, NA),size = x)+  #Walsh et al 2017 - Kent 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.1083,NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.0090, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.1411, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.1427,NA),size = x)+#Walsh et al 2017 - Prince George's 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.1310, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.1838, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.1983,NA),size = x)+#Walsh et al 2017 - Queen Anne's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0839, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0632, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.1635,NA),size = x)+#Walsh et al 2017 - Somerset 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.1265, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0855, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.1324,NA),size = x)+#Walsh et al 2017 - St Mary's 3 Year Average:Log:500-1000
  geom_function(fun =~ if_else(.x <= 300, 0.0793, NA),size = x)+ #Walsh et al 2017 - Talbot 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.1082, NA),size = x)+ #Walsh et al 2017 - Talbot 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , -0.0984,NA),size = x)+#Walsh et al 2017 - Talbot 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0751, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0869, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0878,NA),size = x) +#Walsh et al 2017 - Wicomico 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.1660*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0586*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0103*1.45/0.759,NA),size = x) +#Walsh et al 2017 - Anne Arundel 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0191*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0117*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0015*1.45/0.472,NA),size = x) +#Walsh et al 2017 - Baltimore county 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0133*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0247*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0237*1.45/0.929,NA),size = x) +#Walsh et al 2017 - Calvert 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0023*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0329*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0128*1.45/0.472,NA),size = x) +#Walsh et al 2017 - Cecil 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.2421*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.0670*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.1037*1.45/0.557,NA),size = x) +#Walsh et al 2017 - Charles 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0309*1.45/0.728, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0284*1.45/0.728, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0040*1.45/0.728,NA),size = x) +#Walsh et al 2017 - Dorchester 3 Year Average:500-1000
  geom_function(fun =~ if_else(.x <= 300, 0.0760*1.45/0.379, NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0066*1.45/0.379, NA),size = x)+  #Walsh et al 2017 - Harford 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0109*1.45/0.379,NA),size = x) +#Walsh et al 2017 - Harford 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0277*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0349*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0306*1.45/0.406,NA),size = x) +#Walsh et al 2017 - Kent 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.0227*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0399*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0439*1.45/0.470,NA),size = x) +#Walsh et al 2017 - Prince George's 3 Year Average:500-1000
  geom_function(fun =~ if_else(.x <= 300,  0.0402*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.0633*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0664*1.45/0.783,NA),size = x) +#Walsh et al 2017 - Queen Anne's 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0547*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0499*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:0-500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0761*1.45/0.683,NA),size = x) +#Walsh et al 2017 - Somerset 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  -0.0839*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0476*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , -0.0665*1.45/0.833,NA),size = x) +#Walsh et al 2017 - St Mary's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x <= 300,  0.0473*1.45/1.02, NA),size = x)+ #Walsh et al 2017 - St Talbot 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0149*1.45/1.02, NA),size = x)+ #Walsh et al 2017 - St Talbot 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0226*1.45/1.02,NA),size = x) +#Walsh et al 2017 - St Talbot 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0053*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0187*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0190*1.45/0.399,NA),size = x)+#Walsh et al 2017 - Wicomico 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 500, 2704.7*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 1750.7*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x <= 500, 782.4*0.00190,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 3196.1*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x <= 500, 2479.9*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x <= 500, 1301.7*0.00280,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x <= 500, 1424.9*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x <= 500, 410.4*-0.00500,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x <= 500, 312.7*0.00240,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x <= 500, 1071.3*0.00160,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x <= 500, 2006.9*0.00020,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x <= 500, 4367.5*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x <= 500, 1220.4*0.00370,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x <= 500, 282.8*0.00440,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x <= 500, 2357.8*0.00020,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x <= 500, 1115.7*0.00100,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x <= 500, 3038.6*0.00020,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x <= 500, 749.6*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x <= 500, 976.9*-0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x <= 500, 1376.1*0.00100,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x <= 500, 259.8*0.00820,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x <= 500, 38.8*0.09360,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x <= 500, 90.4*0.02720,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x <= 500, 1277.8*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x <= 500, 1327.7*-0.00020,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00480,NA),size = x)+ # Wolf et al 2022 - Model 27
  geom_function(fun =~ if_else(.x <= 500, 2704.7*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 1750.7*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x <= 500, 782.4*0.00180,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 3196.1*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x <= 500, 2479.9*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x <= 500, 1301.7*0.00290,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x <= 500, 1424.9*-0.00020,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x <= 500, 410.4*-0.00570,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x <= 500, 312.7*0.00200,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x <= 500, 1071.3*0.00100,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x <= 500, 2006.9*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x <= 500, 4367.5*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x <= 500, 1220.4*0.00370,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x <= 500, 282.8*0.00520,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x <= 500, 2357.8*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x <= 500, 1115.7*0.00140,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x <= 500, 3038.6*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x <= 500, 749.6*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x <= 500, 976.9*0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x <= 500, 1376.1*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x <= 500, 259.8*0.00940,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x <= 500, 38.8*0.09800,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x <= 500, 90.4*0.02600,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x <= 500, 1277.8*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x <= 500, 1327.7*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00350,NA),size = x)+ # Wolf et al 2022 - Model 27
  geom_function(fun =~ if_else(.x <= 500, 2704.7*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 1750.7*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x <= 500, 782.4*0.00120,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 3196.1*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x <= 500, 2479.9*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x <= 500, 1301.7*0.00540,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x <= 500, 1424.9*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x <= 500, 410.4*-0.01180,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x <= 500, 312.7*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x <= 500, 1071.3*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x <= 500, 2006.9*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x <= 500, 4367.5*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x <= 500, 1220.4*0.00510,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x <= 500, 282.8*0.00340,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x <= 500, 2357.8*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x <= 500, 1115.7*0.00180,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x <= 500, 3038.6*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x <= 500, 749.6*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x <= 500, 976.9*0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x <= 500, 1376.1*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x <= 500, 259.8*0.00850,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x <= 500, 38.8*0.08330,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x <= 500, 90.4*0.03240,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x <= 500, 1277.8*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x <= 500, 1327.7*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00150,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00590,NA),size = x)+ # Wolf et al 2022 - Model 27
  geom_function(fun =~ if_else(.x <= 500, 2704.7*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 1750.7*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x <= 500, 782.4*0.00190,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 3196.1*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x <= 500, 2479.9*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x <= 500, 1301.7*0.00280,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x <= 500, 1424.9*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x <= 500, 410.4*-0.00630,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x <= 500, 312.7*0.00180,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x <= 500, 1071.3*0.00160,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x <= 500, 2006.9*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x <= 500, 4367.5*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x <= 500, 1220.4*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x <= 500, 282.8*0.00470,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x <= 500, 2357.8*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x <= 500, 1115.7*0.00120,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x <= 500, 3038.6*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x <= 500, 749.6*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x <= 500, 976.9*0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x <= 500, 1376.1*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x <= 500, 259.8*0.00850,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x <= 500, 38.8*0.08330,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x <= 500, 90.4*0.03240,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x <= 500, 1277.8*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x <= 500, 1327.7*-0.00060,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00420,NA),size = x)+ # Wolf et al 2022 - Model 27
  ggtitle("Hedonic function with distance dummies")+
  xlab("Distance (Meters)") + ylab("Elasticity")              

p_build_1 = ggplot_build(p1)

plot_data_1 <-  as.data.frame(p_build_1$data[1:625])

df <- plot_data_1%>%
  select(matches('x|y'))%>%
  rename("dist" = "x")%>%
  select(!matches('linetype'))%>%
  select(!matches('x'))


df4 <- df %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "model",
    names_prefix = "e",
    values_to = "elast",
    values_drop_na = TRUE
  )


df4$model[which(df4$model == "y")] = 0

df4$model <- str_remove(df4$model, "y.")


model <- c(0:624)
study_name <- c("Guignet et al 2017-2.C",
                "Guignet et al 2017-2.C",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:Tract*year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:Tract*year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:Tract*year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:County*year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:County*year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:County*year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:State*year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:State*year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:State*year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:Tract+year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:Tract+year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:Tract+year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:County+year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:County+year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:County+year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:State+year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:State+year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:State+year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:Block*year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:Block*year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:Block*year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:Block+year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:Block+year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:Block+year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:year",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:tract",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:tract",
                "Mamun et al 2023-Model 1 - discrete distance bins/FE:tract",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S4:2",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S4:2",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S4:2",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S4:3",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S4:3",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S4:3",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S4:5",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S4:5",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S4:5",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S4:7",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S4:7",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S4:7",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S4:10",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S4:10",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S4:10",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S5:10",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S5:10",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S5:10",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S5:30",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S5:30",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S5:30",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S5:100",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S5:100",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S5:100",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S6:5",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S6:5",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S6:5",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:1",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:1",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:2",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:2",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:3",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:3",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:4",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:4",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:5",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:5",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:5",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:6",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:6",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:6",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:7",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:7",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:7",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:8",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:8",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:8",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:9",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:9",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S7:9",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:0",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:0",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:0",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:1",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:1",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:1",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:3",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:3",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:3",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:5",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:5",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:5",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:10",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:10",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:10",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:anytime",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:anytime",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S8:anytime",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S9:customer price index",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S9:customer price index",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S9:customer price index",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index",
                "Mamun et al 2023-Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index",
                "Mamun et al 2023-Model 2 - discrete distance bins/Table S10:1",
                "Mamun et al 2023-Model 2 - discrete distance bins/Table S10:1",
                "Mamun et al 2023-Model 2 - discrete distance bins/Table S10:1",
                "Mamun et al 2023-Model 2 - discrete distance bins/Table S10:2",
                "Mamun et al 2023-Model 2 - discrete distance bins/Table S10:2",
                "Mamun et al 2023-Model 2 - discrete distance bins/Table S10:2",
                "Mamun et al 2023-Model 2 - discrete distance bins/Table S10:4",
                "Mamun et al 2023-Model 2 - discrete distance bins/Table S10:4",
                "Mamun et al 2023-Model 2 - discrete distance bins/Table S10:4",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S11:ln(housing price)",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S11:ln(housing price)",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S11:ln(housing price)",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:CPL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:CPL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:CPL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:NAP",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:NAP",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:NAP",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:NPL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:NPL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:NPL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:SAP",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:SAP",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:SAP",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:SPL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:SPL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:SPL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:TPL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:TPL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:TPL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:WMT",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:WMT",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:WMT",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:XER",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:XER",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S12:ecoregion:XER",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:AL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:AL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:AL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:AR",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:AR",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:AR",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:AZ",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:AZ",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:AZ",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:CA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:CA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:CA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:CO",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:CO",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:CO",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:CT",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:CT",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:CT",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:DE",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:DE",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:DE",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:FL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:FL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:FL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:GA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:GA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:GA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:IA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:IA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:IA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:IL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:IL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:IL",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:IN",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:IN",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:IN",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:KY",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:KY",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:KY",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:LA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:LA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:LA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MD",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MD",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MD",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:ME",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:ME",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:ME",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MI",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MI",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MI",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MO",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MO",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MO",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MS",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MS",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MS",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MT",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MT",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:MT",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NC",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NC",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NC",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NE",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NE",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NE",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NH",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NH",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NH",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NJ",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NJ",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NJ",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NV",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NV",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NV",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NY",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NY",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:NY",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:OH",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:OH",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:OH",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:OK",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:OK",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:OK",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:OR",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:OR",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:OR",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:PA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:PA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:PA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:RI",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:RI",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:RI",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:SC",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:SC",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:SC",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:SD",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:SD",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:SD",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:TN",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:TN",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:TN",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:TX",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:TX",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:TX",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:UT",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:UT",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:UT",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:VA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:VA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:VA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:VT",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:VT",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:VT",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:WA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:WA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:WA",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:WI",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:WI",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:WI",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:WV",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:WV",
                "Mamun et al 2023-Model 3 - discrete distance bins/Table S13:state:WV",
                "Nepf et al 2022-Model 5 - discrete distance bins",
                "Nepf et al 2022-Model 5 - discrete distance bins",
                "Nepf et al 2022-Model 5 - discrete distance bins",
                "Nepf et al 2022-Model 5 - discrete distance bins",
                "Nepf et al 2022-Model 5 - discrete distance bins",
                "Nepf et al 2022-Model 6 - discrete distance bins",
                "Nepf et al 2022-Model 6 - discrete distance bins",
                "Nepf et al 2022-Model 6 - discrete distance bins",
                "Nepf et al 2022-Model 6 - discrete distance bins",
                "Nepf et al 2022-Model 6 - discrete distance bins",
                "Nepf et al 2022-Model 7 - discrete distance bins",
                "Nepf et al 2022-Model 7 - discrete distance bins",
                "Nepf et al 2022-Model 7 - discrete distance bins",
                "Nepf et al 2022-Model 7 - discrete distance bins",
                "Nepf et al 2022-Model 7 - discrete distance bins",
                "Nepf et al 2022-Model 8 - discrete distance bins",
                "Nepf et al 2022-Model 8 - discrete distance bins",
                "Nepf et al 2022-Model 8 - discrete distance bins",
                "Nepf et al 2022-Model 8 - discrete distance bins",
                "Nepf et al 2022-Model 8 - discrete distance bins",
                "Nepf et al 2022-Model 13 - discrete distance bins",
                "Nepf et al 2022-Model 13 - discrete distance bins",
                "Nepf et al 2022-Model 13 - discrete distance bins",
                "Nepf et al 2022-Model 13 - discrete distance bins",
                "Nepf et al 2022-Model 13 - discrete distance bins",
                "Nepf et al 2022-Model 14 - discrete distance bins",
                "Nepf et al 2022-Model 14 - discrete distance bins",
                "Nepf et al 2022-Model 14 - discrete distance bins",
                "Nepf et al 2022-Model 14 - discrete distance bins",
                "Nepf et al 2022-Model 14 - discrete distance bins",
                "Nepf et al 2022-Model 15 - discrete distance bins",
                "Nepf et al 2022-Model 15 - discrete distance bins",
                "Nepf et al 2022-Model 15 - discrete distance bins",
                "Nepf et al 2022-Model 15 - discrete distance bins",
                "Nepf et al 2022-Model 15 - discrete distance bins",
                "Nepf et al 2022-Model 16 - discrete distance bins",
                "Nepf et al 2022-Model 16 - discrete distance bins",
                "Nepf et al 2022-Model 16 - discrete distance bins",
                "Nepf et al 2022-Model 16 - discrete distance bins",
                "Nepf et al 2022-Model 16 - discrete distance bins",
                "TJ, Pat, Richard-Model 1 - discrete distance bin-500m",
                "TJ, Pat, Richard-Model 2 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 2 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 3 - discrete distance bin: 0-250m & 0-500m",
                "TJ, Pat, Richard-Model 3 - discrete distance bin: 0-250m & 0-500m",
                "TJ, Pat, Richard-Model 4 - discrete distance bin-500m",
                "TJ, Pat, Richard-Model 5 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 5 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 6 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 6 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 7 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 7 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 8 - discrete distance bin-500m",
                "TJ, Pat, Richard-Model 9 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 9 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 9 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 9 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 10 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 10 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 11 - discrete distance bins from 0-500m by 100m",
                "TJ, Pat, Richard-Model 11 - discrete distance bins from 0-500m by 100m",
                "TJ, Pat, Richard-Model 11 - discrete distance bins from 0-500m by 100m",
                "TJ, Pat, Richard-Model 11 - discrete distance bins from 0-500m by 100m",
                "TJ, Pat, Richard-Model 11 - discrete distance bins from 0-500m by 100m",
                "TJ, Pat, Richard-Model 12 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 12 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 13 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 13 - discrete distance bin: 0-250m & 250m-500m",
                "TJ, Pat, Richard-Model 14 - discrete distance bin-500m - different fixed effects",
                "TJ, Pat, Richard-Model 14 - discrete distance bin-500m - different fixed effects",
                "TJ, Pat, Richard-Model 14 - discrete distance bin-500m - different fixed effects",
                "TJ, Pat, Richard-Model 14 - discrete distance bin-500m - different fixed effects",
                "TJ, Pat, Richard-Model 14 - discrete distance bin-500m - different fixed effects",
                "TJ, Pat, Richard-Model 14 - discrete distance bin-500m - different fixed effects",
                "TJ, Pat, Richard-Model 20 - discrete distance bin-500m - property attributes",
                "TJ, Pat, Richard-Model 21 - discrete distance bin-500m - property attributes",
                "TJ, Pat, Richard-Model 22 - discrete distance bin-500m - property attributes",
                "Walsh et al 2011a-Model 1",
                "Walsh et al 2011a-Model 1",
                "Walsh et al 2011a-Model 1S",
                "Walsh et al 2011a-Model 1S",
                "Walsh et al 2017-Anne Arundel 1 Year Average - Log",
                "Walsh et al 2017-Anne Arundel 1 Year Average - Log",
                "Walsh et al 2017-Anne Arundel 1 Year Average - Log",
                "Walsh et al 2017-Baltimore County 1 Year Average - Log",
                "Walsh et al 2017-Baltimore County 1 Year Average - Log",
                "Walsh et al 2017-Baltimore County 1 Year Average - Log",
                'Walsh et al 2017-Calvert 1 Year Average - Log',
                "Walsh et al 2017-Calvert 1 Year Average - Log",
                "Walsh et al 2017-Calvert 1 Year Average - Log",
                "Walsh et al 2017-Cecil 1 Year Average - Log",
                "Walsh et al 2017-Cecil 1 Year Average - Log",
                "Walsh et al 2017-Cecil 1 Year Average - Log",
                "Walsh et al 2017-Charles 1 Year Average - Log",
                "Walsh et al 2017-Charles 1 Year Average - Log",
                "Walsh et al 2017-Charles 1 Year Average - Log",
                "Walsh et al 2017-Dorchester 1 Year Average - Log",
                "Walsh et al 2017-Dorchester 1 Year Average - Log",
                "Walsh et al 2017-Dorchester 1 Year Average - Log",
                "Walsh et al 2017-Harford 1 Year Average - Log",
                "Walsh et al 2017-Harford 1 Year Average - Log",
                "Walsh et al 2017-Harford 1 Year Average - Log",
                "Walsh et al 2017-Kent 1 Year Average - Log",
                "Walsh et al 2017-Kent 1 Year Average - Log",
                "Walsh et al 2017-Kent 1 Year Average - Log",
                "Walsh et al 2017-Prince George's 1 Year Average - Log",
                "Walsh et al 2017-Prince George's 1 Year Average - Log",
                "Walsh et al 2017-Prince George's 1 Year Average - Log",
                "Walsh et al 2017-Queen Anne's 1 Year Average - Log",
                "Walsh et al 2017-Queen Anne's 1 Year Average - Log",
                "Walsh et al 2017-Queen Anne's 1 Year Average - Log",
                "Walsh et al 2017-Somerset 1 Year Average - Log",
                "Walsh et al 2017-Somerset 1 Year Average - Log",
                "Walsh et al 2017-Somerset 1 Year Average - Log",
                "Walsh et al 2017-St Mary's 1 Year Average - Log",
                "Walsh et al 2017-St Mary's 1 Year Average - Log",
                "Walsh et al 2017-St Mary's 1 Year Average - Log",
                "Walsh et al 2017-Talbot 1 Year Average - Log",
                "Walsh et al 2017-Talbot 1 Year Average - Log",
                "Walsh et al 2017-Talbot 1 Year Average - Log",
                "Walsh et al 2017-Wicomico 1 Year Average - Log",
                "Walsh et al 2017-Wicomico 1 Year Average - Log",
                "Walsh et al 2017-Wicomico 1 Year Average - Log",
                "Walsh et al 2017-Anne Arundel 1 Year Average - level",
                "Walsh et al 2017-Anne Arundel 1 Year Average - level",
                "Walsh et al 2017-Anne Arundel 1 Year Average - level",
                "Walsh et al 2017-Baltimore County 1 Year Average - level",
                "Walsh et al 2017-Baltimore County 1 Year Average - level",
                "Walsh et al 2017-Baltimore County 1 Year Average - level",
                "Walsh et al 2017-Calvert 1 Year Average - level",
                "Walsh et al 2017-Calvert 1 Year Average - level",
                "Walsh et al 2017-Calvert 1 Year Average - level",
                "Walsh et al 2017-Cecil 1 Year Average - level",
                "Walsh et al 2017-Cecil 1 Year Average - level",
                "Walsh et al 2017-Cecil 1 Year Average - level",
                "Walsh et al 2017-Charles 1 Year Average - level",
                "Walsh et al 2017-Charles 1 Year Average - level",
                "Walsh et al 2017-Charles 1 Year Average - level",
                "Walsh et al 2017-Dorchester 1 Year Average - level",
                "Walsh et al 2017-Dorchester 1 Year Average - level",
                "Walsh et al 2017-Dorchester 1 Year Average - level",
                "Walsh et al 2017-Harford 1 Year Average - level",
                "Walsh et al 2017-Harford 1 Year Average - level",
                "Walsh et al 2017-Harford 1 Year Average - level",
                "Walsh et al 2017-Kent 1 Year Average - level",
                "Walsh et al 2017-Kent 1 Year Average - level",
                "Walsh et al 2017-Kent 1 Year Average - level",
                "Walsh et al 2017-Prince George's 1 Year Average - level",
                "Walsh et al 2017-Prince George's 1 Year Average - level",
                "Walsh et al 2017-Prince George's 1 Year Average - level",
                "Walsh et al 2017-Queen Anne's 1 Year Average - level",
                "Walsh et al 2017-Queen Anne's 1 Year Average - level",
                "Walsh et al 2017-Queen Anne's 1 Year Average - level",
                "Walsh et al 2017-Somerset 1 Year Average - level",
                "Walsh et al 2017-Somerset 1 Year Average - level",
                "Walsh et al 2017-Somerset 1 Year Average - level",
                "Walsh et al 2017-St Mary's 1 Year Average - level",
                "Walsh et al 2017-St Mary's 1 Year Average - level",
                "Walsh et al 2017-St Mary's 1 Year Average - level",
                "Walsh et al 2017-Talbot 1 Year Average - level",
                "Walsh et al 2017-Talbot 1 Year Average - level",
                "Walsh et al 2017-Talbot 1 Year Average - level",
                "Walsh et al 2017-Wicomico 1 Year Average - level",
                "Walsh et al 2017-Wicomico 1 Year Average - level",
                "Walsh et al 2017-Wicomico 1 Year Average - level",
                "Walsh et al 2017-Anne Arundel 3 Year Average - Log",
                "Walsh et al 2017-Anne Arundel 3 Year Average - Log",
                "Walsh et al 2017-Anne Arundel 3 Year Average - Log",
                "Walsh et al 2017-Baltimore County 3 Year Average - Log",
                "Walsh et al 2017-Baltimore County 3 Year Average - Log",
                "Walsh et al 2017-Baltimore County 3 Year Average - Log",
                "Walsh et al 2017-Calvert 3 Year Average - Log",
                "Walsh et al 2017-Calvert 3 Year Average - Log",
                "Walsh et al 2017-Calvert 3 Year Average - Log",
                "Walsh et al 2017-Cecil 3 Year Average - Log",
                "Walsh et al 2017-Cecil 3 Year Average - Log",
                "Walsh et al 2017-Cecil 3 Year Average - Log",
                "Walsh et al 2017-Charles 3 Year Average - Log",
                "Walsh et al 2017-Charles 3 Year Average - Log",
                "Walsh et al 2017-Charles 3 Year Average - Log",
                "Walsh et al 2017-Dorchester 3 Year Average - Log",
                "Walsh et al 2017-Dorchester 3 Year Average - Log",
                "Walsh et al 2017-Dorchester 3 Year Average - Log",
                "Walsh et al 2017-Harford 3 Year Average - Log",
                "Walsh et al 2017-Harford 3 Year Average - Log",
                "Walsh et al 2017-Harford 3 Year Average - Log",
                "Walsh et al 2017-Kent 3 Year Average - Log",
                "Walsh et al 2017-Kent 3 Year Average - Log",
                "Walsh et al 2017-Kent 3 Year Average - Log",
                "Walsh et al 2017-Prince George's 3 Year Average - Log",
                "Walsh et al 2017-Prince George's 3 Year Average - Log",
                "Walsh et al 2017-Prince George's 3 Year Average - Log",
                "Walsh et al 2017-Queen Anne's 3 Year Average - Log",
                "Walsh et al 2017-Queen Anne's 3 Year Average - Log",
                "Walsh et al 2017-Queen Anne's 3 Year Average - Log",
                "Walsh et al 2017-Somerset 3 Year Average - Log",
                "Walsh et al 2017-Somerset 3 Year Average - Log",
                "Walsh et al 2017-Somerset 3 Year Average - Log",
                "Walsh et al 2017-St Mary's 3 Year Average - Log",
                "Walsh et al 2017-St Mary's 3 Year Average - Log",
                "Walsh et al 2017-St Mary's 3 Year Average - Log",
                "Walsh et al 2017-Talbot 3 Year Average - Log",
                "Walsh et al 2017-Talbot 3 Year Average - Log",
                "Walsh et al 2017-Talbot 3 Year Average - Log",
                "Walsh et al 2017-Wicomico 3 Year Average - Log",
                "Walsh et al 2017-Wicomico 3 Year Average - Log",
                "Walsh et al 2017-Wicomico 3 Year Average - Log",
                "Walsh et al 2017-Anne Arundel 3 Year Average - level",
                "Walsh et al 2017-Anne Arundel 3 Year Average - level",
                "Walsh et al 2017-Anne Arundel 3 Year Average - level",
                "Walsh et al 2017-Baltimore County 3 Year Average - level",
                "Walsh et al 2017-Baltimore County 3 Year Average - level",
                "Walsh et al 2017-Baltimore County 3 Year Average - level",
                "Walsh et al 2017-Calvert 3 Year Average - level",
                "Walsh et al 2017-Calvert 3 Year Average - level",
                "Walsh et al 2017-Calvert 3 Year Average - level",
                "Walsh et al 2017-Cecil 3 Year Average - level",
                "Walsh et al 2017-Cecil 3 Year Average - level",
                "Walsh et al 2017-Cecil 3 Year Average - level",
                "Walsh et al 2017-Charles 3 Year Average - level",
                "Walsh et al 2017-Charles 3 Year Average - level",
                "Walsh et al 2017-Charles 3 Year Average - level",
                "Walsh et al 2017-Dorchester 3 Year Average - level",
                "Walsh et al 2017-Dorchester 3 Year Average - level",
                "Walsh et al 2017-Dorchester 3 Year Average - level",
                "Walsh et al 2017-Harford 3 Year Average - level",
                "Walsh et al 2017-Harford 3 Year Average - level",
                "Walsh et al 2017-Harford 3 Year Average - level",
                "Walsh et al 2017-Kent 3 Year Average - level",
                "Walsh et al 2017-Kent 3 Year Average - level",
                "Walsh et al 2017-Kent 3 Year Average - level",
                "Walsh et al 2017-Prince George's 3 Year Average - level",
                "Walsh et al 2017-Prince George's 3 Year Average - level",
                "Walsh et al 2017-Prince George's 3 Year Average - level",
                "Walsh et al 2017-Queen Anne's 3 Year Average - level",
                "Walsh et al 2017-Queen Anne's 3 Year Average - level",
                "Walsh et al 2017-Queen Anne's 3 Year Average - level",
                "Walsh et al 2017-Somerset 3 Year Average - level",
                "Walsh et al 2017-Somerset 3 Year Average - level",
                "Walsh et al 2017-Somerset 3 Year Average - level",
                "Walsh et al 2017-St Mary's 3 Year Average - level",
                "Walsh et al 2017-St Mary's 3 Year Average - level",
                "Walsh et al 2017-St Mary's 3 Year Average - level",
                "Walsh et al 2017-Talbot 3 Year Average - level",
                "Walsh et al 2017-Talbot 3 Year Average - level",
                "Walsh et al 2017-Talbot 3 Year Average - level",
                "Walsh et al 2017-Wicomico 3 Year Average - level",
                "Walsh et al 2017-Wicomico 3 Year Average - level",
                "Walsh et al 2017-Wicomico 3 Year Average - level",
                "Wolf et al 2022-Model 1",
                "Wolf et al 2022-Model 2",
                "Wolf et al 2022-Model 3",
                "Wolf et al 2022-Model 4",
                "Wolf et al 2022-Model 5",
                "Wolf et al 2022-Model 6",
                "Wolf et al 2022-Model 7",
                "Wolf et al 2022-Model 8",
                "Wolf et al 2022-Model 9",
                "Wolf et al 2022-Model 10",
                "Wolf et al 2022-Model 11",
                "Wolf et al 2022-Model 12",
                "Wolf et al 2022-Model 13",
                "Wolf et al 2022-Model 14",
                "Wolf et al 2022-Model 15",
                "Wolf et al 2022-Model 16",
                "Wolf et al 2022-Model 17",
                "Wolf et al 2022-Model 18",
                "Wolf et al 2022-Model 19",
                "Wolf et al 2022-Model 20",
                "Wolf et al 2022-Model 21",
                "Wolf et al 2022-Model 22",
                "Wolf et al 2022-Model 23",
                "Wolf et al 2022-Model 24",
                "Wolf et al 2022-Model 25",
                "Wolf et al 2022-Model 26",
                "Wolf et al 2022-Model 27",
                "Wolf et al 2022-Model 1",
                "Wolf et al 2022-Model 2",
                "Wolf et al 2022-Model 3",
                "Wolf et al 2022-Model 4",
                "Wolf et al 2022-Model 5",
                "Wolf et al 2022-Model 6",
                "Wolf et al 2022-Model 7",
                "Wolf et al 2022-Model 8",
                "Wolf et al 2022-Model 9",
                "Wolf et al 2022-Model 10",
                "Wolf et al 2022-Model 11",
                "Wolf et al 2022-Model 12",
                "Wolf et al 2022-Model 13",
                "Wolf et al 2022-Model 14",
                "Wolf et al 2022-Model 15",
                "Wolf et al 2022-Model 16",
                "Wolf et al 2022-Model 17",
                "Wolf et al 2022-Model 18",
                "Wolf et al 2022-Model 19",
                "Wolf et al 2022-Model 20",
                "Wolf et al 2022-Model 21",
                "Wolf et al 2022-Model 22",
                "Wolf et al 2022-Model 23",
                "Wolf et al 2022-Model 24",
                "Wolf et al 2022-Model 25",
                "Wolf et al 2022-Model 26",
                "Wolf et al 2022-Model 27",
                "Wolf et al 2022-Model 1",
                "Wolf et al 2022-Model 2",
                "Wolf et al 2022-Model 3",
                "Wolf et al 2022-Model 4",
                "Wolf et al 2022-Model 5",
                "Wolf et al 2022-Model 6",
                "Wolf et al 2022-Model 7",
                "Wolf et al 2022-Model 8",
                "Wolf et al 2022-Model 9",
                "Wolf et al 2022-Model 10",
                "Wolf et al 2022-Model 11",
                "Wolf et al 2022-Model 12",
                "Wolf et al 2022-Model 13",
                "Wolf et al 2022-Model 14",
                "Wolf et al 2022-Model 15",
                "Wolf et al 2022-Model 16",
                "Wolf et al 2022-Model 17",
                "Wolf et al 2022-Model 18",
                "Wolf et al 2022-Model 19",
                "Wolf et al 2022-Model 20",
                "Wolf et al 2022-Model 21",
                "Wolf et al 2022-Model 22",
                "Wolf et al 2022-Model 23",
                "Wolf et al 2022-Model 24",
                "Wolf et al 2022-Model 25",
                "Wolf et al 2022-Model 26",
                "Wolf et al 2022-Model 27",
                "Wolf et al 2022-Model 1",
                "Wolf et al 2022-Model 2",
                "Wolf et al 2022-Model 3",
                "Wolf et al 2022-Model 4",
                "Wolf et al 2022-Model 5",
                "Wolf et al 2022-Model 6",
                "Wolf et al 2022-Model 7",
                "Wolf et al 2022-Model 8",
                "Wolf et al 2022-Model 9",
                "Wolf et al 2022-Model 10",
                "Wolf et al 2022-Model 11",
                "Wolf et al 2022-Model 12",
                "Wolf et al 2022-Model 13",
                "Wolf et al 2022-Model 14",
                "Wolf et al 2022-Model 15",
                "Wolf et al 2022-Model 16",
                "Wolf et al 2022-Model 17",
                "Wolf et al 2022-Model 18",
                "Wolf et al 2022-Model 19",
                "Wolf et al 2022-Model 20",
                "Wolf et al 2022-Model 21",
                "Wolf et al 2022-Model 22",
                "Wolf et al 2022-Model 23",
                "Wolf et al 2022-Model 24",
                "Wolf et al 2022-Model 25",
                "Wolf et al 2022-Model 26",
                "Wolf et al 2022-Model 27")




df_studname <- data_frame(model, study_name)

df4$model <- as.numeric(as.character(df4$model))

df4 <- df4%>%
  left_join(df_studname)

library(stringr)

df4[c('model_name', 'study_name')] <- str_split_fixed(df4$study_name, '-', 2)


df4 <- df4%>%
  rename("study_name" = "model_name",
         "model name" = "study_name")%>%
  mutate(distance_spec = "Discrete Distance")

ggplot(df4, aes(dist, elast)) +
  geom_point(size=0.005)+
  geom_smooth()



# Select subset where elasticity vary between 0-0.5


p_distdum <- ggplot(df4, aes(dist, elast)) +
  geom_function(fun =~ if_else(.x <= 200, -0.099399999*-1,NA),size = x)+ # Guignet et al 2017 - 2.C:0_200m
  geom_function(fun =~ if_else(.x >= 200 & .x <= 500, -0.0058*-1,NA),size = x)+ # Guignet et al 2017 - 2.C:200_500m
  geom_function(fun =~ if_else(.x <= 100,0.0076 + 0.1673, NA),size = x)+# Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0199 + 0.1778, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County*year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0199 + 0.0435, NA),size = x)+ # Mamun et al 2023:Model 1 - discrete distance bins/FE:County*year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0199,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County*year:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0383 + 0.1758, NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/FE:State*year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0383 + 0.0477, NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/FE:State*year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0383,NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/FE:State*year:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0105 + 0.1674, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract+year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0105 + 0.0437, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract+year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0105,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract+year:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0141 + 0.1804, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County+year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0141 + 0.0475, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County+year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0141,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County+year:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0343 + 0.1797, NA),size = x)+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:State+year)
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0343 + 0.0514, NA),size = x)+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:State+year)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0343,NA),size = x)+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:State+year)
  geom_function(fun =~ if_else(.x <= 100, 0.0287 + 0.1581, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Block*year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0287 + 0.0388, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Block*year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0287,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Block*year:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0108 + 0.1568, NA),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/FE:Block+year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0108 + 0.0413, NA),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/FE:Block+year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0108,NA),size = x)+  # Mamun et al 2023- Model 1:discrete distance bins/FE:Block+year:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0051 + 0.2315, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0051 + 0.0726, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0051,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:year:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0123 + 0.1678, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:tract:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0123 + 0.0439, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:tract:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0123,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:tract:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0085 + 0.1573, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:2:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0085 + 0.0414, NA),size = x)+  # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:2:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0085,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:2:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0072 + 0.1532, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:3:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0072 + 0.0401, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:3:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0072,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:3:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0136 + 0.1586, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:5:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0136 + 0.0455, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:5:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0136,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:5:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0101 + 0.1666, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:7:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0101 + 0.055, NA),size = x)+  # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:7:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0101,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:7:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0018 + 0.1377, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:10:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0018 + 0.0497, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:10:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0018,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:10:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0108 + 0.1654, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:10:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0108 + 0.0417, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:10:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0108,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:10:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0225 + 0.1582, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:30:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0225 + 0.0472, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:30:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0225,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:30:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0231 + 0.129, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:100:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0231 + 0.0461, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:100:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0231,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:300:2000
  geom_function(fun =~ if_else(.x <= 100, 0.0104 + 0.1461, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S6:5:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0104 + 0.0364, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S6:5:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0104,NA),size = x)+  # Mamun et al 2023 - Model 1:discrete distance bins/Table S6:5:300_2000
  geom_function(fun =~ if_else(.x <= 300, 0.0123 + 0.1563, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:1:waterfront we define 300m(1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0123,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:1:waterfront we define 300m(0)
  geom_function(fun =~ if_else(.x <= 50, 0.0424 + 0.0953, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:2:watefront 50m(1)
  geom_function(fun =~ if_else(.x >= 50 & .x <= 2000 , 0.0424,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:2:watefront 50m(0)
  geom_function(fun =~ if_else(.x <= 100, 0.0157 + 0.151, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:3):watefront 100m(1)
  geom_function(fun =~ if_else(.x >= 100 & .x <= 2000 , 0.0157,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:3):watefront 100m(0)
  geom_function(fun =~ if_else(.x <= 200, 0.0076 + 0.1296, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:4:watefront 200m(1)
  geom_function(fun =~ if_else(.x >= 200 & .x <= 2000 , 0.0076,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:4:watefront 200m(0)
  geom_function(fun =~ if_else(.x <= 50, 0.022 + 0.1439, NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/Table S7:5:0_50
  geom_function(fun =~ if_else(.x >= 50 & .x <= 200 , 0.022 + 0.1134,NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/Table S7:5:50_200
  geom_function(fun =~ if_else(.x >= 200 & .x <= 2000 , 0.022, NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/Table S7:5:200_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0104 + 0.1636, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:6:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 200 , 0.0104 + 0.0507,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:6:100_200
  geom_function(fun =~ if_else(.x >= 200 & .x <= 2000 , 0.0104, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:6:200_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:7:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:7:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:7:300_2000
  geom_function(fun =~ if_else(.x <= 200, 0.0037 + 0.1359, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:8:0_200
  geom_function(fun =~ if_else(.x >= 200 & .x <= 500 , 0.0037 + 0.0132,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:8:200_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 2000 , 0.0037, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:8:500_2000
  geom_function(fun =~ if_else(.x <= 300, 0.0019 + 0.1089, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:9:0_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 1000 , 0.0019 + 0.0031,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:9:300_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 2000 , 0.0019, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:9:100_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0041 + 0.155, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:0:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0041 + 0.041,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:0:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0041, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:0:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0166 + 0.1579, NA),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/Table S8:1:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0166 + 0.0375,NA),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/Table S8:1:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0166, NA),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/Table S8:1:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0121 + 0.1645, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:3:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0121 + 0.0393,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:3:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0121, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:3:300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:5:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+  # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:5:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:5:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0072 + 0.1688, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:10:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0072 + 0.041,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:10:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0072, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:10:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0078 + 0.1728, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:anytime:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0078 + 0.0382,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:anytime:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0078, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:anytime:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S9:customer price index:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S9:customer price index:100-300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076, NA),size = x)+  # Mamun et al 2023 - Model 1:discrete distance bins/Table S9:customer price index:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0072 + 0.1532, NA),size = x)+ # Mamun et al 2023 - Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0072 + 0.0401,NA),size = x)+  # Mamun et al 2023 - Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0072, NA),size = x)+ # Mamun et al 2023 - Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0126 + 0.1148, NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:1:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0126 + 0.0184,NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:1:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0126, NA),size = x)+  # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:1:300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0173 + 0.1676, NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:2:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0173 + 0.0401,NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:2:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0401, NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:2:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:4:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:4:100-300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076, NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:4:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.007 + 0.0789, NA),size = x)+ # Mamun et al 2023 -Model 3:discrete distance bins/Table S11:ln(housing price)):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.007 + 0.0227,NA),size = x)+ # Mamun et al 2023 -Model 3:discrete distance bins/Table S11:ln(housing price)):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.007, NA),size = x)+ # Mamun et al 2023 -Model 3:discrete distance bins/Table S11:ln(housing price)):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.0469, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:CPL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204  + 0.0155,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:CPL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:CPL:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + 0.0824, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NAP:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + 0.0333,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NAP:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NAP:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.2392, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NPL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + -0.0445,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NPL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NPL:300_1000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + 0.1601, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SAP):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + 0.1002,NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SAP):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SAP):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.1651, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SPL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + -0.0836,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SPL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SPL:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.1553, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:TPL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + -0.0477,NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:TPL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:TPL:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + 0.0551, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:WMT:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + 0.0303,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:WMT:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:WMT:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.0765, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:XER:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + 0.0002,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:XER:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:XER:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1364, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0949,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AL:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0547, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AR:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.207,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AR:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AR:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.1572, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AZ:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0368,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AZ:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AZ:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1312, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CA):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0552,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CA):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CA):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.2389, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CO):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0367,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CO):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CO):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.1809, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CT):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.065,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CT):100-300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CT):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.5047, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:DE):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.2419,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:DE):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:DE):300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.1284, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:FL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0018,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:FL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:FL:300:2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.5163, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:GA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.2384,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:GA:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:GA:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0758, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.1,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IA:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IA:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.3359, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.2071,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IL:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0794, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IN:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.039,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IN:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IN:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 1.2532, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:KY:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0888,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:KY:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:KY:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0273, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:LA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0462,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:LA:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:LA:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0285, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.059,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MA:100-300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MA:300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.1581, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MD:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0346,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MD:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MD:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1299, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:ME:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.1871,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:ME:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:ME:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0913, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MI:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0146,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MI:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MI:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0672, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MO):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.2849,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MO):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MO):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.3935, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MS:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.6851,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MS:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MS:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.4651, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MT:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.3235,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MT:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MT:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.108, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NC):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.047,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NC):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NC):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0542, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NE:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.1447,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NE:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NE:300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0968, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NH:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0315,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NH:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NH:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.2329, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NJ:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0775,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NJ:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NJ:300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0915, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NV:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0197,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NV:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NV:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1677, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NY:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.074,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NY:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NY:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.3565, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OH:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.051,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OH:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OH:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.3811, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OK:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.3099,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OK:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OK:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0729, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OR:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0604,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OR:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OR:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0623, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:PA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0366,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:PA:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:PA:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.4243, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:RI:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0761,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:RI:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:RI:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.2388, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SC:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.1485,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SC:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SC:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0699, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SD:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0298,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SD:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SD:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1608, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TN:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.1244,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TN:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TN:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1807, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TX:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0074,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TX:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TX:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.9602, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:UT:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0612,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:UT:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:UT:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.3189, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VA):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0059,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VA):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VA):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0231, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VT:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0491,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VT:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VT:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0623, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0217,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WA:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WA:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0069, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WI:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0208,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WI:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WI:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0314, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WV:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.1154,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WV:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WV:300_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0190*1 + 0.0138*0 + 0.0083*0 + 0.0095*0 + -0.0003*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0190*0 + 0.0138*1 + 0.0083*0 + 0.0095*0 + -0.0003*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0190*0 + 0.0138*0 + 0.0083*1 + 0.0095*0 + -0.0003*0)*3.28*1.86,NA),size = x)+  # Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0190*0 + 0.0138*0 + 0.0083*0 + 0.0095*1 + -0.0003*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0190*0 + 0.0138*0 + 0.0083*0 + 0.0095*1 + -0.0003*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:1500:2000
  geom_function(fun =~ if_else(.x <= 300,(0.0088 + 0.0108*1 + 0.0110*0 + 0.0077*0 + 0.0088*0 + 0.0040*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 6:discrete distance bins:Town FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0088 + 0.0108*0 + 0.0110*1 + 0.0077*0 + 0.0088*0 + 0.0040*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 6:discrete distance bins:Town FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*1 + 0.0088*0 + 0.0040*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 6:discrete distance bins:Town FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*0 + 0.0088*1 + 0.0040*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 6:discrete distance bins:Town FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*0 + 0.0088*0 + 0.0040*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 6:discrete distance bins:Town FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0051 + 0.0155*1 + 0.0139*0 + 0.0116*0 + 0.0132*0 + 0.0046*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 7:discrete distance bins:School FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0051 + 0.0155*0 + 0.0139*1 + 0.0116*0 + 0.0132*0 + 0.0046*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 7:discrete distance bins:School FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*1 + 0.0132*0 + 0.0046*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 7:discrete distance bins:School FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*0 + 0.0132*1 + 0.0046*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 7:discrete distance bins:School FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*0 + 0.0132*0 + 0.0046*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 7:discrete distance bins:School FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0090 + 0.0103*1 + 0.0118*0 + 0.0084*0 + 0.0094*0 + 0.0051*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0090 + 0.0103*0 + 0.0118*1 + 0.0084*0 + 0.0094*0 + 0.0051*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*1 + 0.0094*0 + 0.0051*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*0 + 0.0094*1 + 0.0051*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*0 + 0.0094*0 + 0.0051*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0050 + 0.0260*1 + 0.0096*0 + -0.0141*0 + -0.0056*0 + -0.0079*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0050 + 0.0260*0 + 0.0096*1 + -0.0141*0 + -0.0056*0 + -0.0079*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*1 + -0.0056*0 + -0.0079*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*0 + -0.0056*1 + -0.0079*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*0 + -0.0056*0 + -0.0079*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0072 + 0.0170*1 + -0.0048*0 + -0.0193*0 + -0.0007*0 + -0.0167*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0072 + 0.0170*0 + -0.0048*1 + -0.0193*0 + -0.0007*0 + -0.0167*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*1 + -0.0007*0 + -0.0167*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*0 + -0.0007*1 + -0.0167*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*0 + -0.0007*0 + -0.0167*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0098 + 0.0248*1 + 0.0114*0 + -0.0144*0 + -0.0011*0 + -0.0144*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0098 + 0.0248*0 + 0.0114*1 + -0.0144*0 + -0.0011*0 + -0.0144*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*1 + -0.0011*0 + -0.0144*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*0 + -0.0011*1 + -0.0144*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*0 + -0.0011*0 + -0.0144*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0095 + 0.0156*1 + -0.0063*0 + -0.0205*0 + -0.0027*0 + -0.0184*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0095 + 0.0156*0 + -0.0063*1 + -0.0205*0 + -0.0027*0 + -0.0184*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*1 + -0.0027*0 + -0.0184*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*0 + -0.0027*1 + -0.0184*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*0 + -0.0027*0 + -0.0184*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:1500_2000
  geom_function(fun =~ if_else(.x <= 500, 0.06, NA),size = x)+ # TJ, Pat, Richard - Model 1:discrete distance bin:500
  geom_function(fun =~ if_else(.x <= 250, 0.063, NA),size = x)+# TJ, Pat, Richard - Model 2:discrete distance bin: 0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 , 0.056,NA),size = x)+ # TJ, Pat, Richard - Model 2:discrete distance bin: 250_500
  geom_function(fun =~ if_else(.x <= 250, 0.056, NA),size = x)+# TJ, Pat, Richard - Model 3:discrete distance bin: 0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 , 0.007,NA),size = x)+ # TJ, Pat, Richard - Model 3:discrete distance bin: 250_500
  geom_function(fun =~ if_else(.x <= 500, 0.06, NA),size = x)+ # TJ, Pat, Richard - Model 4:discrete distance bin:500
  geom_function(fun =~ if_else(.x <= 250, 0.061, NA),size = x)+# TJ, Pat, Richard - Model 5:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 , 0.059,NA),size = x)+ # TJ, Pat, Richard - Model 5:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 250, 0.087, NA),size = x)+# TJ, Pat, Richard - Model 6:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 , 0.063,NA),size = x)+ # TJ, Pat, Richard - Model 6:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 250, 0.063, NA),size = x)+# TJ, Pat, Richard - Model 7:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 , 0.056,NA),size = x)+ # TJ, Pat, Richard - Model 7:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 500, 0.044, NA),size = x)+ # TJ, Pat, Richard - Model 8:discrete distance bin:500
  geom_function(fun =~ if_else(.x <= 250, 0.041, NA),size = x)+# TJ, Pat, Richard - Model 9:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 ,0.048,NA),size = x)+ # TJ, Pat, Richard - Model 9:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 250, 0.041, NA),size = x)+# TJ, Pat, Richard - Model 9:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 ,0.048,NA),size = x)+ # TJ, Pat, Richard - Model 9:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 250, 0.152, NA),size = x)+# TJ, Pat, Richard - Model 10:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 ,0.083,NA),size = x)+# TJ, Pat, Richard - Model 10:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 100, 0.072, NA),size = x)+# TJ, Pat, Richard - Model 11:discrete distance bin:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 200 , 0.062,NA),size = x)+ # TJ, Pat, Richard - Model 11:discrete distance bin:100_200
  geom_function(fun =~ if_else(.x >=200 & .x <= 300 , 0.053,NA),size = x)+ # TJ, Pat, Richard - Model 11:discrete distance bin:200_300
  geom_function(fun =~ if_else(.x >=300 & .x <= 400 , 0.056,NA),size = x)+ # TJ, Pat, Richard - Model 11:discrete distance bin:300_400
  geom_function(fun =~ if_else(.x >=400 & .x <= 500 , 0.062,NA),size = x)+ # TJ, Pat, Richard - Model 11:discrete distance bin:400_500
  geom_function(fun =~ if_else(.x <= 250, 0.058, NA),size = x)+# TJ, Pat, Richard - Model 12:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 ,0.056,NA),size = x)+ # TJ, Pat, Richard - Model 12:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 250, 0.124, NA),size = x)+# TJ, Pat, Richard - Model 13:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 ,0.074,NA),size = x)+ # TJ, Pat, Richard - Model 13:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 500, 0.031, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.024, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.029, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.018, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.064, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.06, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, -0.04, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.01, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.03, NA),size = x) + # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 50, 0.017 + 0.115, NA),size = x)+# Walsh et al 2011a - Model 1:50
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467 ,0.017,NA),size = x)+# Walsh et al 2011a - Model 5-_467
  geom_function(fun =~ if_else(.x <= 50, 0.017 + 0.11, NA),size = x)+# Walsh et al 2011a - Model 1S:50
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467 ,0.017,NA),size = x) +# Walsh et al 2011a - Model 1S:50_467
  geom_function(fun =~ if_else(.x <= 300, 0.126, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.023, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.009,NA),size = x)+#Walsh et al 2017 - Anne Arundel 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.090, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , -0.009, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:Log:500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  0.015,NA),size = x)+#Walsh et al 2017 - Baltimore county 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.033, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , -0.001, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  -0.021,NA),size = x)+#Walsh et al 2017 - Calvert 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.010, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.001, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  -0.003,NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.058, NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.056, NA),size = x)+  #Walsh et al 2017 - Charles 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  0.107,NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.078, NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.008, NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   0.013,NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.096, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.001, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.012,NA),size = x)+#Walsh et al 2017 - Harford 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.142, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.008, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.002,NA),size = x)+#Walsh et al 2017 - Kent 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.062, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.001, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.022,NA),size = x)+#Walsh et al 2017 - Prince George's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.017, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.060, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   0.068,NA),size = x)+#Walsh et al 2017 - Queen Anne's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.091, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.055, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.141,NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.014, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.015, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.017,NA),size = x)+#Walsh et al 2017 - St Mary's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.156, NA),size = x)+ #Walsh et al 2017 - Talbot 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.014, NA),size = x)+ #Walsh et al 2017 - Talbot 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.031,NA),size = x)+#Walsh et al 2017 - Talbot 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.046, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.015, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.010,NA),size = x) +#Walsh et al 2017 - Wicomico 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0585*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0249*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0089*1.45/0.759,NA),size = x) +#Walsh et al 2017 - Anne Arundel 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0293*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0032*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0060*1.45/0.472,NA),size = x) + #Walsh et al 2017 - Baltimore county 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0088*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0174*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0196*1.45/0.929,NA),size = x) +#Walsh et al 2017 - Calvert 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.0024*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0086*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0012*1.45/0.472,NA),size = x) + #Walsh et al 2017 - Cecil 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.041*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0252*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0335*1.45/0.557,NA),size = x) +#Walsh et al 2017 - Charles 1 Year Average:500-1000
  geom_function(fun =~ if_else(.x <= 300, 0.0557*1.45/0.728, NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0076*1.45/0.728, NA),size = x)+  #Walsh et al 2017 - Dorchester 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0079*1.45/0.728,NA),size = x) +#Walsh et al 2017 - Dorchester 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0243*1.45/0.379, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0022*1.45/0.379, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0022*1.45/0.379,NA),size = x) +#Walsh et al 2017 - Harford 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0289*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0120*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0049*1.45/0.406,NA),size = x) +#Walsh et al 2017 - Kent 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0093*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0018*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0023*1.45/0.470,NA),size = x) + #Walsh et al 2017 - Prince George's 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0151*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.041*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0470*1.45/0.783,NA),size = x) +#Walsh et al 2017 - Queen Anne's 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0300*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0207*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0498*1.45/0.683,NA),size = x) +#Walsh et al 2017 - Somerset 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  -0.0375*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0082*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , -0.0115*1.45/0.833,NA),size = x) +#Walsh et al 2017 - St Mary's 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0631*1.45/1.02, NA),size = x)+ #Walsh et al 2017 - St Talbot 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0122*1.45/1.02, NA),size = x)+  #Walsh et al 2017 - St Talbot 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0190*1.45/1.02,NA),size = x) + #Walsh et al 2017 - St Talbot 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0018*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0130*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:0:500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0116*1.45/0.399,NA),size = x) +#Walsh et al 2017 - Wicomico 1 Year Average:500:100
  geom_function(fun =~ if_else(.x <= 300, 0.3058, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.1020, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0123,NA),size = x)+#Walsh et al 2017 - Anne Arundel 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.05560, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , -0.0386, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  0.0077,NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.0134, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , -0.0779, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  -0.0653,NA),size = x)+  #Walsh et al 2017 - Calvert 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0010, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , -0.1257, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  -0.0362,NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.6413, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.1764, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  0.3021,NA),size = x)+#Walsh et al 2017 - Charles 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0607, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0429, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0053,NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.2600, NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0213, NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.0370,NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0745, NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.1147, NA),size = x)+  #Walsh et al 2017 - Kent 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.1083,NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.0090, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.1411, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.1427,NA),size = x)+#Walsh et al 2017 - Prince George's 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.1310, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.1838, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.1983,NA),size = x)+#Walsh et al 2017 - Queen Anne's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0839, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0632, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.1635,NA),size = x)+#Walsh et al 2017 - Somerset 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.1265, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0855, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.1324,NA),size = x)+#Walsh et al 2017 - St Mary's 3 Year Average:Log:500-1000
  geom_function(fun =~ if_else(.x <= 300, 0.0793, NA),size = x)+ #Walsh et al 2017 - Talbot 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.1082, NA),size = x)+ #Walsh et al 2017 - Talbot 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , -0.0984,NA),size = x)+#Walsh et al 2017 - Talbot 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0751, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0869, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0878,NA),size = x) +#Walsh et al 2017 - Wicomico 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.1660*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0586*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0103*1.45/0.759,NA),size = x) +#Walsh et al 2017 - Anne Arundel 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0191*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0117*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0015*1.45/0.472,NA),size = x) +#Walsh et al 2017 - Baltimore county 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0133*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0247*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0237*1.45/0.929,NA),size = x) +#Walsh et al 2017 - Calvert 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0023*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0329*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0128*1.45/0.472,NA),size = x) +#Walsh et al 2017 - Cecil 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.2421*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.0670*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.1037*1.45/0.557,NA),size = x) +#Walsh et al 2017 - Charles 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0309*1.45/0.728, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0284*1.45/0.728, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0040*1.45/0.728,NA),size = x) +#Walsh et al 2017 - Dorchester 3 Year Average:500-1000
  geom_function(fun =~ if_else(.x <= 300, 0.0760*1.45/0.379, NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0066*1.45/0.379, NA),size = x)+  #Walsh et al 2017 - Harford 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0109*1.45/0.379,NA),size = x) +#Walsh et al 2017 - Harford 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0277*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0349*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0306*1.45/0.406,NA),size = x) +#Walsh et al 2017 - Kent 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.0227*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0399*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0439*1.45/0.470,NA),size = x) +#Walsh et al 2017 - Prince George's 3 Year Average:500-1000
  geom_function(fun =~ if_else(.x <= 300,  0.0402*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.0633*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0664*1.45/0.783,NA),size = x) +#Walsh et al 2017 - Queen Anne's 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0547*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0499*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:0-500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0761*1.45/0.683,NA),size = x) +#Walsh et al 2017 - Somerset 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  -0.0839*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0476*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , -0.0665*1.45/0.833,NA),size = x) +#Walsh et al 2017 - St Mary's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x <= 300,  0.0473*1.45/1.02, NA),size = x)+ #Walsh et al 2017 - St Talbot 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0149*1.45/1.02, NA),size = x)+ #Walsh et al 2017 - St Talbot 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0226*1.45/1.02,NA),size = x) +#Walsh et al 2017 - St Talbot 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0053*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0187*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0190*1.45/0.399,NA),size = x)+#Walsh et al 2017 - Wicomico 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 500, 2704.7*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 1750.7*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x <= 500, 782.4*0.00190,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 3196.1*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x <= 500, 2479.9*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x <= 500, 1301.7*0.00280,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x <= 500, 1424.9*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x <= 500, 410.4*-0.00500,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x <= 500, 312.7*0.00240,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x <= 500, 1071.3*0.00160,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x <= 500, 2006.9*0.00020,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x <= 500, 4367.5*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x <= 500, 1220.4*0.00370,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x <= 500, 282.8*0.00440,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x <= 500, 2357.8*0.00020,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x <= 500, 1115.7*0.00100,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x <= 500, 3038.6*0.00020,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x <= 500, 749.6*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x <= 500, 976.9*-0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x <= 500, 1376.1*0.00100,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x <= 500, 259.8*0.00820,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x <= 500, 38.8*0.09360,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x <= 500, 90.4*0.02720,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x <= 500, 1277.8*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x <= 500, 1327.7*-0.00020,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00480,NA),size = x)+ # Wolf et al 2022 - Model 27
  geom_function(fun =~ if_else(.x <= 500, 2704.7*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 1750.7*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x <= 500, 782.4*0.00180,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 3196.1*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x <= 500, 2479.9*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x <= 500, 1301.7*0.00290,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x <= 500, 1424.9*-0.00020,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x <= 500, 410.4*-0.00570,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x <= 500, 312.7*0.00200,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x <= 500, 1071.3*0.00100,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x <= 500, 2006.9*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x <= 500, 4367.5*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x <= 500, 1220.4*0.00370,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x <= 500, 282.8*0.00520,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x <= 500, 2357.8*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x <= 500, 1115.7*0.00140,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x <= 500, 3038.6*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x <= 500, 749.6*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x <= 500, 976.9*0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x <= 500, 1376.1*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x <= 500, 259.8*0.00940,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x <= 500, 38.8*0.09800,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x <= 500, 90.4*0.02600,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x <= 500, 1277.8*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x <= 500, 1327.7*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00350,NA),size = x)+ # Wolf et al 2022 - Model 27
  geom_function(fun =~ if_else(.x <= 500, 2704.7*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 1750.7*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x <= 500, 782.4*0.00120,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 3196.1*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x <= 500, 2479.9*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x <= 500, 1301.7*0.00540,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x <= 500, 1424.9*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x <= 500, 410.4*-0.01180,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x <= 500, 312.7*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x <= 500, 1071.3*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x <= 500, 2006.9*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x <= 500, 4367.5*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x <= 500, 1220.4*0.00510,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x <= 500, 282.8*0.00340,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x <= 500, 2357.8*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x <= 500, 1115.7*0.00180,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x <= 500, 3038.6*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x <= 500, 749.6*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x <= 500, 976.9*0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x <= 500, 1376.1*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x <= 500, 259.8*0.00850,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x <= 500, 38.8*0.08330,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x <= 500, 90.4*0.03240,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x <= 500, 1277.8*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x <= 500, 1327.7*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00150,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00590,NA),size = x)+ # Wolf et al 2022 - Model 27
  geom_function(fun =~ if_else(.x <= 500, 2704.7*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 1750.7*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x <= 500, 782.4*0.00190,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 3196.1*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x <= 500, 2479.9*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x <= 500, 1301.7*0.00280,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x <= 500, 1424.9*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x <= 500, 410.4*-0.00630,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x <= 500, 312.7*0.00180,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x <= 500, 1071.3*0.00160,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x <= 500, 2006.9*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x <= 500, 4367.5*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x <= 500, 1220.4*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x <= 500, 282.8*0.00470,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x <= 500, 2357.8*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x <= 500, 1115.7*0.00120,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x <= 500, 3038.6*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x <= 500, 749.6*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x <= 500, 976.9*0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x <= 500, 1376.1*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x <= 500, 259.8*0.00850,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x <= 500, 38.8*0.08330,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x <= 500, 90.4*0.03240,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x <= 500, 1277.8*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x <= 500, 1327.7*-0.00060,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00420,NA),size = x,aes(col = "Primary Study Estimations"))+ # Wolf et al 2022 - Model 27
  #geom_point(aes(color = study_name), size=0.5)+
  geom_smooth(method = "gam", aes(color = "Smoothed line (GAM)"))+
  scale_colour_manual(values = c("Smoothed line (GAM)"="red",
                                 "Primary Study Estimations"="black"))+
  theme_bw()+
  labs(x = "Distance to waterbody (meters)", y = "Elasticity")+
  scale_x_continuous(breaks=seq(0, 2050, 250), expand=c(0, 0), limits=c(0, 2050))+
  scale_y_continuous(expand=c(0, 0), limits=c(0, 0.6))+
  labs(color='')+
  theme(legend.position = "bottom")+
  theme(axis.line = element_line(color='black'),
      plot.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank())+
  theme(legend.text=element_text(size=11))
  
  
library(ggpubr)

plotlist<- list(p_no_dist,p_linear,p_distdum,p_lindist_dumdist)

p <- ggarrange(plotlist = plotlist, ncol = 2, nrow = 2, common.legend = TRUE, legend="bottom",labels = c("A","B","C","D"))

# Save the plot to a file 
ggsave("./results/Figures/Figure2.png", plot = p, width = 9, height = 6, dpi = 300, bg = "white")


##################################################################

p_all <- ggplot(df1, aes(dist, elast)) +
  geom_function(fun =~ if_else(.x <= 500, 0.009,NA), size = x) + # Ara 2007-IH Cluster 5 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.172,NA), size = x) + # Ara 2007 - IH Cluster 6 - OLS
  geom_function(fun =~ if_else(.x <= 500, -0.033,NA), size = x) + # Ara 2007 - IH Cluster 7 - OLS
  geom_function(fun =~ if_else(.x <= 500, -0.033,NA), size = x) + # Ara 2007 - IH Cluster 11 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.012,NA), size = x) + # Ara 2007 - IH Cluster 5 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.163,NA), size = x) + # Ara 2007 - IH Cluster 6 - GMM
  geom_function(fun =~ if_else(.x <= 500, -0.007,NA), size = x) + # Ara 2007 - IH Cluster 7 - GMM
  geom_function(fun =~ if_else(.x <= 500, -0.029,NA), size = x) + # Ara 2007 - IH Cluster 11 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.204,NA), size = x) + # Ara 2007 - CBG Cluster 2 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.029,NA), size = x) + # Ara 2007 - CBG Cluster 3 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.029,NA), size = x) + # Ara 2007 - CBG Cluster 3 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.12,NA), size = x) + # Ara 2007 - CBG Cluster 4 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.037,NA), size = x) + # Ara 2007 - CBG Cluster 7 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.012,NA), size = x) + # Ara 2007 - CBG Cluster 9 - OLS
  geom_function(fun =~ if_else(.x <= 500, 0.203,NA), size = x) + # Ara 2007 - CBG Cluster 2 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.026,NA), size = x) + # Ara 2007 - CBG Cluster 3 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.056,NA), size = x) + # Ara 2007 - CBG Cluster 4 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.052,NA), size = x) + # Ara 2007 - CBG Cluster 7 - GMM
  geom_function(fun =~ if_else(.x <= 500, 0.016,NA), size = x) + # Ara 2007 - CBG Cluster 9 - GMM
  geom_function(fun =~ if_else(.x <= 500, 4.369999886*3515/102746,NA), size = x)+ # Boyle and Taylor 2001 - G1-Town Data
  geom_function(fun =~ if_else(.x <= 500, 3.920000076*3515/102746,NA), size = x)+ # Boyle and Taylor 2001 - G1-Survey Data
  geom_function(fun =~ if_else(.x <= 500, 2.099999905*3515/85197,NA), size = x)+ # Boyle and Taylor 2001 - G2-Town Data
  geom_function(fun =~ if_else(.x <= 500, 1.389999986*3515/85197,NA), size = x)+ # Boyle and Taylor 2001 - G2-Survey Data
  geom_function(fun =~ if_else(.x <= 300, 4.730000019*3515/32779,NA), size = x)+ # Boyle and Taylor 2001 - G3-Town Data
  geom_function(fun =~ if_else(.x <= 500, 6.130000114*3515/32779,NA), size = x)+ # Boyle and Taylor 2001 - G3-Survey Data
  geom_function(fun =~ if_else(.x <= 500, 40.02999878*3515/97482,NA), size = x)+ # Boyle and Taylor 2001 - G4-Town Data
  geom_function(fun =~ if_else(.x <= 500, 36.13999939*3515/97482,NA), size = x)+ # Boyle and Taylor 2001 - G4-Survey Data
  geom_function(fun =~ if_else(.x <= 500, 3514/104069*7.375697136,NA), size = x) + # Boyle et al 1999 -model 1
  geom_function(fun =~ if_else(.x <= 500, 3514/85880*3.167430162,NA), size = x) + # Boyle et al 1999 - model 2
  geom_function(fun =~ if_else(.x <= 500, 3514/73938*3.584641457,NA), size = x) + # Boyle et al 1999 - model 3
  geom_function(fun =~ if_else(.x <= 500, 3514/100350*13.07054615,NA), size = x)+  # Boyle et al 1999 - model 4
  geom_function(fun =~ if_else(.x <= 500, 178.3*3.83/2239.2,NA), size = x)+  # Calder¢n-Arrieta 2019 - model 1
  geom_function(fun =~ if_else(.x <= 500, 185.9*3.83/2239.2,NA), size = x)+  # Calder¢n-Arrieta 2019 - model 2
  geom_function(fun =~ if_else(.x <= 500, 43520.43*3.851/648415.8,NA), size = x)+  # Clapper & Caudill 2014 - liner - sale price
  geom_function(fun =~ if_else(.x <= 500, 0.065*3.851,NA), size = x)+  # Clapper & Caudill 2014 - log-liner - sale price
  geom_function(fun =~ if_else(.x <= 500, 0.269,NA), size = x)+  # Clapper & Caudill 2014 - log-log - sale price
  geom_function(fun =~ if_else(.x <= 500, 30.988*3.851/443.566,NA), size = x)+  # Clapper & Caudill 2014 - liner - sale price per square foot
  geom_function(fun =~ if_else(.x <= 500, 0.064*3.851,NA), size = x)+  # Clapper & Caudill 2014 - log-liner - sale price per square foot
  geom_function(fun =~ if_else(.x <= 500, 0.268,NA), size = x)+  # Clapper & Caudill 2014 - log-log - sale price per square foot
  geom_function(fun =~ if_else(.x <= 500, 4.48059988*1235.77002/138763.05,NA), size = x)+  # Gibbs et al 2002 - Conway/Milton
  geom_function(fun =~ if_else(.x <= 500, 17.34000015*1879.099976/175157.73,NA), size = x)+  # Gibbs et al 2002 - Winnipesaukee
  geom_function(fun =~ if_else(.x <= 500, 76.76999664*213.5800018/132162.84,NA), size = x)+  # Gibbs et al 2002 - Derry/Amherst
  geom_function(fun =~ if_else(.x <= 500, 149.6000061*283.6900024/167104.7,NA), size = x)+  # Gibbs et al 2002 - Spofford/Greenfield
  geom_function(fun =~ if_else(.x <= 500, 13893.84961*3.28*0.926591992/268034.57,NA), size = x)+  # Horsch & Lewis 2009 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 14355.2998*3.28*0.926591992/268034.57,NA), size = x)+  # Horsch & Lewis 2009 - Model 2
  geom_function(fun =~ if_else(.x <= 500, 13367.92969*3.28*0.926591992/268034.57,NA), size = x)+  # Horsch & Lewis 2009 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 7072.709961*3.28*0.926591992/268034.57,NA), size = x)+  # Horsch & Lewis 2009 - Random Effects with Year Dummies
  geom_function(fun =~ if_else(.x <= 500, 6443.779785*3.28*0.926591992/268034.57,NA), size = x)+  # Horsch & Lewis 2009 - Random Effects with Year Trend Variable
  geom_function(fun =~ if_else(.x <= 500, 8.75*866/65363.49,NA), size = x)+  # Hsu 2000 - Northeast Kingdom
  geom_function(fun =~ if_else(.x <= 500, 12.84000015*2021/120063.58,NA), size = x)+  # Hsu 2000 - Lake Champlain
  geom_function(fun =~ if_else(.x <= 500, 10.48999977*1552/104471.03,NA), size = x)+  # Hsu 2000 - Rutland County
  geom_function(fun =~ if_else(.x <= 500, 10.60999966*1581/105410.9,NA), size = x)+  # Hsu 2000 - Milfoil
  geom_function(fun =~ if_else(.x <= 500, 13.32999992*1581/105410.9,NA), size = x)+  # Hsu 2000 - exp(milfoil)
  geom_function(fun =~ if_else(.x <= 500, 44.31000137*1581/105410.9,NA), size = x)+  # Hsu 2000 - Total Weed
  geom_function(fun =~ if_else(.x <= 500, 22.92000008*1581/105410.9,NA), size = x)+  # Hsu 2000 - exp(Total Weed)
  geom_function(fun =~ if_else(.x <= 2000, 0.059,NA), size = x)+  # Irwin & Wolf 2022 - Model 1
  geom_function(fun =~ if_else(.x <= 2000, 0.093,NA), size = x)+  # Irwin & Wolf 2022 - Model 2
  geom_function(fun =~ if_else(.x <= 2000, 0.103,NA), size = x)+  # Irwin & Wolf 2022 - Model 3
  geom_function(fun =~ if_else(.x <= 2000, 0.086,NA), size = x)+  # Irwin & Wolf 2022 - Model 4
  geom_function(fun =~ if_else(.x <= 2000, 0.079,NA), size = x)+  # Irwin & Wolf 2022 - Model 6
  geom_function(fun =~ if_else(.x <= 500, 5207*3.28*3.627120018/184892,NA), size = x)+  # Kashian et al 2006 - Hedonic Equation (3)
  geom_function(fun =~ if_else(.x <= 500, 46459.33/(218562*3.28),NA), size = x)+  # Kemp et al 2017 - Model 1 (SD)
  geom_function(fun =~ if_else(.x <= 500, 45292.89/(238142*3.28),NA), size = x)+  # Kemp et al 2017 - Model 2 (SD)
  geom_function(fun =~ if_else(.x <= 500, 66046.22/(175331*3.28),NA), size = x)+  # Kemp et al 2017 - Model 3 (SD)
  geom_function(fun =~ if_else(.x <= 500, 13.22999954*702.6099854/100313,NA), size = x)+  # Krysel et al 2003  - Aitkin Lake Group
  geom_function(fun =~ if_else(.x <= 500, 4.71999979*985.7600098/176461,NA), size = x)+  # Krysel et al 2003 - Brainerd Lake Group
  geom_function(fun =~ if_else(.x <= 500, 1.100000024*852.2600098/135905,NA), size = x)+  # Krysel et al 2003 - Grand Rapids Lake Group
  geom_function(fun =~ if_else(.x <= 500, 2.150000095*1031.869995/179621,NA), size = x)+  # Krysel et al 2003 - Walker Lake Group
  geom_function(fun =~ if_else(.x <= 500, 21.75*882.9099731/124390,NA), size = x)+  # Krysel et al 2003 - Park Rapids Lake Group
  geom_function(fun =~ if_else(.x <= 500, 7.309999943*702.6099854/100313,NA), size = x)+  # Krysel et al 2003 - MN - Aitkin Lake Group
  geom_function(fun =~ if_else(.x <= 500, 1.940000057*985.7600098/176461,NA), size = x)+  # Krysel et al 2003 - MN - Brainerd Lake Group
  geom_function(fun =~ if_else(.x <= 500, 1.730000019*852.2600098/135905,NA), size = x)+  # Krysel et al 2003 - MN - Grand Rapids Lake Group
  geom_function(fun =~ if_else(.x <= 500, 1.909999967*1031.869995/179621,NA), size = x)+  # Krysel et al 2003 - MN - Walker Lake Group
  geom_function(fun =~ if_else(.x <= 500, 19.95000076*882.9099731/124390,NA), size = x)+  # Krysel et al 2003 - MN - Park Rapids Lake Group
  geom_function(fun =~ if_else(.x <= 500, 9.720000267*1101.5/142829,NA), size = x)+  # Krysel et al 2003 - MN - Bemidji Lake Group
  geom_function(fun =~ if_else(.x <= 500, 0.275000006,NA), size = x)+  # Liao et al 2016 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 0.231999993,NA), size = x)+  # Liao et al 2016 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 0.219999999,NA), size = x)+  # Liao et al 2016 - Spatial Regime Model 1
  geom_function(fun =~ if_else(.x <= 500, 0.165000007,NA), size = x)+  # Liao et al 2016 - Spatial Regime Model 1
  geom_function(fun =~ if_else(.x <= 500, 0.223000005,NA), size = x)+  # Liao et al 2016 - Spatial Regime Model 3
  geom_function(fun =~ if_else(.x <= 500, 0.165999994,NA), size = x)+  # Liao et al 2016 - Spatial Regime Model 3
  geom_function(fun =~ if_else(.x <= 2000, 0.30340001*0.709999979,NA), size = x)+  # Liu et al 2014 - Sale Price - semi-log
  geom_function(fun =~ if_else(.x <= 2000, -0.0145, NA), size = x) +  # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:1
  geom_function(fun =~ if_else(.x <= 2000, 0.024, NA), size = x) + # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:2
  geom_function(fun =~ if_else(.x <= 2000, -0.0094, NA), size = x) + # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:3
  geom_function(fun =~ if_else(.x <= 2000, 0.028, NA), size = x) + # Mamun et al 2023 - Model 1 - discrete distance bins/Table S6:4
  geom_function(fun =~ if_else(.x <= 500, 0.413004667, NA), size = x) + # Michael et al 2000 - Model 1 - Group 1 - CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.093727589, NA), size = x) + # Michael et al 2000 - Model 1 - Group 2 -  CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.491040945, NA), size = x) + # Michael et al 2000 - Model 1 - Group 3 -  CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.285336018, NA), size = x) + # Michael et al 2000 - Model 2 - Group 1 -  PMIN 
  geom_function(fun =~ if_else(.x <= 500, 0.10351032, NA), size = x) + # Michael et al 2000 - Model 2 - Group 2 -  PMIN  
  geom_function(fun =~ if_else(.x <= 500, 0.664476693, NA), size = x) + # Michael et al 2000 - Model 2 - Group 3 - PMIN 
  geom_function(fun =~ if_else(.x <= 500, 0.376048774, NA), size = x) + # Michael et al 2000 - Model 3 - Group 1 - HMIN   
  geom_function(fun =~ if_else(.x <= 500, 0.138936102, NA), size = x) + # Michael et al 2000 - Model 3 - Group 2 - HMIN 
  geom_function(fun =~ if_else(.x <= 500, 0.916609764, NA), size = x) + # Michael et al 2000 -Model 3 - Group 3 - HMIN  
  geom_function(fun =~ if_else(.x <= 500, 0.385807753, NA), size = x) + # Michael et al 2000 -Model 4 - Group 1 - CMIN * HMIN 
  geom_function(fun =~ if_else(.x <= 500, 0.079734102, NA), size = x) + # Michael et al 2000 -Model 4 - Group 2 - CMIN * HMIN  
  geom_function(fun =~ if_else(.x <= 500, 0.431535065, NA), size = x) + # Michael et al 2000 - Model 4 - Group 3 - CMIN * HMIN 
  geom_function(fun =~ if_else(.x <= 500, -0.062146027, NA), size = x) + # Michael et al 2000 - Model 5 - Group 1 - CMIN * HMIN +/-  
  geom_function(fun =~ if_else(.x <= 500, 0.015297231, NA), size = x) + # Michael et al 2000 - Model 5 - Group 2 - CMIN * HMIN +/-  
  geom_function(fun =~ if_else(.x <= 500, -0.054691911, NA), size = x) + # Michael et al 2000 - Model 5 - Group 3 - CMIN * HMIN +/-  
  geom_function(fun =~ if_else(.x <= 500, 0.040301826, NA), size = x) + # Michael et al 2000 - Model 6 - Group 1 - CMAX/CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.004697338, NA), size = x) + # Michael et al 2000 - Model 6 - Group 2 - CMAX/CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.193003386, NA), size = x) + # Michael et al 2000 - Model 6 - Group 3 - CMAX/CMIN
  geom_function(fun =~ if_else(.x <= 500, 0.432574719, NA), size = x) + # Michael et al 2000 - Model 7 - Group 1 - CMAX/CMIN%
  geom_function(fun =~ if_else(.x <= 500, 0.104469016, NA), size = x) + # Michael et al 2000 - Model 7 - Group 2 - CMAX/CMIN%
  geom_function(fun =~ if_else(.x <= 500, 0.542586148, NA), size = x) + # Michael et al 2000 - Model 7 - Group 3 - CMAX/CMIN%
  geom_function(fun =~ if_else(.x <= 500, 0.452805698, NA), size = x) + # Michael et al 2000 - Model 8 - Group 1 - CMIN-HMIN
  geom_function(fun =~ if_else(.x <= 500, 0.114391185, NA), size = x) + # Michael et al 2000 - Model 8 - Group 2 - CMIN-HMIN
  geom_function(fun =~ if_else(.x <= 500, 0.589391351, NA), size = x) + # Michael et al 2000 - Model 8 - Group 3 - CMIN-HMIN
  geom_function(fun =~ if_else(.x <= 500, 0.325085133, NA), size = x) + # Michael et al 2000 - Model 9 - Group 1 - HMIN+ and HMIN -
  geom_function(fun =~ if_else(.x <= 500, 0.098492384, NA), size = x) + # Michael et al 2000 - Model 9 - Group 2 - HMIN+ and HMIN -
  geom_function(fun =~ if_else(.x <= 500, 0.401052326, NA), size = x) + # Michael et al 2000 - Model 9 - Group 3 - HMIN+ and HMIN -
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.151, NA), size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.154, NA), size = x) + # Moore et al 2020 - IV-GMM - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.114, NA), size = x) + # Moore et al 2020 - OLS - 2a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.117, NA), size = x) + # Moore et al 2020 - IV-GMM - 2b
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.121, NA), size = x) + # Moore et al 2020 - OLS - 3a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.126, NA), size = x) + # Moore et al 2020 - IV-GMM - 3b
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.099, NA), size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.101, NA), size = x) + # Moore et al 2020 - IV-GMM - 4b
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.151, NA),size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.151, NA),size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.151, NA),size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.096, NA),size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.094, NA),size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.098, NA),size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.153 + -0.118*1 + -0.068*0 + -0.017*0), NA),size = x) + # Moore et al 2020 - OLS - 1a - Florida
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.153 + -0.118*0 + -0.068*1 + -0.017*0), NA),size = x) + # Moore et al 2020 - OLS - 1a - Indiana
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.153 + -0.118*0 + -0.068*0 + -0.017*1), NA),size = x) + # Moore et al 2020 - OLS - 1a - Washington
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.128 + 0.407*1 + -0.029*0 + -0.02*0), NA),size = x) + # Moore et al 2020 - OLS - 2a - Florida
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.128 + 0.407*0 + -0.029*1 + -0.02*0), NA),size = x) + # Moore et al 2020 - OLS - 2a - Indiana
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.128 + 0.407*0 + -0.029*0 + -0.02*1), NA),size = x) + # Moore et al 2020 - OLS - 2a - Washington
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.138 + 0.358*1 + -0.041*0 + -0.03*0), NA),size = x) + # Moore et al 2020 - OLS - 3a - Florida
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.138 + 0.358*0 + -0.041*1 + -0.03*0), NA),size = x) + # Moore et al 2020 - OLS - 3a - Indiana
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.138 + 0.358*0 + -0.041*0 + -0.03*1), NA),size = x) + # Moore et al 2020 - OLS - 3a - Washington
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.111 + 0.251*1 + -0.072*0 + -0.007*0), NA),size = x) + # Moore et al 2020 - OLS - 3a - Florida
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.111 + 0.251*0 + -0.072*1 + -0.007*0), NA),size = x) + # Moore et al 2020 - OLS - 3a - Indiana
  geom_function(fun =~ if_else(.x <= 160, 2.1*(0.111 + 0.251*0 + -0.072*0 + -0.007*1), NA),size = x) + # Moore et al 2020 - OLS - 3a - Washington
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.21, NA),size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.09, NA),size = x) + # Moore et al 2020 - OLS - 2a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.103, NA),size = x) + # Moore et al 2020 - OLS - 3a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.085, NA),size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.186, NA),size = x) + # Moore et al 2020 - OLS - 1a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.07, NA),size = x) + # Moore et al 2020 - OLS - 2a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.084, NA),size = x) + # Moore et al 2020 - OLS - 3a
  geom_function(fun =~ if_else(.x <= 160, 2.1*0.089, NA),size = x) + # Moore et al 2020 - OLS - 4a
  geom_function(fun =~ if_else(.x <= 500, -7430.29*3.4/502312.8, NA),size = x) + # Olden & Tamayo 2014 - Model 1
  geom_function(fun =~ if_else(.x <= 500, -6962.7*3.4/502312.8, NA),size = x) + # Olden & Tamayo 2014 - Model 2
  geom_function(fun =~ if_else(.x <= 500, -6954.8*3.4/502312.8, NA),size = x) + # Olden & Tamayo 2014 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 10.36999989*3209/103853, NA),size = x) + # Poor et al 2001 - Lewiston
  geom_function(fun =~ if_else(.x <= 500, 2.25*4391/86880, NA),size = x) + # Poor et al 2001 - Augusta
  geom_function(fun =~ if_else(.x <= 500, 1.46*2517/67881, NA),size = x) + # Poor et al 2001 - Bangor
  geom_function(fun =~ if_else(.x <= 500, -0.88*5161/31287, NA),size = x) + # Poor et al 2001 - Northern Maine
  geom_function(fun =~ if_else(.x <= 500, 0.17+6.80, NA),size = x) + # Swedberg el al 2020 - Multi-state model- ME ###Outlier
  geom_function(fun =~ if_else(.x <= 500, 0.17+ -1.04, NA),size = x) + # Swedberg el al 2020 - Multi-state model- MI
  geom_function(fun =~ if_else(.x <= 500, 0.17+ 0.08, NA),size = x) + # Swedberg el al 2020 - Multi-state model-  NY
  geom_function(fun =~ if_else(.x <= 500, 0.17+ 0.14, NA),size = x) + # Swedberg el al 2020 - Multi-state model-  VT
  geom_function(fun =~ if_else(.x <= 500, 0.17+ -0.11, NA),size = x) + # Swedberg el al 2020 - Multi-state model-   WI
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - Multi-state model
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop ME
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop MI
  geom_function(fun =~ if_else(.x <= 500, 0.13, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop MN
  geom_function(fun =~ if_else(.x <= 500, 0.16, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop NY
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop VT
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - Multi-state model - drop WI
  geom_function(fun =~ if_else(.x <= 500, -2.71, NA),size = x) + # Swedberg el al 2020 - State level model - Maine
  geom_function(fun =~ if_else(.x <= 500, 9.20, NA),size = x) + # Swedberg el al 2020 - State level model - Michigan
  geom_function(fun =~ if_else(.x <= 500, 0.17, NA),size = x) + # Swedberg el al 2020 - State level model - Minnesota
  geom_function(fun =~ if_else(.x <= 500, 0.13, NA),size = x) + # Swedberg el al 2020 - State level model - New York
  geom_function(fun =~ if_else(.x <= 500, -0.32, NA),size = x) + # Swedberg el al 2020 - State level model - Vermont
  geom_function(fun =~ if_else(.x <= 500, 0.1, NA),size = x) + # Swedberg el al 2020 - State level model - Wisconsin
  geom_function(fun =~ if_else(.x <= 500, 0.5, NA),size = x) + # Swedberg el al 2020 - Regional model - Otter Tail
  geom_function(fun =~ if_else(.x <= 500, 0.1, NA),size = x) + # Swedberg el al 2020 - Regional model - Twin Cities
  geom_function(fun =~ if_else(.x <= 500, -0.21, NA),size = x) + # Swedberg el al 2020 - Regional model - Adirondacks
  geom_function(fun =~ if_else(.x <= 500, 0.41, NA),size = x) + # Swedberg el al 2020 - Regional model - Regional model - Finger Lakes
  geom_function(fun =~ if_else(.x <= 500, 0.129, NA),size = x) + # Weng et al 2020 - Secci depth
  geom_function(fun =~ if_else(.x <= 139, 0.0352 + 0.0755 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 1
  geom_function(fun =~ if_else(.x <= 139, 0.0136 + 0.0543 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 1
  geom_function(fun =~ if_else(.x <= 139, 0.0548 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 2
  geom_function(fun =~ if_else(.x <= 139, 0.0291 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 2
  geom_function(fun =~ if_else(.x <= 139, 0.0092 + 0.0498 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 3
  geom_function(fun =~ if_else(.x <= 139, -0.0205 + 0.0444 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 3
  geom_function(fun =~ if_else(.x <= 139, -0.0147 + 0.0447 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 4
  geom_function(fun =~ if_else(.x <= 139, -0.0242 + 0.0421 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 4
  geom_function(fun =~ if_else(.x <= 139, -0.0086 + 0.0432 * log(2.39), NA),size = x) + # Wolf & Kemp 2021 - Model 5
  geom_function(fun =~ if_else(.x <= 139, 0.022 + 0.0407 * log(2.39), NA),size = x)+ # Wolf & Kemp 2021 - Model 5
  geom_function(fun =~ if_else(.x <= 500, -0.647772014, NA),size = x) + # Zhang & Boyle 2010 - Milfoil - Quadratic
  geom_function(fun =~ if_else(.x <= 500, -0.647772014, NA),size = x) + # Zhang & Boyle 2010 - Milfoil - Exponential
  geom_function(fun =~ if_else(.x <= 500, -0.323886007, NA),size = x) + # Zhang & Boyle 2010 - Total macrophytes - Quadratic
  geom_function(fun =~ if_else(.x <= 500, -0.323886007, NA),size = x) + # Zhang & Boyle 2010 - Total macrophytes - Exponential
  geom_function(fun =~ if_else(.x <= 500, -0.161943004, NA),size = x) + # Zhang & Boyle 2010 - Best Model - Quadratic
  geom_function(fun =~ if_else(.x <= 500, -0.323886007, NA),size = x) + # Zhang & Boyle 2010 - Best Model - Exponential
  geom_function(fun =~ if_else(.x <= 500, 0.18130137, NA),size = x) + # Zhang et al 2015 - VT1
  geom_function(fun =~ if_else(.x <= 500, 0.168578461, NA),size = x) + # Zhang et al 2015 - VT2
  geom_function(fun =~ if_else(.x <= 500, 0.031807259, NA),size = x) + # Zhang et al 2015 - VT3
  geom_function(fun =~ if_else(.x <= 500, 0.368520737, NA),size = x) + # Zhang et al 2015 - ME1
  geom_function(fun =~ if_else(.x <= 500, -0.044222489, NA),size = x) + # Zhang et al 2015 - ME2
  geom_function(fun =~ if_else(.x <= 500, 0.078617759, NA),size = x) + # Zhang et al 2015 - ME3
  geom_function(fun =~ if_else(.x <= 500, 0.560151517, NA),size = x) + # Zhang et al 2015 - ME4
  geom_function(fun =~ if_else(.x <= 500, -0.00491361, NA),size = x) + # Zhang et al 2015 - ME5
  geom_function(fun =~ if_else(.x <= 500, -0.01965444, NA),size = x) + # Zhang et al 2015 - ME6
  geom_function(fun =~ if_else(.x <= 500, 1.719763517, NA),size = x) + # Zhang et al 2015 - ME7
  geom_function(fun =~ if_else(.x <= 500, 0.035835754, NA),size = x) + # Zhang et al 2015 - NH1
  geom_function(fun =~ if_else(.x <= 500, 0.04596325, NA),size = x) + # Zhang et al 2015 - NH2
  geom_function(fun =~ if_else(.x <= 500, 1.505880713, NA),size = x) + # Zhang et al 2015 - NH3
  geom_function(fun =~ if_else(.x <= 500, 0.971460581, NA),size = x) + # Zhang et al 2015 - NH4
  geom_function(fun =~ if_else(.x <= 500, 0.185411081, NA),size = x, aes(col = "Primary Study Estimations")) + # Zhang et al 2015 - NH5
  #linear
  geom_function(fun =~ if_else(.x <= 2000, 0.248 + -0.076*(log(.x/100)),NA),size = x) + # # Irwin & Wolf 2022 - Model 5 (2km)
  geom_function(fun =~ if_else(.x <= 2000, (0.0964 + -0.0305*(.x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - without distance threshhold interaction( sample within 5 miles)
  geom_function(fun =~ if_else(.x <= 2000, (0.2096 + -0.0426*(.x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - property fixed effect model - 2 or more sale
  geom_function(fun =~ if_else(.x <= 2000, (0.2638 + -0.0682*(.x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - property fixed effect model - 3 or more sale
  geom_function(fun =~ if_else(.x <= 2000, (0.4503 + -0.1117*(.x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - property fixed effect model - 4 or more sale
  geom_function(fun =~ if_else(.x <= 2000, (0.0841 + -0.025*(.x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - property fixed effect model - 4 or more sale
  geom_function(fun =~ if_else(.x <= 2000, (6966.6554 + -469.5857*(.x/1609.34))*(0.542544*3.28/217951.2),NA),size = x,aes(col = "Primary Study Estimations")) +
  #dist dum
  geom_function(fun =~ if_else(.x <= 200, -0.099399999*-1,NA),size = x)+ # Guignet et al 2017 - 2.C:0_200m
  geom_function(fun =~ if_else(.x >= 200 & .x <= 500, -0.0058*-1,NA),size = x)+ # Guignet et al 2017 - 2.C:200_500m
  geom_function(fun =~ if_else(.x <= 100,0.0076 + 0.1673, NA),size = x)+# Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0199 + 0.1778, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County*year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0199 + 0.0435, NA),size = x)+ # Mamun et al 2023:Model 1 - discrete distance bins/FE:County*year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0199,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County*year:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0383 + 0.1758, NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/FE:State*year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0383 + 0.0477, NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/FE:State*year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0383,NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/FE:State*year:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0105 + 0.1674, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract+year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0105 + 0.0437, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract+year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0105,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract+year:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0141 + 0.1804, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County+year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0141 + 0.0475, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County+year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0141,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County+year:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0343 + 0.1797, NA),size = x)+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:State+year)
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0343 + 0.0514, NA),size = x)+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:State+year)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0343,NA),size = x)+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:State+year)
  geom_function(fun =~ if_else(.x <= 100, 0.0287 + 0.1581, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Block*year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0287 + 0.0388, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Block*year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0287,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Block*year:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0108 + 0.1568, NA),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/FE:Block+year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0108 + 0.0413, NA),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/FE:Block+year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0108,NA),size = x)+  # Mamun et al 2023- Model 1:discrete distance bins/FE:Block+year:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0051 + 0.2315, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0051 + 0.0726, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0051,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:year:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0123 + 0.1678, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:tract:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0123 + 0.0439, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:tract:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0123,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:tract:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0085 + 0.1573, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:2:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0085 + 0.0414, NA),size = x)+  # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:2:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0085,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:2:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0072 + 0.1532, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:3:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0072 + 0.0401, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:3:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0072,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:3:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0136 + 0.1586, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:5:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0136 + 0.0455, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:5:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0136,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:5:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0101 + 0.1666, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:7:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0101 + 0.055, NA),size = x)+  # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:7:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0101,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:7:300_2000
  geom_function(fun =~ if_else(.x <= 100, -0.0018 + 0.1377, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:10:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , -0.0018 + 0.0497, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:10:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , -0.0018,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:10:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0108 + 0.1654, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:10:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0108 + 0.0417, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:10:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0108,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:10:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0225 + 0.1582, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:30:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0225 + 0.0472, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:30:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0225,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:30:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0231 + 0.129, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:100:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0231 + 0.0461, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:100:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0231,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:300:2000
  geom_function(fun =~ if_else(.x <= 100, 0.0104 + 0.1461, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S6:5:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0104 + 0.0364, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S6:5:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0104,NA),size = x)+  # Mamun et al 2023 - Model 1:discrete distance bins/Table S6:5:300_2000
  geom_function(fun =~ if_else(.x <= 300, 0.0123 + 0.1563, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:1:waterfront we define 300m(1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0123,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:1:waterfront we define 300m(0)
  geom_function(fun =~ if_else(.x <= 50, 0.0424 + 0.0953, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:2:watefront 50m(1)
  geom_function(fun =~ if_else(.x >= 50 & .x <= 2000 , 0.0424,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:2:watefront 50m(0)
  geom_function(fun =~ if_else(.x <= 100, 0.0157 + 0.151, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:3):watefront 100m(1)
  geom_function(fun =~ if_else(.x >= 100 & .x <= 2000 , 0.0157,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:3):watefront 100m(0)
  geom_function(fun =~ if_else(.x <= 200, 0.0076 + 0.1296, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:4:watefront 200m(1)
  geom_function(fun =~ if_else(.x >= 200 & .x <= 2000 , 0.0076,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:4:watefront 200m(0)
  geom_function(fun =~ if_else(.x <= 50, 0.022 + 0.1439, NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/Table S7:5:0_50
  geom_function(fun =~ if_else(.x >= 50 & .x <= 200 , 0.022 + 0.1134,NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/Table S7:5:50_200
  geom_function(fun =~ if_else(.x >= 200 & .x <= 2000 , 0.022, NA),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/Table S7:5:200_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0104 + 0.1636, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:6:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 200 , 0.0104 + 0.0507,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:6:100_200
  geom_function(fun =~ if_else(.x >= 200 & .x <= 2000 , 0.0104, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:6:200_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:7:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:7:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:7:300_2000
  geom_function(fun =~ if_else(.x <= 200, 0.0037 + 0.1359, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:8:0_200
  geom_function(fun =~ if_else(.x >= 200 & .x <= 500 , 0.0037 + 0.0132,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:8:200_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 2000 , 0.0037, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:8:500_2000
  geom_function(fun =~ if_else(.x <= 300, 0.0019 + 0.1089, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:9:0_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 1000 , 0.0019 + 0.0031,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:9:300_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 2000 , 0.0019, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:9:100_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0041 + 0.155, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:0:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0041 + 0.041,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:0:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0041, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:0:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0166 + 0.1579, NA),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/Table S8:1:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0166 + 0.0375,NA),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/Table S8:1:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0166, NA),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/Table S8:1:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0121 + 0.1645, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:3:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0121 + 0.0393,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:3:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0121, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:3:300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:5:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+  # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:5:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:5:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0072 + 0.1688, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:10:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0072 + 0.041,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:10:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0072, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:10:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0078 + 0.1728, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:anytime:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0078 + 0.0382,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:anytime:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0078, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:anytime:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S9:customer price index:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S9:customer price index:100-300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076, NA),size = x)+  # Mamun et al 2023 - Model 1:discrete distance bins/Table S9:customer price index:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0072 + 0.1532, NA),size = x)+ # Mamun et al 2023 - Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0072 + 0.0401,NA),size = x)+  # Mamun et al 2023 - Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0072, NA),size = x)+ # Mamun et al 2023 - Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0126 + 0.1148, NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:1:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0126 + 0.0184,NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:1:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0126, NA),size = x)+  # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:1:300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0173 + 0.1676, NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:2:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0173 + 0.0401,NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:2:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0401, NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:2:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:4:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:4:100-300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076, NA),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:4:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.007 + 0.0789, NA),size = x)+ # Mamun et al 2023 -Model 3:discrete distance bins/Table S11:ln(housing price)):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.007 + 0.0227,NA),size = x)+ # Mamun et al 2023 -Model 3:discrete distance bins/Table S11:ln(housing price)):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.007, NA),size = x)+ # Mamun et al 2023 -Model 3:discrete distance bins/Table S11:ln(housing price)):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.0469, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:CPL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204  + 0.0155,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:CPL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:CPL:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + 0.0824, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NAP:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + 0.0333,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NAP:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NAP:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.2392, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NPL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + -0.0445,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NPL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NPL:300_1000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + 0.1601, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SAP):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + 0.1002,NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SAP):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SAP):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.1651, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SPL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + -0.0836,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SPL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SPL:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.1553, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:TPL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + -0.0477,NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:TPL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:TPL:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + 0.0551, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:WMT:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + 0.0303,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:WMT:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:WMT:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.0765, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:XER:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0113 + 0.0204 + 0.0002,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:XER:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0113, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:XER:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1364, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0949,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AL:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0547, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AR:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.207,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AR:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AR:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.1572, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AZ:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0368,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AZ:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AZ:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1312, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CA):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0552,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CA):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CA):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.2389, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CO):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0367,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CO):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CO):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.1809, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CT):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.065,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CT):100-300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CT):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.5047, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:DE):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.2419,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:DE):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:DE):300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.1284, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:FL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0018,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:FL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:FL:300:2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.5163, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:GA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.2384,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:GA:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:GA:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0758, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.1,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IA:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IA:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.3359, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IL:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.2071,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IL:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IL:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0794, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IN:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.039,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IN:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IN:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 1.2532, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:KY:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0888,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:KY:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:KY:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0273, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:LA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0462,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:LA:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:LA:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0285, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.059,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MA:100-300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MA:300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.1581, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MD:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0346,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MD:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MD:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1299, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:ME:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.1871,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:ME:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:ME:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0913, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MI:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0146,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MI:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MI:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0672, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MO):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.2849,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MO):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MO):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.3935, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MS:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.6851,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MS:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MS:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.4651, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MT:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.3235,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MT:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MT:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.108, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NC):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.047,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NC):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NC):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0542, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NE:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.1447,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NE:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NE:300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0968, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NH:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0315,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NH:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NH:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.2329, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NJ:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0775,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NJ:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NJ:300-2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0915, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NV:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0197,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NV:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NV:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1677, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NY:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.074,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NY:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+  # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NY:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.3565, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OH:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.051,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OH:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OH:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.3811, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OK:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.3099,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OK:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OK:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0729, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OR:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0604,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OR:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OR:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0623, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:PA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0366,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:PA:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:PA:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.4243, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:RI:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0761,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:RI:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:RI:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.2388, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SC:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.1485,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SC:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SC:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0699, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SD:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0298,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SD:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SD:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1608, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TN:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.1244,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TN:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TN:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1807, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TX:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0074,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TX:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TX:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.9602, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:UT:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0612,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:UT:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:UT:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.3189, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VA):0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0059,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VA):100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VA):300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.0231, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VT:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0491,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VT:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VT:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0623, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WA:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0217,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WA:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WA:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0069, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WI:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.0208,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WI:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WI:300_2000
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0314, NA),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WV:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0134 + 0.0238 + -0.1154,NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WV:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0134, NA),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WV:300_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0190*1 + 0.0138*0 + 0.0083*0 + 0.0095*0 + -0.0003*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0190*0 + 0.0138*1 + 0.0083*0 + 0.0095*0 + -0.0003*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0190*0 + 0.0138*0 + 0.0083*1 + 0.0095*0 + -0.0003*0)*3.28*1.86,NA),size = x)+  # Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0190*0 + 0.0138*0 + 0.0083*0 + 0.0095*1 + -0.0003*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0190*0 + 0.0138*0 + 0.0083*0 + 0.0095*1 + -0.0003*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE:1500:2000
  geom_function(fun =~ if_else(.x <= 300,(0.0088 + 0.0108*1 + 0.0110*0 + 0.0077*0 + 0.0088*0 + 0.0040*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 6:discrete distance bins:Town FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0088 + 0.0108*0 + 0.0110*1 + 0.0077*0 + 0.0088*0 + 0.0040*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 6:discrete distance bins:Town FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*1 + 0.0088*0 + 0.0040*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 6:discrete distance bins:Town FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*0 + 0.0088*1 + 0.0040*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 6:discrete distance bins:Town FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*0 + 0.0088*0 + 0.0040*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 6:discrete distance bins:Town FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0051 + 0.0155*1 + 0.0139*0 + 0.0116*0 + 0.0132*0 + 0.0046*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 7:discrete distance bins:School FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0051 + 0.0155*0 + 0.0139*1 + 0.0116*0 + 0.0132*0 + 0.0046*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 7:discrete distance bins:School FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*1 + 0.0132*0 + 0.0046*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 7:discrete distance bins:School FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*0 + 0.0132*1 + 0.0046*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 7:discrete distance bins:School FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*0 + 0.0132*0 + 0.0046*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 7:discrete distance bins:School FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0090 + 0.0103*1 + 0.0118*0 + 0.0084*0 + 0.0094*0 + 0.0051*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0090 + 0.0103*0 + 0.0118*1 + 0.0084*0 + 0.0094*0 + 0.0051*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*1 + 0.0094*0 + 0.0051*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*0 + 0.0094*1 + 0.0051*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*0 + 0.0094*0 + 0.0051*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0050 + 0.0260*1 + 0.0096*0 + -0.0141*0 + -0.0056*0 + -0.0079*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0050 + 0.0260*0 + 0.0096*1 + -0.0141*0 + -0.0056*0 + -0.0079*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*1 + -0.0056*0 + -0.0079*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*0 + -0.0056*1 + -0.0079*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*0 + -0.0056*0 + -0.0079*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0072 + 0.0170*1 + -0.0048*0 + -0.0193*0 + -0.0007*0 + -0.0167*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0072 + 0.0170*0 + -0.0048*1 + -0.0193*0 + -0.0007*0 + -0.0167*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*1 + -0.0007*0 + -0.0167*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*0 + -0.0007*1 + -0.0167*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*0 + -0.0007*0 + -0.0167*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0098 + 0.0248*1 + 0.0114*0 + -0.0144*0 + -0.0011*0 + -0.0144*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0098 + 0.0248*0 + 0.0114*1 + -0.0144*0 + -0.0011*0 + -0.0144*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*1 + -0.0011*0 + -0.0144*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*0 + -0.0011*1 + -0.0144*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*0 + -0.0011*0 + -0.0144*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE:1500_2000
  geom_function(fun =~ if_else(.x <= 300,(0.0095 + 0.0156*1 + -0.0063*0 + -0.0205*0 + -0.0027*0 + -0.0184*0)*3.28*1.86 , NA),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:WF_300(1)
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , (0.0095 + 0.0156*0 + -0.0063*1 + -0.0205*0 + -0.0027*0 + -0.0184*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*1 + -0.0027*0 + -0.0184*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:500_1000
  geom_function(fun =~ if_else(.x >= 1000 & .x <= 1500 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*0 + -0.0027*1 + -0.0184*0)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:1000_1500
  geom_function(fun =~ if_else(.x >= 1500 & .x <= 2000 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*0 + -0.0027*0 + -0.0184*1)*3.28*1.86,NA),size = x)+ # Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE:1500_2000
  geom_function(fun =~ if_else(.x <= 500, 0.06, NA),size = x)+ # TJ, Pat, Richard - Model 1:discrete distance bin:500
  geom_function(fun =~ if_else(.x <= 250, 0.063, NA),size = x)+# TJ, Pat, Richard - Model 2:discrete distance bin: 0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 , 0.056,NA),size = x)+ # TJ, Pat, Richard - Model 2:discrete distance bin: 250_500
  geom_function(fun =~ if_else(.x <= 250, 0.056, NA),size = x)+# TJ, Pat, Richard - Model 3:discrete distance bin: 0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 , 0.007,NA),size = x)+ # TJ, Pat, Richard - Model 3:discrete distance bin: 250_500
  geom_function(fun =~ if_else(.x <= 500, 0.06, NA),size = x)+ # TJ, Pat, Richard - Model 4:discrete distance bin:500
  geom_function(fun =~ if_else(.x <= 250, 0.061, NA),size = x)+# TJ, Pat, Richard - Model 5:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 , 0.059,NA),size = x)+ # TJ, Pat, Richard - Model 5:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 250, 0.087, NA),size = x)+# TJ, Pat, Richard - Model 6:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 , 0.063,NA),size = x)+ # TJ, Pat, Richard - Model 6:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 250, 0.063, NA),size = x)+# TJ, Pat, Richard - Model 7:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 , 0.056,NA),size = x)+ # TJ, Pat, Richard - Model 7:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 500, 0.044, NA),size = x)+ # TJ, Pat, Richard - Model 8:discrete distance bin:500
  geom_function(fun =~ if_else(.x <= 250, 0.041, NA),size = x)+# TJ, Pat, Richard - Model 9:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 ,0.048,NA),size = x)+ # TJ, Pat, Richard - Model 9:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 250, 0.041, NA),size = x)+# TJ, Pat, Richard - Model 9:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 ,0.048,NA),size = x)+ # TJ, Pat, Richard - Model 9:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 250, 0.152, NA),size = x)+# TJ, Pat, Richard - Model 10:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 ,0.083,NA),size = x)+# TJ, Pat, Richard - Model 10:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 100, 0.072, NA),size = x)+# TJ, Pat, Richard - Model 11:discrete distance bin:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 200 , 0.062,NA),size = x)+ # TJ, Pat, Richard - Model 11:discrete distance bin:100_200
  geom_function(fun =~ if_else(.x >=200 & .x <= 300 , 0.053,NA),size = x)+ # TJ, Pat, Richard - Model 11:discrete distance bin:200_300
  geom_function(fun =~ if_else(.x >=300 & .x <= 400 , 0.056,NA),size = x)+ # TJ, Pat, Richard - Model 11:discrete distance bin:300_400
  geom_function(fun =~ if_else(.x >=400 & .x <= 500 , 0.062,NA),size = x)+ # TJ, Pat, Richard - Model 11:discrete distance bin:400_500
  geom_function(fun =~ if_else(.x <= 250, 0.058, NA),size = x)+# TJ, Pat, Richard - Model 12:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 ,0.056,NA),size = x)+ # TJ, Pat, Richard - Model 12:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 250, 0.124, NA),size = x)+# TJ, Pat, Richard - Model 13:discrete distance bin:0_250
  geom_function(fun =~ if_else(.x >= 250 & .x <= 500 ,0.074,NA),size = x)+ # TJ, Pat, Richard - Model 13:discrete distance bin:250_500
  geom_function(fun =~ if_else(.x <= 500, 0.031, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.024, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.029, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.018, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.064, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.06, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, -0.04, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.01, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 500, 0.03, NA),size = x) + # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x <= 50, 0.017 + 0.115, NA),size = x)+# Walsh et al 2011a - Model 1:50
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467 ,0.017,NA),size = x)+# Walsh et al 2011a - Model 5-_467
  geom_function(fun =~ if_else(.x <= 50, 0.017 + 0.11, NA),size = x)+# Walsh et al 2011a - Model 1S:50
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467 ,0.017,NA),size = x) +# Walsh et al 2011a - Model 1S:50_467
  geom_function(fun =~ if_else(.x <= 300, 0.126, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.023, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.009,NA),size = x)+#Walsh et al 2017 - Anne Arundel 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.090, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , -0.009, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:Log:500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  0.015,NA),size = x)+#Walsh et al 2017 - Baltimore county 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.033, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , -0.001, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  -0.021,NA),size = x)+#Walsh et al 2017 - Calvert 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.010, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.001, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  -0.003,NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.058, NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.056, NA),size = x)+  #Walsh et al 2017 - Charles 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  0.107,NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.078, NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.008, NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   0.013,NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.096, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.001, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.012,NA),size = x)+#Walsh et al 2017 - Harford 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.142, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.008, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.002,NA),size = x)+#Walsh et al 2017 - Kent 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.062, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.001, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.022,NA),size = x)+#Walsh et al 2017 - Prince George's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.017, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.060, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   0.068,NA),size = x)+#Walsh et al 2017 - Queen Anne's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.091, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.055, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.141,NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.014, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.015, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.017,NA),size = x)+#Walsh et al 2017 - St Mary's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.156, NA),size = x)+ #Walsh et al 2017 - Talbot 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.014, NA),size = x)+ #Walsh et al 2017 - Talbot 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.031,NA),size = x)+#Walsh et al 2017 - Talbot 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.046, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.015, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.010,NA),size = x) +#Walsh et al 2017 - Wicomico 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0585*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0249*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0089*1.45/0.759,NA),size = x) +#Walsh et al 2017 - Anne Arundel 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0293*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0032*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0060*1.45/0.472,NA),size = x) + #Walsh et al 2017 - Baltimore county 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0088*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0174*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0196*1.45/0.929,NA),size = x) +#Walsh et al 2017 - Calvert 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.0024*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0086*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0012*1.45/0.472,NA),size = x) + #Walsh et al 2017 - Cecil 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.041*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0252*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0335*1.45/0.557,NA),size = x) +#Walsh et al 2017 - Charles 1 Year Average:500-1000
  geom_function(fun =~ if_else(.x <= 300, 0.0557*1.45/0.728, NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0076*1.45/0.728, NA),size = x)+  #Walsh et al 2017 - Dorchester 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0079*1.45/0.728,NA),size = x) +#Walsh et al 2017 - Dorchester 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0243*1.45/0.379, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0022*1.45/0.379, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0022*1.45/0.379,NA),size = x) +#Walsh et al 2017 - Harford 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0289*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0120*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0049*1.45/0.406,NA),size = x) +#Walsh et al 2017 - Kent 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0093*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0018*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0023*1.45/0.470,NA),size = x) + #Walsh et al 2017 - Prince George's 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0151*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.041*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0470*1.45/0.783,NA),size = x) +#Walsh et al 2017 - Queen Anne's 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0300*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0207*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0498*1.45/0.683,NA),size = x) +#Walsh et al 2017 - Somerset 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  -0.0375*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0082*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , -0.0115*1.45/0.833,NA),size = x) +#Walsh et al 2017 - St Mary's 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0631*1.45/1.02, NA),size = x)+ #Walsh et al 2017 - St Talbot 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0122*1.45/1.02, NA),size = x)+  #Walsh et al 2017 - St Talbot 1 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0190*1.45/1.02,NA),size = x) + #Walsh et al 2017 - St Talbot 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0018*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0130*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:0:500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0116*1.45/0.399,NA),size = x) +#Walsh et al 2017 - Wicomico 1 Year Average:500:100
  geom_function(fun =~ if_else(.x <= 300, 0.3058, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.1020, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0123,NA),size = x)+#Walsh et al 2017 - Anne Arundel 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.05560, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , -0.0386, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  0.0077,NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.0134, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , -0.0779, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  -0.0653,NA),size = x)+  #Walsh et al 2017 - Calvert 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0010, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , -0.1257, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  -0.0362,NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.6413, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.1764, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,  0.3021,NA),size = x)+#Walsh et al 2017 - Charles 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0607, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0429, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0053,NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.2600, NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0213, NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.0370,NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0745, NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.1147, NA),size = x)+  #Walsh et al 2017 - Kent 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.1083,NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.0090, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.1411, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,   -0.1427,NA),size = x)+#Walsh et al 2017 - Prince George's 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.1310, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.1838, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.1983,NA),size = x)+#Walsh et al 2017 - Queen Anne's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0839, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0632, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.1635,NA),size = x)+#Walsh et al 2017 - Somerset 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.1265, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0855, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.1324,NA),size = x)+#Walsh et al 2017 - St Mary's 3 Year Average:Log:500-1000
  geom_function(fun =~ if_else(.x <= 300, 0.0793, NA),size = x)+ #Walsh et al 2017 - Talbot 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.1082, NA),size = x)+ #Walsh et al 2017 - Talbot 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , -0.0984,NA),size = x)+#Walsh et al 2017 - Talbot 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0751, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0869, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0878,NA),size = x) +#Walsh et al 2017 - Wicomico 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.1660*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0586*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0103*1.45/0.759,NA),size = x) +#Walsh et al 2017 - Anne Arundel 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0191*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0117*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.0015*1.45/0.472,NA),size = x) +#Walsh et al 2017 - Baltimore county 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0133*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0247*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0237*1.45/0.929,NA),size = x) +#Walsh et al 2017 - Calvert 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0023*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0329*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0128*1.45/0.472,NA),size = x) +#Walsh et al 2017 - Cecil 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.2421*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.0670*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,0.1037*1.45/0.557,NA),size = x) +#Walsh et al 2017 - Charles 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0309*1.45/0.728, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0284*1.45/0.728, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0040*1.45/0.728,NA),size = x) +#Walsh et al 2017 - Dorchester 3 Year Average:500-1000
  geom_function(fun =~ if_else(.x <= 300, 0.0760*1.45/0.379, NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0066*1.45/0.379, NA),size = x)+  #Walsh et al 2017 - Harford 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0109*1.45/0.379,NA),size = x) +#Walsh et al 2017 - Harford 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, 0.0277*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0349*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 ,-0.0306*1.45/0.406,NA),size = x) +#Walsh et al 2017 - Kent 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300, -0.0227*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0399*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0439*1.45/0.470,NA),size = x) +#Walsh et al 2017 - Prince George's 3 Year Average:500-1000
  geom_function(fun =~ if_else(.x <= 300,  0.0402*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 , 0.0633*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0664*1.45/0.783,NA),size = x) +#Walsh et al 2017 - Queen Anne's 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0547*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0499*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:0-500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0761*1.45/0.683,NA),size = x) +#Walsh et al 2017 - Somerset 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  -0.0839*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0476*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , -0.0665*1.45/0.833,NA),size = x) +#Walsh et al 2017 - St Mary's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x <= 300,  0.0473*1.45/1.02, NA),size = x)+ #Walsh et al 2017 - St Talbot 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,-0.0149*1.45/1.02, NA),size = x)+ #Walsh et al 2017 - St Talbot 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0226*1.45/1.02,NA),size = x) +#Walsh et al 2017 - St Talbot 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 300,  0.0053*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 3 Year Average:300
  geom_function(fun =~ if_else(.x >= 0 & .x <= 500 ,0.0187*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 3 Year Average:0_500
  geom_function(fun =~ if_else(.x >= 500 & .x <= 1000 , 0.0190*1.45/0.399,NA),size = x)+#Walsh et al 2017 - Wicomico 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x <= 500, 2704.7*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 1750.7*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x <= 500, 782.4*0.00190,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 3196.1*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x <= 500, 2479.9*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x <= 500, 1301.7*0.00280,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x <= 500, 1424.9*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x <= 500, 410.4*-0.00500,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x <= 500, 312.7*0.00240,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x <= 500, 1071.3*0.00160,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x <= 500, 2006.9*0.00020,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x <= 500, 4367.5*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x <= 500, 1220.4*0.00370,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x <= 500, 282.8*0.00440,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x <= 500, 2357.8*0.00020,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x <= 500, 1115.7*0.00100,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x <= 500, 3038.6*0.00020,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x <= 500, 749.6*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x <= 500, 976.9*-0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x <= 500, 1376.1*0.00100,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x <= 500, 259.8*0.00820,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x <= 500, 38.8*0.09360,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x <= 500, 90.4*0.02720,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x <= 500, 1277.8*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x <= 500, 1327.7*-0.00020,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00480,NA),size = x)+ # Wolf et al 2022 - Model 27
  geom_function(fun =~ if_else(.x <= 500, 2704.7*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 1750.7*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x <= 500, 782.4*0.00180,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 3196.1*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x <= 500, 2479.9*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x <= 500, 1301.7*0.00290,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x <= 500, 1424.9*-0.00020,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x <= 500, 410.4*-0.00570,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x <= 500, 312.7*0.00200,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x <= 500, 1071.3*0.00100,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x <= 500, 2006.9*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x <= 500, 4367.5*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x <= 500, 1220.4*0.00370,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x <= 500, 282.8*0.00520,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x <= 500, 2357.8*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x <= 500, 1115.7*0.00140,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x <= 500, 3038.6*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x <= 500, 749.6*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x <= 500, 976.9*0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x <= 500, 1376.1*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x <= 500, 259.8*0.00940,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x <= 500, 38.8*0.09800,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x <= 500, 90.4*0.02600,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x <= 500, 1277.8*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x <= 500, 1327.7*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00350,NA),size = x)+ # Wolf et al 2022 - Model 27
  geom_function(fun =~ if_else(.x <= 500, 2704.7*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 1750.7*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x <= 500, 782.4*0.00120,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 3196.1*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x <= 500, 2479.9*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x <= 500, 1301.7*0.00540,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x <= 500, 1424.9*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x <= 500, 410.4*-0.01180,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x <= 500, 312.7*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x <= 500, 1071.3*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x <= 500, 2006.9*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x <= 500, 4367.5*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x <= 500, 1220.4*0.00510,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x <= 500, 282.8*0.00340,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x <= 500, 2357.8*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x <= 500, 1115.7*0.00180,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x <= 500, 3038.6*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x <= 500, 749.6*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x <= 500, 976.9*0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x <= 500, 1376.1*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x <= 500, 259.8*0.00850,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x <= 500, 38.8*0.08330,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x <= 500, 90.4*0.03240,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x <= 500, 1277.8*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x <= 500, 1327.7*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00150,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00590,NA),size = x)+ # Wolf et al 2022 - Model 27
  geom_function(fun =~ if_else(.x <= 500, 2704.7*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x <= 500, 1750.7*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x <= 500, 782.4*0.00190,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x <= 500, 3196.1*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x <= 500, 2479.9*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x <= 500, 1301.7*0.00280,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x <= 500, 1424.9*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x <= 500, 410.4*-0.00630,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x <= 500, 312.7*0.00180,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x <= 500, 1071.3*0.00160,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x <= 500, 2006.9*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x <= 500, 4367.5*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x <= 500, 1220.4*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x <= 500, 282.8*0.00470,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x <= 500, 2357.8*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x <= 500, 1115.7*0.00120,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x <= 500, 3038.6*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x <= 500, 749.6*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x <= 500, 976.9*0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x <= 500, 1376.1*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x <= 500, 259.8*0.00850,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x <= 500, 38.8*0.08330,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x <= 500, 90.4*0.03240,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x <= 500, 1277.8*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x <= 500, 1327.7*-0.00060,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x <= 500, 688.5*0.00420,NA),size = x,aes(col = "Primary Study Estimations"))+ # Wolf et al 2022 - Model 27
  #lin+distfum
  geom_function(fun =~ if_else(.x <= 300,(0.0876 + 0.1973*1 + (-0.0287* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000,(0.0876 + 0.1973*0 + (-0.0287* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 161,(0.0914 + 0.1568*1 + (-0.0292* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 161m (1)
  geom_function(fun =~ if_else(.x >= 161 & .x <= 2000,(0.0914 + 0.1568*0 + (-0.0292* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 161m (0)
  geom_function(fun =~ if_else(.x <= 482,(0.0937 + 0.0772*1 + (-0.0294* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 482m (1)
  geom_function(fun =~ if_else(.x >= 482 & .x <= 2000,(0.0937 + 0.0772*0 + (-0.0294* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 482m (0)
  geom_function(fun =~ if_else(.x <= 804,(0.0951 + 0.02*1 + (-0.0302* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 804m (1)
  geom_function(fun =~ if_else(.x >= 804 & .x <= 2000,(0.0951 + 0.02*0 + (-0.0302* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 804m (0)
  geom_function(fun =~ if_else(.x <= 965,(0.0961 + 0.0055*1 + (-0.0304* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 965m (1)
  geom_function(fun =~ if_else(.x >= 965 & .x <= 2000,(0.0961 + 0.0055*0 + (-0.0304* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 965m (0)
  geom_function(fun =~ if_else(.x <= 300,(0.0742 + 0.1978*1 + (-0.0237* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.0742 + 0.1978*0 + (-0.023* .x/1609.34))*0.542544*3.28,NA),size = x) +# Liu et al 2019 - waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 161,(0.0783 + 0.1559*1 + (-0.0236* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 161m (1)
  geom_function(fun =~ if_else(.x >= 161 & .x <= 2000,(0.0783 + 0.1559*0 + (-0.0236* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 161m (0)
  geom_function(fun =~ if_else(.x <= 482,(0.0809 + 0.077*1 + (-0.0238* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 482m (1)
  geom_function(fun =~ if_else(.x >= 482 & .x <= 2000,(0.0809 + 0.077*0 + (-0.0238* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 482m (0)
  geom_function(fun =~ if_else(.x <= 804,(0.0822 + 0.0285*1 + (-0.0245* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 804m (1)
  geom_function(fun =~ if_else(.x >= 804 & .x <= 2000,(0.0822 + 0.0285*0 + (-0.0245* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 804m (0)
  geom_function(fun =~ if_else(.x <= 965,(0.0834 + 0.0148*1 + (-0.0249* .x/1609.34))*0.542544*3.28,NA),size = x)+ # Liu et al 2019 - waterfron adjucent 965m (1)
  geom_function(fun =~ if_else(.x >= 965 & .x <= 2000,(0.0834 + 0.0148*0 + (-0.0249* .x/1609.34))*0.542544*3.28,NA),size = x) + # Liu et al 2019 - waterfron adjucent 965m (0)
  geom_function(fun =~ if_else(.x <= 300,(4165.1918 + 63099.4773*1 + (96.6563* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x)+ # Liu et al 2019 - waterfron adjucent 300m (1) linear
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000,(4165.1918 + 63099.4773*0 + (96.6563* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - waterfron adjucent 300m (0) linear
  geom_function(fun =~ if_else(.x <= 161,(5450.1438 + 48099.1068*1 + (-80.7421* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x)+ # Liu et al 2019 - waterfron adjucent 161m (1) linear
  geom_function(fun =~ if_else(.x >= 161 & .x <= 2000,(5450.1438 + 48099.1068*0 + (-80.7421* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - waterfron adjucent 161m (0) linear
  geom_function(fun =~ if_else(.x <= 482,(6215.4558 + 21609.3494*1 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x)+ # Liu et al 2019 - waterfron adjucent 482m (1) linear
  geom_function(fun =~ if_else(.x >= 482 & .x <= 2000, (6215.4558 + 21609.3494*0 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - waterfron adjucent 482m (0) linear
  geom_function(fun =~ if_else(.x <= 804,(6215.4558 + 21609.3494*1 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x)+ # Liu et al 2019 - waterfron adjucent 804m (1) linear
  geom_function(fun =~ if_else(.x >= 804 & .x <= 2000, (6215.4558 + 21609.3494*0 + (-169.2611* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - waterfron adjucent 804m (0) linear
  geom_function(fun =~ if_else(.x <= 965,(6807.1986 + 3826.6165*1 + (-437.7854* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x)+ # Liu et al 2019 - waterfron adjucent 965m (1) linear
  geom_function(fun =~ if_else(.x >= 965 & .x <= 2000, (6807.1986 + 3826.6165*0 + (-437.7854* .x/1609.34))*(0.542544*3.28/217951.2),NA),size = x) + # Liu et al 2019 - waterfron adjucent 965m (0) linear
  geom_function(fun =~ if_else(.x <= 300,(0.0084 + 0.0099*1 + (-0.0016*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 1 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.0084 + 0.0099*0 + (-0.0016*.x)/1000)*1.862328*3.28,NA),size = x) + # Nepf et al 2022 - Model 1 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300,(0.0208 + 0.001*1 + (-0.0029*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 2 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000,(0.0208 + 0.001*0 + (-0.0029*.x)/1000)*1.862328*3.28 ,NA),size = x) + # Nepf et al 2022 - Model 2 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300,(0.0191 + 0.004*1 + (-0.0022*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 3 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.0191 + 0.004*0 + (-0.0022*.x)/1000)*1.862328*3.28 ,NA),size = x) + # Nepf et al 2022 - Model 3 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300, (0.0221 + -0.0003*1 + (-0.0031*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 4 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.0221 + 0.004*0 + (-0.0003*.x)/1000)*1.862328*3.28,NA),size = x) + # Nepf et al 2022 - Model 4 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300, (0.0026 + 0.0277*1 + (0.0005*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 9 - continious distance/regional analysis waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.0026 + 0.0277*0  + (0.0005*.x)/1000)*1.862328*3.28,NA),size = x) + # Nepf et al 2022 - Model 9 - continious distance/regional analysis waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300,(-0.0016 + 0.0262*1 + (0.0012*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 10 - continious distance/regional analysis waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (-0.0016 + 0.0262*0  + (0.0012*.x)/1000)*1.862328*3.28,NA),size = x) + # Nepf et al 2022 - Model 10 - continious distance/regional analysis waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300,(0.007 + 0.0263*1 + (0.0004*.x)/1000)*1.862328*3.28,NA),size = x)+ # Nepf et al 2022 - Model 11 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (0.007 + 0.0263*0  + (0.0004*.x)/1000)*1.862328*3.28,NA),size = x) + # Nepf et al 2022 - Model 11 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 300,(-0.009 + 0.0266*1 + (0.0016*.x)/1000)*1.862328*3.28,NA),size = x)+ #  Nepf et al 2022 - Model 12 - continious distance waterfron adjucent 300m (1)
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000, (-0.009 + 0.0266*0  + (0.0016*.x)/1000)*1.862328*3.28,NA),size = x) + #  Nepf et al 2022 - Model 12 - continious distance waterfron adjucent 300m (0)
  geom_function(fun =~ if_else(.x <= 50,0.117 + 0.08*1 + -0.017* log(.x),NA),size = x)+ #  Walsh et al 2011a - Model 2 waterfron adjucent 50m (1)
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467,  0.117 + 0.08*0 + -0.017* log(.x) ,NA),size = x) + #  Walsh et al 2011a - Model 2 waterfron adjucent 50m (0)
  geom_function(fun =~ if_else(.x <= 50,0.117 + 0.08*1 + -0.017* log(.x) + 0.011*log(519.2999878),NA),size = x)+ #  Walsh et al 2011a - Model 3 waterfron adjucent 50m (1)
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467,  0.117 + 0.08*0 + -0.017* log(.x) + 0.011*log(519.2999878) ,NA),size = x) + #  Walsh et al 2011a - Model 3 waterfron adjucent 50m (0)
  geom_function(fun =~ if_else(.x <= 50,0.118 + 0.081*1 + -0.017* log(.x),NA),size = x)+ #  Walsh et al 2011a - Model 2S waterfron adjucent 50m (1)
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467,  0.118 + 0.08*0 + -0.017* log(.x) ,NA),size = x) + #  Walsh et al 2011a - Model 2S waterfron adjucent 50m (0)
  geom_function(fun =~ if_else(.x <= 50,-0.03 + 0.079*1 + -0.017* log(.x) + 0.012*log(519.2999878),NA),size = x)+ #  Walsh et al 2011a - Model 3S waterfron adjucent 50m (1)
  geom_function(fun =~ if_else(.x >= 50 & .x <= 467,  0.117 + 0.08*0 + -0.017* log(.x) + 0.011*log(519.2999878) ,NA),size = x, aes(col = "Primary Study Estimations"))+ #  Walsh et al 2011a - Model 3S waterfron adjucent 50m (0)
  #geom_point(aes(color = study_name), size=0.5)+
  #geom_point(aes(color = study_name), size=0.5)+
  geom_smooth(method = "gam", aes(color = "Smoothed line (GAM)"))+
  scale_colour_manual(values = c("Smoothed line (GAM)"="red",
                                 "Primary Study Estimations"="black"))+
  theme_bw()+
  labs(x = "Distance to waterbody (meters)", y = "Elasticity")+
  scale_x_continuous(breaks=seq(0, 2050, 250),expand=c(0, 0), limits=c(0, 2050))+
  scale_y_continuous(expand=c(0, 0), limits=c(0, 0.5))+
  labs(color='')+
  theme(legend.position = "bottom")+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(
        legend.text = element_text(size=12),
        axis.text = element_text(size=12),           # Increases the axis tick labels
        axis.title = element_text(size=12))

p_all



# Save the plot to a file 
ggsave("./results/Figures/Figure3.png", plot = p_all, width = 9, height = 6, dpi = 300, bg = "white")



# save geom_smooth all

df <- rbind(df1,df2,df3,df4)

write_csv(df, "./metadata/Meta_dataset_from_geomsmooth.csv") 

#######EXTRA##########EXTRA###########EXTRA###########EXTRA
# All together

ggplot(df, aes(dist, elast)) +
  geom_point(size=0.005)+
  geom_smooth(method = "loess", span = 0.3)


# Select subset where elasticity vary between 0-0.5

df_sub <- df%>%
  filter(elast > 0 & elast < 0.5)

ggplot(df_sub, aes(dist, elast)) +
  #geom_line(aes(color= distance_spec),size=0.005)+
  geom_smooth(method = "gam", span = 0.07, aes(colour = " Rolling Mean"))+
  scale_x_continuous(breaks=seq(0, 2000, 250), expand=c(0, 0), limits=c(0, 2000))+
  scale_y_continuous(expand=c(0, 0), limits=c(0, 0.5))+
  theme_bw()+
  labs(x = "Distance to waterbody (Meters)", y = "Elasticity")+
  labs(color='Distance Specification')+
  theme(legend.position = "bottom")


library(readr)
write_csv(df, "./metadata/Meta_dataset_from_geomsmooth.csv") 
  
##############################################################
# All geomsmooth in one
  

df_all_sub <- df%>%
  select(-distance_spec)%>%
  mutate(distance_spec = "Across all Specifications")

df_1 <- rbind(df, df_all_sub)


ggplot(df_1, aes(x = dist, y = elast, colour = distance_spec)) + 
  geom_smooth(method ="gam",span = 0.07) + theme_bw() + 
  scale_x_continuous(breaks=seq(0, 2050, 250), expand=c(0, 0), limits=c(0, 2050))+
  scale_y_continuous(expand=c(0, 0), limits=c(0, 0.5))+
  scale_colour_discrete("")+
  labs(x = "Distance to waterbody (meters)", y = "Elasticity")+
  labs(color='Distance Specification')+
  theme(legend.position = "bottom")



