base_dummy <-
  ggplot() +
  xlim(0, 2000)

x =0.1

base_dummy + geom_function(fun =~ if_else(.x <= 200, -0.099399999*-1, ifelse(.x > 200 & .x < 500, -0.0058*-1,NA)),size = x)+ #Guignet et al 2017
  geom_function(fun =~ if_else(.x <= 100,0.0076 + 0.1673, if_else(.x > 100 & .x <= 300 , 0.0076 + 0.0405,if_else(.x > 300 & .x <= 2000 , 0.0076,NA))),size = x) +# Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year
  geom_function(fun =~ if_else(.x <= 100, 0.0199 + 0.1778, if_else(.x > 100 & .x <= 300 , 0.0199 + 0.0435, if_else(.x > 300 & .x <= 2000 , 0.0199,NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County*year
  geom_function(fun =~ if_else(.x <= 100, -0.0383 + 0.1758, if_else(.x > 100 & .x <= 300 , -0.0383 + 0.0477, if_else(.x > 300 & .x <= 2000 , -0.0383,NA))),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/FE:State*year
  geom_function(fun =~ if_else(.x <= 100, -0.0105 + 0.1674, if_else(.x > 100 & .x <= 300 , -0.0105 + 0.0437, if_else(.x > 300 & .x <= 2000 , -0.0105,NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract+year
  geom_function(fun =~ if_else(.x <= 100, 0.0141 + 0.1804, if_else(.x > 100 & .x <= 300 , 0.0141 + 0.0475, if_else(.x > 300 & .x <= 2000 , 0.0141,NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:County+year  
  geom_function(fun =~ if_else(.x <= 100, -0.0343 + 0.1797, if_else(.x > 100 & .x <= 300 , -0.0343 + 0.0514, if_else(.x > 300 & .x <= 2000 , -0.0343,NA))),size = x)+ # Mamun et al 2023 (Model 1 - discrete distance bins/FE:State+year)
  geom_function(fun =~ if_else(.x <= 100, 0.0287 + 0.1581, if_else(.x > 100 & .x <= 300 , 0.0287 + 0.0388, if_else(.x > 300 & .x <= 2000 , 0.0287,NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Block*year
  geom_function(fun =~ if_else(.x <= 100, 0.0108 + 0.1568, if_else(.x > 100 & .x <= 300 , 0.0108 + 0.0413, if_else(.x > 300 & .x <= 2000 , 0.0108,NA))),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/FE:Block+year  
  geom_function(fun =~ if_else(.x <= 100, -0.0051 + 0.2315, if_else(.x > 100 & .x <= 300 , -0.0051 + 0.0726, if_else(.x > 300 & .x <= 2000 , -0.0051,NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:year
  geom_function(fun =~ if_else(.x <= 100, -0.0123 + 0.1678, if_else(.x > 100 & .x <= 300 , -0.0123 + 0.0439, if_else(.x > 300 & .x <= 2000 , -0.0123,NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:tract:
  geom_function(fun =~ if_else(.x <= 100, 0.0085 + 0.1573, if_else(.x > 100 & .x <= 300 , 0.0085 + 0.0414, if_else(.x > 300 & .x <= 2000 , 0.0085,NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:2
  geom_function(fun =~ if_else(.x <= 100, 0.0072 + 0.1532, if_else(.x > 100 & .x <= 300 , 0.0072 + 0.0401, if_else(.x > 300 & .x <= 2000 , 0.0072,NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:3
  geom_function(fun =~ if_else(.x <= 100, 0.0136 + 0.1586, if_else(.x > 100 & .x <= 300 , 0.0136 + 0.0455, if_else(.x > 300 & .x <= 2000 , 0.0136,NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4:5
  geom_function(fun =~ if_else(.x <= 100, 0.0101 + 0.1666, if_else(.x > 100 & .x <= 300 , 0.0101 + 0.055, if_else(.x > 300 & .x <= 2000 , 0.0101,NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4
  geom_function(fun =~ if_else(.x <= 100, -0.0018 + 0.1377, if_else(.x > 100 & .x <= 300 , -0.0018 + 0.0497, if_else(.x > 300 & .x <= 2000 , -0.0018,NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S4
  geom_function(fun =~ if_else(.x <= 100, 0.0108 + 0.1654, if_else(.x > 100 & .x <= 300 , 0.0108 + 0.0417, if_else(.x > 300 & .x <= 2000 , 0.0108,NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:10
  geom_function(fun =~ if_else(.x <= 100, 0.0225 + 0.1582, if_else(.x > 100 & .x <= 300 , 0.0225 + 0.0472, if_else(.x > 300 & .x <= 2000 , 0.0225,NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:30
  geom_function(fun =~ if_else(.x <= 100, 0.0231 + 0.129, if_else(.x > 100 & .x <= 300 , 0.0231 + 0.0461, if_else(.x > 300 & .x <= 2000 , 0.0231,NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S5:100
  geom_function(fun =~ if_else(.x <= 100, 0.0104 + 0.1461, if_else(.x > 100 & .x <= 300 , 0.0104 + 0.0364, if_else(.x > 300 & .x <= 2000 , 0.0104,NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S6:5
  geom_function(fun =~ if_else(.x <= 300, 0.0123 + 0.1563, if_else(.x > 300 & .x <= 2000 , 0.0123,NA)),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:1
  geom_function(fun =~ if_else(.x <= 50, 0.0424 + 0.0953, if_else(.x > 50 & .x <= 2000 , 0.0424,NA)),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:2
  geom_function(fun =~ if_else(.x <= 100, 0.0157 + 0.151, if_else(.x > 100 & .x <= 2000 , 0.0157,NA)),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:3)
  geom_function(fun =~ if_else(.x <= 200, 0.0076 + 0.1296, if_else(.x > 200 & .x <= 2000 , 0.0076,NA)),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:4
  geom_function(fun =~ if_else(.x <= 50, 0.022 + 0.1439, if_else(.x > 50 & .x <= 200 , 0.022 + 0.1134,if_else(.x > 200 & .x <= 2000 , 0.022, NA))),size = x)+ # Mamun et al 2023-Model 1:discrete distance bins/Table S7:5
  geom_function(fun =~ if_else(.x <= 100, 0.0104 + 0.1636, if_else(.x > 100 & .x <= 200 , 0.0104 + 0.0507,if_else(.x > 200 & .x <= 2000 , 0.0104, NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:6
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, if_else(.x > 100 & .x <= 300 , 0.0076 + 0.0405,if_else(.x > 300 & .x <= 2000 , 0.0076, NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:7
  geom_function(fun =~ if_else(.x <= 200, 0.0037 + 0.1359, if_else(.x > 200 & .x <= 500 , 0.0037 + 0.0132,if_else(.x > 500 & .x <= 2000 , 0.0037, NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:8
  geom_function(fun =~ if_else(.x <= 300, 0.0019 + 0.1089, if_else(.x > 300 & .x <= 1000 , 0.0019 + 0.0031,if_else(.x > 1000 & .x <= 2000 , 0.0019, NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S7:9
  geom_function(fun =~ if_else(.x <= 100, 0.0041 + 0.155, if_else(.x > 100 & .x <= 300 , 0.0041 + 0.041,if_else(.x > 300 & .x <= 2000 , 0.0041, NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:0
  geom_function(fun =~ if_else(.x <= 100, 0.0166 + 0.1579, if_else(.x > 100 & .x <= 300 , 0.0166 + 0.0375,if_else(.x > 300 & .x <= 2000 , 0.0166, NA))),size = x)+ # Mamun et al 2023- Model 1:discrete distance bins/Table S8:1
  geom_function(fun =~ if_else(.x <= 100, 0.0121 + 0.1645, if_else(.x > 100 & .x <= 300 , 0.0121 + 0.0393,if_else(.x > 300 & .x <= 2000 , 0.0121, NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:3
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, if_else(.x > 100 & .x <= 300 , 0.0076 + 0.0405,if_else(.x > 300 & .x <= 2000 , 0.0076, NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:5
  geom_function(fun =~ if_else(.x <= 100, 0.0072 + 0.1688, if_else(.x > 100 & .x <= 300 , 0.0072 + 0.041,if_else(.x > 300 & .x <= 2000 , 0.0072, NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:10
  geom_function(fun =~ if_else(.x <= 100, 0.0078 + 0.1728, if_else(.x > 100 & .x <= 300 , 0.0078 + 0.0382,if_else(.x > 300 & .x <= 2000 , 0.0078, NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S8:anytime
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, if_else(.x > 100 & .x <= 300 , 0.0076 + 0.0405,if_else(.x > 300 & .x <= 2000 , 0.0076, NA))),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/Table S9:customer price index
  geom_function(fun =~ if_else(.x <= 100, 0.0072 + 0.1532, if_else(.x > 100 & .x <= 300 , 0.0072 + 0.0401,if_else(.x > 300 & .x <= 2000 , 0.0072, NA))),size = x)+ # Mamun et al 2023 - Model 1 - discrete distance bins/Table S9:snot easonal adjusted housing price index
  geom_function(fun =~ if_else(.x <= 100, 0.0126 + 0.1148, if_else(.x > 100 & .x <= 300 , 0.0126 + 0.0184,if_else(.x > 300 & .x <= 2000 , 0.0126, NA))),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:1
  geom_function(fun =~ if_else(.x <= 100, 0.0173 + 0.1676, if_else(.x > 100 & .x <= 300 , 0.0173 + 0.0401,if_else(.x > 300 & .x <= 2000 , 0.0173, NA))),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:2
  geom_function(fun =~ if_else(.x <= 100, 0.0076 + 0.1673, if_else(.x > 100 & .x <= 300 , 0.0076 + 0.0405,if_else(.x > 300 & .x <= 2000 , 0.0076, NA))),size = x)+ # Mamun et al 2023 - Model 2:discrete distance bins/Table S10:4
  geom_function(fun =~ if_else(.x <= 100, 0.007 + 0.0789, if_else(.x > 100 & .x <= 300 , 0.007 + 0.0227,if_else(.x > 300 & .x <= 2000 , 0.007, NA))),size = x)+ # Mamun et al 2023 -Model 3:discrete distance bins/Table S11:ln(housing price)):
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.0469, if_else(.x > 100 & .x <= 300 , 0.0113 + 0.0204  + 0.0155,if_else(.x > 300 & .x <= 2000 , 0.0113, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:CPL
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + 0.0824, if_else(.x > 100 & .x <= 300 , 0.0113 + 0.0204 + 0.0333,if_else(.x > 300 & .x <= 2000 , 0.0113, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NAP
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.2392, if_else(.x > 100 & .x <= 300 , 0.0113 + 0.0204 + -0.0445,if_else(.x > 300 & .x <= 2000 , 0.0113, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:NPL
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + 0.1601, if_else(.x > 100 & .x <= 300 , 0.0113 + 0.0204 + 0.1002,if_else(.x > 300 & .x <= 2000 , 0.0113, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:SAP)
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.1651, if_else(.x > 100 & .x <= 300 , 0.0113 + 0.0204 + -0.0836,if_else(.x > 300 & .x <= 2000 , 0.0113, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.1553, if_else(.x > 100 & .x <= 300 , 0.0113 + 0.0204 + -0.0477,if_else(.x > 300 & .x <= 2000 , 0.0113, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:TPL
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + 0.0551, if_else(.x > 100 & .x <= 300 , 0.0113 + 0.0204 + 0.0303,if_else(.x > 300 & .x <= 2000 , 0.0113, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:WMT
  geom_function(fun =~ if_else(.x <= 100, 0.0113 + 0.1285 + -0.0765, if_else(.x > 100 & .x <= 300 , 0.0113 + 0.0204 + 0.0002,if_else(.x > 300 & .x <= 2000 , 0.0113, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S12:ecoregion:XER
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1364, if_else(.x > 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0949,if_else(.x > 300 & .x <= 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AL
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.0547, if_else(.x > 100 & .x <= 300 , 0.0134 + 0.0238 + 0.207,if_else(.x > 300 & .x <= 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AR
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.1572, if_else(.x > 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0368, if_else(.x > 300 & .x <= 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:AZ:
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + 0.1312, if_else(.x > 100 & .x <= 300 , 0.0134 + 0.0238 + 0.0552,if_else(.x > 300 & .x <= 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CA)
  geom_function(fun =~ if_else(.x <= 100, 0.0134 + 0.1253 + -0.2389, if_else(.x > 100 & .x <=300 , 0.0134 + 0.0238 + -0.0367,if_else(.x > 300 & .x <=2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CO)
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.1809, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.065,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:CT):0_100
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.5047, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.2419,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:DE):
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.1284, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0018,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:FL
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.5163, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.2384,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:GA
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.0758,if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.1,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IA
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.3359, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.2071,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IL
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0794, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.039,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:IN
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 1.2532, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0888,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:KY
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0273, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0462,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:LA
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0285, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.059,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MA
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.1581, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0346,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MD
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.1299, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.1871,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:ME
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.0913, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0146,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MI
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0672, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.2849,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MO)
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.3935, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.6851,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MS
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.4651, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.3235,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:MT
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.108, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.047,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NC)
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0542, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.1447,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NE
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0968, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0315,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NH
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.2329, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0775,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NJ
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0915, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0197,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NV
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.1677, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.074,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:NY
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.3565, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.051,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OH
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.3811, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.3099,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OK
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.0729, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0604,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:OR
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0623, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0366,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:PA
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.4243, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0761,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+ # Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:RI
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.2388, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.1485,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SC
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0699, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0298,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:SD
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.1608, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.1244,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TN
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.1807, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0074,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:TX
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.9602, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0612,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:UT
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.3189, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0059,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VA)
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + 0.0231, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + 0.0491,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:VT
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.0623, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0217,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WA
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.0069, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.0208,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WI
  geom_function(fun =~ if_else(.x < 100, 0.0134 + 0.1253 + -0.0314, if_else(.x > 100 & .x < 300 , 0.0134 + 0.0238 + -0.1154,if_else(.x > 300 & .x < 2000 , 0.0134, NA))),size = x)+# Mamun et al 2023 - Model 3:discrete distance bins/Table S13:state:WV  
  geom_function(fun =~ if_else(.x < 300,(0.0190*1 + 0.0138*0 + 0.0083*0 + 0.0095*0 + -0.0003*0)*3.28*1.86 , 
                               if_else(.x > 0 & .x < 500 , (0.0190*0 + 0.0138*1 + 0.0083*0 + 0.0095*0 + -0.0003*0)*3.28*1.86,
                                       if_else(.x > 500 & .x < 1000 , (0.0190*0 + 0.0138*0 + 0.0083*1 + 0.0095*0 + -0.0003*0)*3.28*1.86,
                                               if_else(.x > 1000 & .x < 1500 , (0.0190*0 + 0.0138*0 + 0.0083*0 + 0.0095*1 + -0.0003*0)*3.28*1.86,
                                                       if_else(.x > 1500 & .x < 2000 , (0.0190*0 + 0.0138*0 + 0.0083*0 + 0.0095*1 + -0.0003*0)*3.28*1.86,NA))))),size = x)+# Nepf et al 2022 - Model 5:iscrete distance bins:Tract FE
  geom_function(fun =~ if_else(.x < 300,(0.0088 + 0.0108*1 + 0.0110*0 + 0.0077*0 + 0.0088*0 + 0.0040*0)*3.28*1.86 , 
                               if_else(.x > 0 & .x < 500 , (0.0088 + 0.0108*0 + 0.0110*1 + 0.0077*0 + 0.0088*0 + 0.0040*0)*3.28*1.86,
                                       if_else(.x > 500 & .x < 1000 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*1 + 0.0088*0 + 0.0040*0)*3.28*1.86,
                                               if_else(.x > 1000 & .x < 1500 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*0 + 0.0088*1 + 0.0040*0)*3.28*1.86,
                                                       if_else(.x > 1500 & .x < 2000 , (0.0088 + 0.0108*0 + 0.0110*0 + 0.0077*0 + 0.0088*0 + 0.0040*1)*3.28*1.86,NA))))),size = x)+# Nepf et al 2022 - Model 6:discrete distance bins:Town FE
  geom_function(fun =~ if_else(.x < 300,(0.0051 + 0.0155*1 + 0.0139*0 + 0.0116*0 + 0.0132*0 + 0.0046*0)*3.28*1.86 , 
                               if_else(.x > 0 & .x < 500 , (0.0051 + 0.0155*0 + 0.0139*1 + 0.0116*0 + 0.0132*0 + 0.0046*0)*3.28*1.86,
                                       if_else(.x > 500 & .x < 1000 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*1 + 0.0132*0 + 0.0046*0)*3.28*1.86,
                                               if_else(.x > 1000 & .x < 1500 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*0 + 0.0132*1 + 0.0046*0)*3.28*1.86,
                                                       if_else(.x > 1500 & .x < 2000 , (0.0051 + 0.0155*0 + 0.0139*0 + 0.0116*0 + 0.0132*0 + 0.0046*1)*3.28*1.86,NA))))),size = x)+# Nepf et al 2022 - Model 7:discrete distance bins:School FE
  geom_function(fun =~ if_else(.x < 300,(0.0090 + 0.0103*1 + 0.0118*0 + 0.0084*0 + 0.0094*0 + 0.0051*0)*3.28*1.86 , 
                               if_else(.x > 0 & .x < 500 , (0.0090 + 0.0103*0 + 0.0118*1 + 0.0084*0 + 0.0094*0 + 0.0051*0)*3.28*1.86,
                                       if_else(.x > 500 & .x < 1000 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*1 + 0.0094*0 + 0.0051*0)*3.28*1.86,
                                               if_else(.x > 1000 & .x < 1500 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*0 + 0.0094*1 + 0.0051*0)*3.28*1.86,
                                                       if_else(.x > 1500 & .x < 2000 , (0.0090 + 0.0103*0 + 0.0118*0 + 0.0084*0 + 0.0094*0 + 0.0051*1)*3.28*1.86,NA))))),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:Town x Year FE
  geom_function(fun =~ if_else(.x < 300,(0.0050 + 0.0260*1 + 0.0096*0 + -0.0141*0 + -0.0056*0 + -0.0079*0)*3.28*1.86 , 
                               if_else(.x > 0 & .x < 500 , (0.0050 + 0.0260*0 + 0.0096*1 + -0.0141*0 + -0.0056*0 + -0.0079*0)*3.28*1.86,
                                       if_else(.x > 500 & .x < 1000 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*1 + -0.0056*0 + -0.0079*0)*3.28*1.86,
                                               if_else(.x > 1000 & .x < 1500 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*0 + -0.0056*1 + -0.0079*0)*3.28*1.86,
                                                       if_else(.x > 1500 & .x < 2000 , (0.0050 + 0.0260*0 + 0.0096*0 + -0.0141*0 + -0.0056*0 + -0.0079*1)*3.28*1.86,NA))))),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:ract FE
  geom_function(fun =~ if_else(.x < 300,(0.0072 + 0.0170*1 + -0.0048*0 + -0.0193*0 + -0.0007*0 + -0.0167*0)*3.28*1.86 , 
                               if_else(.x > 0 & .x < 500 , (0.0072 + 0.0170*0 + -0.0048*1 + -0.0193*0 + -0.0007*0 + -0.0167*0)*3.28*1.86,
                                       if_else(.x > 500 & .x < 1000 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*1 + -0.0007*0 + -0.0167*0)*3.28*1.86,
                                               if_else(.x > 1000 & .x < 1500 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*0 + -0.0007*1 + -0.0167*0)*3.28*1.86,
                                                       if_else(.x > 1500 & .x < 2000 , (0.0072 + 0.0170*0 + -0.0048*0 + -0.0193*0 + -0.0007*0 + -0.0167*1)*3.28*1.86,NA))))),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:Town FE  
  geom_function(fun =~ if_else(.x < 300,(0.0098 + 0.0248*1 + 0.0114*0 + -0.0144*0 + -0.0011*0 + -0.0144*0)*3.28*1.86 , 
                               if_else(.x > 0 & .x < 500 , (0.0098 + 0.0248*0 + 0.0114*1 + -0.0144*0 + -0.0011*0 + -0.0144*0)*3.28*1.86,
                                       if_else(.x > 500 & .x < 1000 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*1 + -0.0011*0 + -0.0144*0)*3.28*1.86,
                                               if_else(.x > 1000 & .x < 1500 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*0 + -0.0011*1 + -0.0144*0)*3.28*1.86,
                                                       if_else(.x > 1500 & .x < 2000 , (0.0098 + 0.0248*0 + 0.0114*0 + -0.0144*0 + -0.0011*0 + -0.0144*1)*3.28*1.86,NA))))),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:School FE
  geom_function(fun =~ if_else(.x < 300,(0.0095 + 0.0156*1 + -0.0063*0 + -0.0205*0 + -0.0027*0 + -0.0184*0)*3.28*1.86 , 
                               if_else(.x > 0 & .x < 500 , (0.0095 + 0.0156*0 + -0.0063*1 + -0.0205*0 + -0.0027*0 + -0.0184*0)*3.28*1.86,
                                       if_else(.x > 500 & .x < 1000 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*1 + -0.0027*0 + -0.0184*0)*3.28*1.86,
                                               if_else(.x > 1000 & .x < 1500 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*0 + -0.0027*1 + -0.0184*0)*3.28*1.86,
                                                       if_else(.x > 1500 & .x < 2000 , (0.0095 + 0.0156*0 + -0.0063*0 + -0.0205*0 + -0.0027*0 + -0.0184*1)*3.28*1.86,NA))))),size = x)+# Nepf et al 2022 - Model 8:discrete distance bins:regional analysis:TownxYear FE  
  geom_function(fun =~ if_else(.x < 500, 0.06, NA),size = x)+ # TJ, Pat, Richard - Model 1:discrete distance bin:500
  geom_function(fun =~ if_else(.x < 250, 0.063, if_else(.x > 250 & .x < 500 , 0.056,NA)),size = x)+# TJ, Pat, Richard - Model 2:discrete distance bin: 0_250
  geom_function(fun =~ if_else(.x < 250, 0.056,if_else(.x > 250 & .x < 500 , 0.007,NA)),size = x)+# TJ, Pat, Richard - Model 3:discrete distance bin
  geom_function(fun =~ if_else(.x < 500, 0.06, NA),size = x)+ # TJ, Pat, Richard - Model 4:discrete distance bin:500
  geom_function(fun =~ if_else(.x < 250, 0.061, if_else(.x > 250 & .x < 500 , 0.059,NA)),size = x)+# TJ, Pat, Richard - Model 5:discrete distance bin
  geom_function(fun =~ if_else(.x < 250, 0.087, if_else(.x > 250 & .x < 500 , 0.063,NA)),size = x)+# TJ, Pat, Richard - Model 6:discrete distance bin
  geom_function(fun =~ if_else(.x < 250, 0.063, if_else(.x > 250 & .x < 500 , 0.056,NA)),size = x)+# TJ, Pat, Richard - Model 7:discrete distance bin
  geom_function(fun =~ if_else(.x < 500, 0.044, NA),size = x)+ # TJ, Pat, Richard - Model 8:discrete distance bin:500
  geom_function(fun =~ if_else(.x < 250, 0.041, if_else(.x > 250 & .x < 500 ,0.048,NA)),size = x)+# TJ, Pat, Richard - Model 9:discrete distance bin
  geom_function(fun =~ if_else(.x < 250, 0.041, if_else(.x > 250 & .x < 500 ,0.048,NA)),size = x)+# TJ, Pat, Richard - Model 9:discrete distance bin
  geom_function(fun =~ if_else(.x < 250, 0.152, if_else(.x > 250 & .x < 500 ,0.083,NA)),size = x)+# TJ, Pat, Richard - Model 10:discrete distance bin
  geom_function(fun =~ if_else(.x < 100, 0.072, 
                               if_else(.x > 100 & .x < 200 , 0.062,
                                       if_else(.x >200 & .x < 300 , 0.053,
                                               if_else(.x >300 & .x < 400 , 0.056,
                                                       if_else(.x >400 & .x < 500 , 0.062,NA))))),size = x)+# TJ, Pat, Richard - Model 11:discrete distance bin
  geom_function(fun =~ if_else(.x < 250, 0.058, if_else(.x > 250 & .x < 500 ,0.056,NA)),size = x)+# TJ, Pat, Richard - Model 12:discrete distance bin
  geom_function(fun =~ if_else(.x < 250, 0.124, if_else(.x > 250 & .x < 500 ,0.074,NA)),size = x)+# TJ, Pat, Richard - Model 13:discrete distance bin
  geom_function(fun =~ if_else(.x < 500, 0.031, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x < 500, 0.024, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x < 500, 0.029, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x < 500, 0.018, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x < 500, 0.064, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x < 500, 0.06, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x < 500, -0.04, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x < 500, 0.01, NA),size = x)+ # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  geom_function(fun =~ if_else(.x < 500, 0.03, NA),size = x) + # TJ, Pat, Richard - Model 14:discrete distance bin:500m:different fixed effects
  
  geom_function(fun =~ if_else(.x < 50, 0.017 + 0.115, NA),size = x)+# Walsh et al 2011a - Model 1:50
  geom_function(fun =~ if_else(.x > 50 & .x < 467 ,0.017,NA),size = x)+# Walsh et al 2011a - Model 5-_467
  
  geom_function(fun =~ if_else(.x < 50, 0.017 + 0.11, NA),size = x)+# Walsh et al 2011a - Model 1S:50
  geom_function(fun =~ if_else(.x > 50 & .x < 467 ,0.017,NA),size = x) +# Walsh et al 2011a - Model 1S:50_467
  
  geom_function(fun =~ if_else(.x < 300, 0.126, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , 0.023, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.009,NA),size = x)+#Walsh et al 2017 - Anne Arundel 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.090, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , -0.009, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:Log:500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  0.015,NA),size = x)+#Walsh et al 2017 - Baltimore county 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.033, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , -0.001, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  -0.021,NA),size = x)+#Walsh et al 2017 - Calvert 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, -0.010, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , 0.001, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  -0.003,NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.058, NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , 0.056, NA),size = x)+  #Walsh et al 2017 - Charles 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  0.107,NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.078, NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.008, NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   0.013,NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.096, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.001, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   -0.012,NA),size = x)+#Walsh et al 2017 - Harford 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.142, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.008, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   -0.002,NA),size = x)+#Walsh et al 2017 - Kent 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.062, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.001, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   -0.022,NA),size = x)+#Walsh et al 2017 - Prince George's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, -0.017, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.060, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   0.068,NA),size = x)+#Walsh et al 2017 - Queen Anne's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.091, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.055, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.141,NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, -0.014, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.015, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.017,NA),size = x)+#Walsh et al 2017 - St Mary's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.156, NA),size = x)+ #Walsh et al 2017 - Talbot 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.014, NA),size = x)+ #Walsh et al 2017 - Talbot 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.031,NA),size = x)+#Walsh et al 2017 - Talbot 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, -0.046, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.015, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.010,NA),size = x) +#Walsh et al 2017 - Wicomico 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0585*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0249*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 1 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.0089*1.45/0.759,NA),size = x) +#Walsh et al 2017 - Anne Arundel 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0293*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0032*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 1 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.0060*1.45/0.472,NA),size = x) + #Walsh et al 2017 - Baltimore county 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0088*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0174*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 1 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0196*1.45/0.929,NA),size = x) +#Walsh et al 2017 - Calvert 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300, -0.0024*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0086*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 1 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0012*1.45/0.472,NA),size = x) + #Walsh et al 2017 - Cecil 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.041*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0252*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 1 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.0335*1.45/0.557,NA),size = x) +#Walsh et al 2017 - Charles 1 Year Average:500-1000
  geom_function(fun =~ if_else(.x < 300, 0.0557*1.45/0.728, NA),size = x)+ #Walsh et al 2017 - Dorchester 1 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0076*1.45/0.728, NA),size = x)+  #Walsh et al 2017 - Dorchester 1 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.0079*1.45/0.728,NA),size = x) +#Walsh et al 2017 - Dorchester 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0243*1.45/0.379, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0022*1.45/0.379, NA),size = x)+ #Walsh et al 2017 - Harford 1 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.0022*1.45/0.379,NA),size = x) +#Walsh et al 2017 - Harford 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0289*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0120*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 1 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0049*1.45/0.406,NA),size = x) +#Walsh et al 2017 - Kent 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0093*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0018*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 1 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0023*1.45/0.470,NA),size = x) + #Walsh et al 2017 - Prince George's 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300,  0.0151*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.041*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0470*1.45/0.783,NA),size = x) +#Walsh et al 2017 - Queen Anne's 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300,  0.0300*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0207*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 1 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0498*1.45/0.683,NA),size = x) +#Walsh et al 2017 - Somerset 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300,  -0.0375*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0082*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 1 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , -0.0115*1.45/0.833,NA),size = x) +#Walsh et al 2017 - St Mary's 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300,  0.0631*1.45/1.02, NA),size = x)+ #Walsh et al 2017 - St Talbot 1 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0122*1.45/1.02, NA),size = x)+  #Walsh et al 2017 - St Talbot 1 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0190*1.45/1.02,NA),size = x) + #Walsh et al 2017 - St Talbot 1 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300,  0.0018*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0130*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:0:500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0116*1.45/0.399,NA),size = x) +#Walsh et al 2017 - Wicomico 1 Year Average:500:100
  geom_function(fun =~ if_else(.x < 300, 0.3058, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , 0.1020, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0123,NA),size = x)+#Walsh et al 2017 - Anne Arundel 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.05560, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , -0.0386, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  0.0077,NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, -0.0134, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , -0.0779, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  -0.0653,NA),size = x)+  #Walsh et al 2017 - Calvert 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0010, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , -0.1257, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  -0.0362,NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.6413, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , 0.1764, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,  0.3021,NA),size = x)+#Walsh et al 2017 - Charles 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0607, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0429, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0053,NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.2600, NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0213, NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   -0.0370,NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0745, NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.1147, NA),size = x)+  #Walsh et al 2017 - Kent 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   -0.1083,NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, -0.0090, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.1411, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,   -0.1427,NA),size = x)+#Walsh et al 2017 - Prince George's 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.1310, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.1838, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.1983,NA),size = x)+#Walsh et al 2017 - Queen Anne's 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0839, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0632, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.1635,NA),size = x)+#Walsh et al 2017 - Somerset 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, -0.1265, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0855, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.1324,NA),size = x)+#Walsh et al 2017 - St Mary's 3 Year Average:Log:500-1000
  geom_function(fun =~ if_else(.x < 300, 0.0793, NA),size = x)+ #Walsh et al 2017 - Talbot 3 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.1082, NA),size = x)+ #Walsh et al 2017 - Talbot 3 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , -0.0984,NA),size = x)+#Walsh et al 2017 - Talbot 3 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0751, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0869, NA),size = x)+ #Walsh et al 2017 - Wicomico 1 Year Average:Log:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0878,NA),size = x) +#Walsh et al 2017 - Wicomico 1 Year Average:Log:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.1660*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0586*1.45/0.759, NA),size = x)+ #Walsh et al 2017 - Anne Arundel 3 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.0103*1.45/0.759,NA),size = x) +#Walsh et al 2017 - Anne Arundel 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0191*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0117*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Baltimore county 3 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.0015*1.45/0.472,NA),size = x) +#Walsh et al 2017 - Baltimore county 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0133*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0247*1.45/0.929, NA),size = x)+ #Walsh et al 2017 - Calvert 3 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0237*1.45/0.929,NA),size = x) +#Walsh et al 2017 - Calvert 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0023*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0329*1.45/0.472, NA),size = x)+ #Walsh et al 2017 - Cecil 3 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0128*1.45/0.472,NA),size = x) +#Walsh et al 2017 - Cecil 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.2421*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , 0.0670*1.45/0.557, NA),size = x)+ #Walsh et al 2017 - Charles 3 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,0.1037*1.45/0.557,NA),size = x) +#Walsh et al 2017 - Charles 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0309*1.45/0.728, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0284*1.45/0.728, NA),size = x)+ #Walsh et al 2017 - Dorchester 3 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0040*1.45/0.728,NA),size = x) +#Walsh et al 2017 - Dorchester 3 Year Average:500-1000
  geom_function(fun =~ if_else(.x < 300, 0.0760*1.45/0.379, NA),size = x)+ #Walsh et al 2017 - Harford 3 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0066*1.45/0.379, NA),size = x)+  #Walsh et al 2017 - Harford 3 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0109*1.45/0.379,NA),size = x) +#Walsh et al 2017 - Harford 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300, 0.0277*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0349*1.45/0.406, NA),size = x)+ #Walsh et al 2017 - Kent 3 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 ,-0.0306*1.45/0.406,NA),size = x) +#Walsh et al 2017 - Kent 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300, -0.0227*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0399*1.45/0.470, NA),size = x)+ #Walsh et al 2017 - Prince George's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0439*1.45/0.470,NA),size = x) +#Walsh et al 2017 - Prince George's 3 Year Average:500-1000
  geom_function(fun =~ if_else(.x < 300,  0.0402*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 3 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 , 0.0633*1.45/0.783, NA),size = x)+ #Walsh et al 2017 - Queen Anne's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0664*1.45/0.783,NA),size = x) +#Walsh et al 2017 - Queen Anne's 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300,  0.0547*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0499*1.45/0.683, NA),size = x)+ #Walsh et al 2017 - Somerset 3 Year Average:0-500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0761*1.45/0.683,NA),size = x) +#Walsh et al 2017 - Somerset 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300,  -0.0839*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0476*1.45/0.833, NA),size = x)+ #Walsh et al 2017 - St Mary's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , -0.0665*1.45/0.833,NA),size = x) +#Walsh et al 2017 - St Mary's 3 Year Average:0_500
  geom_function(fun =~ if_else(.x < 300,  0.0473*1.45/1.02, NA),size = x)+ #Walsh et al 2017 - St Talbot 3 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,-0.0149*1.45/1.02, NA),size = x)+ #Walsh et al 2017 - St Talbot 3 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0226*1.45/1.02,NA),size = x) +#Walsh et al 2017 - St Talbot 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 300,  0.0053*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 3 Year Average:300
  geom_function(fun =~ if_else(.x > 0 & .x < 500 ,0.0187*1.45/0.399, NA),size = x)+ #Walsh et al 2017 - Wicomico 3 Year Average:0_500
  geom_function(fun =~ if_else(.x > 500 & .x < 1000 , 0.0190*1.45/0.399,NA),size = x)+#Walsh et al 2017 - Wicomico 3 Year Average:500_1000
  geom_function(fun =~ if_else(.x < 500, 2704.7*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x < 500, 1750.7*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x < 500, 782.4*0.00190,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x < 500, 3196.1*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x < 500, 2479.9*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x < 500, 1301.7*0.00280,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x < 500, 1424.9*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x < 500, 410.4*-0.00500,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x < 500, 312.7*0.00240,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x < 500, 1071.3*0.00160,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x < 500, 2006.9*0.00020,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x < 500, 4367.5*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x < 500, 1220.4*0.00370,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x < 500, 282.8*0.00440,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x < 500, 2357.8*0.00020,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x < 500, 1115.7*0.00100,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x < 500, 3038.6*0.00020,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x < 500, 749.6*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x < 500, 976.9*-0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x < 500, 1376.1*0.00100,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x < 500, 259.8*0.00820,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x < 500, 38.8*0.09360,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x < 500, 90.4*0.02720,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x < 500, 1277.8*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x < 500, 1327.7*-0.00020,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00480,NA),size = x)+ # Wolf et al 2022 - Model 27
  geom_function(fun =~ if_else(.x < 500, 2704.7*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x < 500, 1750.7*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x < 500, 782.4*0.00180,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x < 500, 3196.1*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x < 500, 2479.9*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x < 500, 1301.7*0.00290,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x < 500, 1424.9*-0.00020,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x < 500, 410.4*-0.00570,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x < 500, 312.7*0.00200,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x < 500, 1071.3*0.00100,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x < 500, 2006.9*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x < 500, 4367.5*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x < 500, 1220.4*0.00370,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x < 500, 282.8*0.00520,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x < 500, 2357.8*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x < 500, 1115.7*0.00140,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x < 500, 3038.6*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x < 500, 749.6*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x < 500, 976.9*0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x < 500, 1376.1*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x < 500, 259.8*0.00940,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x < 500, 38.8*0.09800,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x < 500, 90.4*0.02600,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x < 500, 1277.8*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x < 500, 1327.7*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00350,NA),size = x)+ # Wolf et al 2022 - Model 27
  geom_function(fun =~ if_else(.x < 500, 2704.7*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x < 500, 1750.7*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x < 500, 782.4*0.00120,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x < 500, 3196.1*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x < 500, 2479.9*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x < 500, 1301.7*0.00540,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x < 500, 1424.9*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x < 500, 410.4*-0.01180,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x < 500, 312.7*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x < 500, 1071.3*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x < 500, 2006.9*0.00040,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x < 500, 4367.5*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x < 500, 1220.4*0.00510,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x < 500, 282.8*0.00340,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x < 500, 2357.8*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x < 500, 1115.7*0.00180,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x < 500, 3038.6*0.00070,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x < 500, 749.6*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x < 500, 976.9*0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x < 500, 1376.1*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x < 500, 259.8*0.00850,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x < 500, 38.8*0.08330,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x < 500, 90.4*0.03240,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x < 500, 1277.8*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x < 500, 1327.7*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00150,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00590,NA),size = x)+ # Wolf et al 2022 - Model 27
  geom_function(fun =~ if_else(.x < 500, 2704.7*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 1
  geom_function(fun =~ if_else(.x < 500, 1750.7*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 2
  geom_function(fun =~ if_else(.x < 500, 782.4*0.00190,NA),size = x)+ # Wolf et al 2022 - Model 3
  geom_function(fun =~ if_else(.x < 500, 3196.1*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 4
  geom_function(fun =~ if_else(.x < 500, 2479.9*0.00060,NA),size = x)+ # Wolf et al 2022 - Model 5
  geom_function(fun =~ if_else(.x < 500, 1301.7*0.00280,NA),size = x)+ # Wolf et al 2022 - Model 6
  geom_function(fun =~ if_else(.x < 500, 1424.9*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 7
  geom_function(fun =~ if_else(.x < 500, 410.4*-0.00630,NA),size = x)+ # Wolf et al 2022 - Model 8
  geom_function(fun =~ if_else(.x < 500, 312.7*0.00180,NA),size = x)+ # Wolf et al 2022 - Model 9
  geom_function(fun =~ if_else(.x < 500, 1071.3*0.00160,NA),size = x)+ # Wolf et al 2022 - Model 10
  geom_function(fun =~ if_else(.x < 500, 2006.9*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 11
  geom_function(fun =~ if_else(.x < 500, 4367.5*-0.00010,NA),size = x)+ # Wolf et al 2022 - Model 12
  geom_function(fun =~ if_else(.x < 500, 1220.4*0.00030,NA),size = x)+ # Wolf et al 2022 - Model 13
  geom_function(fun =~ if_else(.x < 500, 282.8*0.00470,NA),size = x)+ # Wolf et al 2022 - Model 14
  geom_function(fun =~ if_else(.x < 500, 2357.8*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 15
  geom_function(fun =~ if_else(.x < 500, 1115.7*0.00120,NA),size = x)+ # Wolf et al 2022 - Model 16
  geom_function(fun =~ if_else(.x < 500, 3038.6*0.00010,NA),size = x)+ # Wolf et al 2022 - Model 17
  geom_function(fun =~ if_else(.x < 500, 749.6*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 18
  geom_function(fun =~ if_else(.x < 500, 976.9*0.00210,NA),size = x)+ # Wolf et al 2022 - Model 19
  geom_function(fun =~ if_else(.x < 500, 1376.1*0.00090,NA),size = x)+ # Wolf et al 2022 - Model 20
  geom_function(fun =~ if_else(.x < 500, 259.8*0.00850,NA),size = x)+ # Wolf et al 2022 - Model 21
  geom_function(fun =~ if_else(.x < 500, 38.8*0.08330,NA),size = x)+ # Wolf et al 2022 - Model 22
  geom_function(fun =~ if_else(.x < 500, 90.4*0.03240,NA),size = x)+ # Wolf et al 2022 - Model 23
  geom_function(fun =~ if_else(.x < 500, 1277.8*0.00000,NA),size = x)+ # Wolf et al 2022 - Model 24
  geom_function(fun =~ if_else(.x < 500, 1327.7*-0.00060,NA),size = x)+ # Wolf et al 2022 - Model 25
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00050,NA),size = x)+ # Wolf et al 2022 - Model 26
  geom_function(fun =~ if_else(.x < 500, 688.5*0.00420,NA),size = x)+ # Wolf et al 2022 - Model 27