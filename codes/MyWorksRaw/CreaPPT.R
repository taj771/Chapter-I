# Withould distance involvement 


p1 <- base + geom_function(fun =~ if_else(.x <= 300, 0.009,NA), size = x) +
  geom_function(fun =~ if_else(.x <= 2000, 0.248 + -0.076*(log(.x/100)),NA),size = x) +
  geom_function(fun =~ if_else(.x <= 100,0.117 + 0.08*1 + -0.017* log(.x),NA),size = x)+ #  Walsh et al 2011a - Model 2 waterfron adjucent 50m (1)
  geom_function(fun =~ if_else(.x >= 100 & .x <= 467,  0.117 + 0.08*0 + -0.017* log(.x) ,NA),size = x) + #  Walsh et al 2011a - Model 2 waterfron adjucent 50m (0)
  geom_function(fun =~ if_else(.x <= 100,0.0076 + 0.1673, NA),size = x)+# Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year:0_100
  geom_function(fun =~ if_else(.x >= 100 & .x <= 300 , 0.0076 + 0.0405,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year:100_300
  geom_function(fun =~ if_else(.x >= 300 & .x <= 2000 , 0.0076,NA),size = x)+ # Mamun et al 2023 - Model 1:discrete distance bins/FE:Tract*year:300_2000
  
  ggtitle("Hedonic function without distance involvement")+
  xlab("Distance to waterbody (meters)") + ylab("Elasticity")+
  theme_bw()

  p1
  
  
  base <-
    ggplot() +
    xlim(0, 1000)
  x=1
  
  
  p1 <- base +
    geom_function(aes(color = "Line 1"), fun = ~ if_else(.x <= 300, 0.09, NA), size = x) + # nodist
    geom_function(aes(color = "Line 2"), fun = ~ if_else(.x <= 2000, 0.248 + -0.076 * (log(.x / 100)), NA), size = x) + # continious dist 
    geom_function(aes(color = "Line 3"), fun = ~ if_else(.x <= 100, 0.117 + 0.08 * 1 + -0.017 * log(.x), NA), size = x) + # Walsh et al 2011a - Model 2 waterfron adjacent 50m (1)
    geom_function(aes(color = "Line 3"), fun = ~ if_else(.x >= 100 & .x <= 467, 0.117 + 0.08 * 0 + -0.017 * log(.x), NA), size = x) + # Walsh et al 2011a - Model 2 waterfron adjacent 50m (0)
    geom_function(aes(color = "Line 4"), fun = ~ if_else(.x <= 100, 0.0076 + 0.1673, NA), size = x) + # Mamun et al 2023 - Model 1: discrete distance bins/FE: Tract*year: 0_100
    geom_function(aes(color = "Line 4"), fun = ~ if_else(.x >= 100 & .x <= 300, 0.0076 + 0.0405, NA), size = x) + # Mamun et al 2023 - Model 1: discrete distance bins/FE: Tract*year: 100_300
    geom_function(aes(color = "Line 4"), fun = ~ if_else(.x >= 300 & .x <= 2000, 0.0076, NA), size = x) + # Mamun et al 2023 - Model 1: discrete distance bins/FE: Tract*year: 300_2000
    scale_color_manual(values = c("Line 1" = "red", "Line 2" = "blue", "Line 3" = "green", 
                                  "Line 4" = "purple", "Line 5" = "orange", 
                                  "Line 6" = "cyan", "Line 7" = "brown")) +
    labs(color = "Function Lines")+  # Add legend title for colors
    theme_bw()+
    theme(
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank()   # Remove minor grid lines
    )+
    labs(
      x = "Distance to Nearest Lakeshore",  # Label for the x-axis
      y = "Elasticity"   # Label for the y-axis
    )  +
    scale_y_continuous(
      breaks = seq(-0.1, 0.5, by = 0.05)  # Define breaks for the y-axis (adjust as needed)
    )
  
  # Display the plot
  p1
  
  p1 <- base +
    geom_function(aes(color = "Line 1"), fun = ~ if_else(.x <= 300, 0.09, NA), size = x) + # nodist
    scale_color_manual(values = c("Line 1" = "red", "Line 2" = "blue", "Line 3" = "green", 
                                  "Line 4" = "purple", "Line 5" = "orange", 
                                  "Line 6" = "cyan", "Line 7" = "brown")) +
    labs(color = "Function Lines")+  # Add legend title for colors
    theme_bw()+
    theme(
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank()   # Remove minor grid lines
    )+
    labs(
      x = "Distance to Lakeshore",  # Label for the x-axis
      y = "Elasticity"   # Label for the y-axis
    )  +
    scale_y_continuous(
      breaks = seq(0, 0.5, by = 0.01)  # Define breaks for the y-axis (adjust as needed)
    )
  
  # Display the plot
  p1
  
  p1 <- base +
    #geom_function(aes(color = "Line 1"), fun = ~ if_else(.x <= 300, 0.09, NA), size = x) + # nodist
    geom_function(aes(color = "Line 2"), fun = ~ if_else(.x <= 2000, 0.248 + -0.076 * (log(.x / 100)), NA), size = x) + # continious dist 
    scale_color_manual(values = c("Line 1" = "red", "Line 2" = "blue", "Line 3" = "green", 
                                  "Line 4" = "purple", "Line 5" = "orange", 
                                  "Line 6" = "cyan", "Line 7" = "brown")) +
    labs(color = "Function Lines")+  # Add legend title for colors
    theme_bw()+
    theme(
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank()   # Remove minor grid lines
    )+
    labs(
      x = "Distance to Lakeshore",  # Label for the x-axis
      y = "Elasticity"   # Label for the y-axis
    )  +
    scale_y_continuous(
      breaks = seq(-0.1, 0.5, by = 0.05)  # Define breaks for the y-axis (adjust as needed)
    )
  
  # Display the plot
  p1
  
  
  p1 <- base +
    #geom_function(aes(color = "Line 1"), fun = ~ if_else(.x <= 300, 0.09, NA), size = x) + # nodist
    #geom_function(aes(color = "Line 2"), fun = ~ if_else(.x <= 2000, 0.248 + -0.076 * (log(.x / 100)), NA), size = x) + # continious dist 
    geom_function(aes(color = "Line 3"), fun = ~ if_else(.x <= 100, 0.117 + 0.08 * 1 + -0.017 * log(.x), NA), size = x) + # Walsh et al 2011a - Model 2 waterfron adjacent 50m (1)
    geom_function(aes(color = "Line 3"), fun = ~ if_else(.x >= 100 & .x <= 467, 0.117 + 0.08 * 0 + -0.017 * log(.x), NA), size = x) + # Walsh et al 2011a - Model 2 waterfron adjacent 50m (0)
    scale_color_manual(values = c("Line 1" = "red", "Line 2" = "blue", "Line 3" = "green", 
                                  "Line 4" = "purple", "Line 5" = "orange", 
                                  "Line 6" = "cyan", "Line 7" = "brown")) +
    labs(color = "Function Lines")+  # Add legend title for colors
    theme_bw()+
    theme(
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank()   # Remove minor grid lines
    )+
    labs(
      x = "Distance Lakeshore",  # Label for the x-axis
      y = "Elasticity"   # Label for the y-axis
    )  +
    scale_y_continuous(
      breaks = seq(-0.1, 0.5, by = 0.05)  # Define breaks for the y-axis (adjust as needed)
    )
  
  # Display the plot
  p1
  
  
  p1 <- base +
    #geom_function(aes(color = "Line 1"), fun = ~ if_else(.x <= 300, 0.09, NA), size = x) + # nodist
    #geom_function(aes(color = "Line 2"), fun = ~ if_else(.x <= 2000, 0.248 + -0.076 * (log(.x / 100)), NA), size = x) + # continious dist 
    #geom_function(aes(color = "Line 3"), fun = ~ if_else(.x <= 100, 0.117 + 0.08 * 1 + -0.017 * log(.x), NA), size = x) + # Walsh et al 2011a - Model 2 waterfron adjacent 50m (1)
    #geom_function(aes(color = "Line 3"), fun = ~ if_else(.x >= 100 & .x <= 467, 0.117 + 0.08 * 0 + -0.017 * log(.x), NA), size = x) + # Walsh et al 2011a - Model 2 waterfron adjacent 50m (0)
    geom_function(aes(color = "Line 4"), fun = ~ if_else(.x <= 100, 0.0076 + 0.1673, NA), size = x) + # Mamun et al 2023 - Model 1: discrete distance bins/FE: Tract*year: 0_100
    geom_function(aes(color = "Line 4"), fun = ~ if_else(.x >= 100 & .x <= 300, 0.0076 + 0.0405, NA), size = x) + # Mamun et al 2023 - Model 1: discrete distance bins/FE: Tract*year: 100_300
    geom_function(aes(color = "Line 4"), fun = ~ if_else(.x >= 300 & .x <= 2000, 0.0076, NA), size = x) + # Mamun et al 2023 - Model 1: discrete distance bins/FE: Tract*year: 300_2000
    scale_color_manual(values = c("Line 1" = "red", "Line 2" = "blue", "Line 3" = "green", 
                                  "Line 4" = "purple", "Line 5" = "orange", 
                                  "Line 6" = "cyan", "Line 7" = "brown")) +
    labs(color = "Function Lines")+  # Add legend title for colors
    theme_bw()+
    theme(
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank()   # Remove minor grid lines
    )+
    labs(
      x = "Distance to Lakeshore",  # Label for the x-axis
      y = "Elasticity"   # Label for the y-axis
    )  +
    scale_y_continuous(
      breaks = seq(0, 0.5, by = 0.02)  # Define breaks for the y-axis (adjust as needed)
    )
  
  # Display the plot
  p1
  
