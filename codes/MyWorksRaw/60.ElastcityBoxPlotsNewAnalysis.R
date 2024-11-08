# clear memory
rm(list = ls())

library(dplyr)
library(metafor)  # Ensure you have the metafor package for the rma.mv function
library(tidyr)   # For the rownames_to_column function


df <- read_csv("./metadata/Meta_data_at_50m_distcal_elast.csv") %>%
  rename(distance = x) %>%
  rename(elast = y_value) 

# Define the distances you want to iterate over
distances <- seq(100, 1000, by = 100)

for (dist in distances) {
  df_wf <- df %>%
    filter(distance == dist) %>%  # Filter for the current distance
    ungroup() %>%
    mutate(obsid = row_number()) %>%
    group_by(study_name, region) %>%
    mutate(cluster_id = cur_group_id()) %>%
    ungroup() %>%
    mutate(vi = 1 / sampsize) %>%
    mutate(vi_log = 1 / log(sampsize)) %>%
    group_by(cluster_id) %>%
    mutate(count = n()) %>%
    mutate(w_1 = (1 / count)) %>%
    mutate(log_ss = log(sampsize)) %>%
    mutate(log_ss_within_clus = sum(log_ss)) %>%
    mutate(w_2 = log(sampsize) / log_ss_within_clus) %>%
    mutate(ave_ss_wt_clus = mean(sampsize)) %>%
    ungroup() %>%
    mutate(log_ave_ss_wt_clus = log(ave_ss_wt_clus)) %>%
    mutate(w_3 = log_ave_ss_wt_clus / sum(log_ave_ss_wt_clus))
  
  # Check if df_wf has enough observations
  if (nrow(df_wf) <= 1) {
    message(paste("Not enough data for distance", dist))
    next  # Skip this iteration if not enough data
  }
  
  # Run the meta-analysis model with tryCatch to handle errors
  re_wf <- tryCatch({
    rma.mv(yi = elast, vi_log, random = ~ 1 | cluster_id / obsid, data = df_wf, 
           control = list(optimizer = "optim", maxiter = 5000))
  }, error = function(e) {
    message(paste("Error fitting model for distance", dist, ":", e$message))
    return(NULL)
  })
  
  # Skip to the next iteration if the model fitting fails
  if (is.null(re_wf)) next
  
  # Calculate studentized residuals
  rstud_wf <- rstudent.rma.mv(re_wf, cluster = df_wf$cluster_id)
  
  df_wf <- df_wf %>%
    mutate(obsid = row_number())
  
  rstud_wf <- rstud_wf$obs %>%
    as.data.frame()
  
  rstud_wf <- rownames_to_column(rstud_wf, "obsid")
  
  rstud_wf_out <- rstud_wf %>%
    filter(!between(resid, -2, 2))
  
  df_250 <- subset(df_wf, !(obsid %in% rstud_wf_out$obsid))
  
  # Adjust the file name format
  file_name <- ifelse(dist < 1000, sprintf("%04dm", dist), paste0(dist, "m"))
  
  # Save the filtered data frame to a CSV file
  write.csv(df_250, paste0("./metadata/Newmetadata/meta_data_distance_", file_name, ".csv"), row.names = FALSE)
}




# List all files in the metadata directory that match the pattern
files <- list.files("./metadata/NewMetadata", pattern = "meta_data_distance_\\d+m.csv", full.names = TRUE)

# Create an empty list to store the model results for both models
model_results <- list()

# Loop through each file, run the models, and store the results
for (file in files) {
  # Read the CSV file
  df_250 <- read.csv(file)
  
  # Run the first meta-analysis model (random-effects model)
  re_wf <- rma.mv(yi = elast, vi, random = ~ 1 | cluster_id / obsid, data = df_250)
  
  # Run an additional model, e.g., fixed-effects model as an example
  fe_wf <- rma(yi = elast, vi=vi, data=df_250, method="EE", weighted=FALSE, level = 95)
  
  # Store both model results in the list with the file name as the key
  model_results[[basename(file)]] <- list(random_effects = re_wf, fixed_effects = fe_wf)
}

# Optional: Save the model results to an RData file for future use
save(model_results, file = "./metadata/NewMetadata/model_results.RData")

# Print a summary of the random-effects model for the first file as an example
summary(model_results[[10]]$random_effects)

# Print a summary of the fixed-effects model for the first file as an example
summary(model_results[[1]]$fixed_effects)




# Create an empty data frame to store estimates, standard errors, and counts
results_data <- data.frame()

# Loop through each model result and extract estimates and standard errors for both models
for (model_name in names(model_results)) {
  # Random-effects model
  re_model <- model_results[[model_name]]$random_effects
  fe_model <- model_results[[model_name]]$fixed_effects
  
  # Extract estimates (coefficients) and standard errors for both models
  re_estimates <- re_model$b  # Random-effects model estimates
  re_standard_errors <- re_model$se  # Random-effects standard errors
  
  fe_estimates <- fe_model$b  # Fixed-effects model estimates
  fe_standard_errors <- fe_model$se  # Fixed-effects standard errors
  
  # Count the number of observations used in each model
  count <- length(re_model$yi)  # Assuming both models have the same count
  
  # Create a data frame with estimates, standard errors, model info, and count for random-effects
  re_df <- data.frame(
    model_name = model_name,  # Add model name for each estimate
    estimates = re_estimates,  # Add estimates for random-effects model
    std_error = re_standard_errors,  # Add standard errors
    count = count,  # Add the count of observations
    model_type = "Random Effects"  # Specify the model type
  )
  
  # Create a data frame with estimates, standard errors, model info, and count for fixed-effects
  fe_df <- data.frame(
    model_name = model_name,  # Add model name for each estimate
    estimates = fe_estimates,  # Add estimates for fixed-effects model
    std_error = fe_standard_errors,  # Add standard errors
    count = count,  # Add the count of observations
    model_type = "Fixed Effects"  # Specify the model type
  )
  
  # Bind both to the master data frame
  results_data <- rbind(results_data, re_df, fe_df)
}

# Create a vector for custom x-axis labels with counts
custom_labels <- paste0(c(
  "100m", 
  "200m", 
  "300m", 
  "400m", 
  "500m", 
  "600m", 
  "700m", 
  "800m", 
  "900m", 
  "1000m"), 
  "\nN(", unique(results_data$count), ")"
)
# Check unique values of model_type
unique(results_data$model_type)

# Assuming the unique values are "Random Effects" and "Fixed Effects"
# Create the dot plot with error bars and different colors for model types
ggplot(results_data, aes(x = model_name, y = estimates, color = model_type)) +
  geom_point(size = 3) +  # Add dots for the estimates
  geom_errorbar(aes(ymin = estimates - std_error, ymax = estimates + std_error), width = 0.2) +  # Add error bars
  
  theme_minimal() +
  xlab("Distance to the Lakeshore") +
  ylab("Elasticity") +
  ggtitle("") +
  
  scale_x_discrete(labels = custom_labels) +  # Use custom labels with counts
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.02)) +
  
  # Define custom colors for the models and remove the legend title
  scale_color_manual(values = c("Random Effects" = "blue", "Fixed Effects" = "red"), 
                     labels = c("Mean - Unweighted", "Mean - Random Effect"),  # Change displayed legend names
                     name = NULL) +  # Remove the legend title
  
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 14),  # Increase size of x-axis labels
    axis.text.y = element_text(size = 14),  # Increase size of y-axis labels
    axis.title.x = element_text(margin = margin(t = 10), size = 16),  # Increase size of x-axis title
    axis.title.y = element_text(size = 16),  # Increase size of y-axis title
    plot.title = element_text(size = 18),  # Increase size of the plot title
    legend.text = element_text(size = 12),  # Increase size of the legend text
    legend.title = element_text(size = 14),  # Increase size of the legend title
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.line = element_line(color = "black", size = 0.5)  # Set axis lines to black
  )

###########################################################################################################################################
###### Subset Analysis - Regional Heterogeneity

### Midwest

df <- read_csv("./metadata/Meta_data_at_50m_distcal_elast.csv")%>%
  rename(distance=x)%>%
  rename(elast=y_value)%>%
  filter(midwest==1)


# Define the distances you want to iterate over
distances <- seq(100, 1000, by = 100)

for (dist in distances) {
  df_wf <- df %>%
    filter(distance == dist) %>%  # Filter for the current distance
    ungroup() %>%
    mutate(obsid = row_number()) %>%
    group_by(study_name, region) %>%
    mutate(cluster_id = cur_group_id()) %>%
    ungroup() %>%
    mutate(vi = 1 / sampsize) %>%
    mutate(vi_log = 1 / log(sampsize)) %>%
    group_by(cluster_id) %>%
    mutate(count = n()) %>%
    mutate(w_1 = (1 / count)) %>%
    mutate(log_ss = log(sampsize)) %>%
    mutate(log_ss_within_clus = sum(log_ss)) %>%
    mutate(w_2 = log(sampsize) / log_ss_within_clus) %>%
    mutate(ave_ss_wt_clus = mean(sampsize)) %>%
    ungroup() %>%
    mutate(log_ave_ss_wt_clus = log(ave_ss_wt_clus)) %>%
    mutate(w_3 = log_ave_ss_wt_clus / sum(log_ave_ss_wt_clus))
  
  # Check if df_wf has enough observations
  if (nrow(df_wf) <= 1) {
    message(paste("Not enough data for distance", dist))
    next  # Skip this iteration if not enough data
  }
  
  # Run the meta-analysis model with tryCatch to handle errors
  re_wf <- tryCatch({
    rma.mv(yi = elast, vi_log, random = ~ 1 | cluster_id / obsid, data = df_wf, 
           control = list(optimizer = "optim", maxiter = 5000))
  }, error = function(e) {
    message(paste("Error fitting model for distance", dist, ":", e$message))
    return(NULL)
  })
  
  # Skip to the next iteration if the model fitting fails
  if (is.null(re_wf)) next
  
  # Calculate studentized residuals
  rstud_wf <- rstudent.rma.mv(re_wf, cluster = df_wf$cluster_id)
  
  df_wf <- df_wf %>%
    mutate(obsid = row_number())
  
  rstud_wf <- rstud_wf$obs %>%
    as.data.frame()
  
  rstud_wf <- rownames_to_column(rstud_wf, "obsid")
  
  rstud_wf_out <- rstud_wf %>%
    filter(!between(resid, -2, 2))
  
  df_250 <- subset(df_wf, !(obsid %in% rstud_wf_out$obsid))
  
  # Adjust the file name format
  file_name <- ifelse(dist < 1000, sprintf("%04dm", dist), paste0(dist, "m"))
  
  # Save the filtered data frame to a CSV file
  write.csv(df_250, paste0("./metadata/NewmetadataMidwest/meta_data_distance_", file_name, ".csv"), row.names = FALSE)
}





# List all files in the metadata directory that match the pattern
files <- list.files("./metadata/NewmetadataMidwest", pattern = "meta_data_distance_\\d+m.csv", full.names = TRUE)

# Create an empty list to store the model results
model_results <- list()

# Loop through each file, run the model, and store the results
for (file in files) {
  # Read the CSV file
  df_250 <- read.csv(file)
  
  # Run the meta-analysis model
  re_wf <- rma.mv(yi = elast, vi, random = ~ 1 | cluster_id / obsid, data = df_250)
  
  # Store the results in the list with the file name as the key
  model_results[[basename(file)]] <- re_wf
}

# Optional: Save the model results to an RData file for future use
save(model_results, file = "./metadata/NewmetadataMidwest/model_results.RData")


# Create an empty data frame to store estimates, standard errors, and counts
results_data <- data.frame()

# Loop through each model result and extract estimates and standard errors
for (model_name in names(model_results)) {
  # Get the current model
  model <- model_results[[model_name]]
  
  # Extract estimates (coefficients) and standard errors
  estimates <- model$b  # Model estimates
  standard_errors <- model$se  # Standard errors
  
  # Count the number of observations used in the model
  count <- length(model$yi)  # Assuming yi contains the response variable
  
  # Create a data frame with estimates, standard errors, model info, and count
  temp_df <- data.frame(
    model_name = model_name,  # Add model name for each estimate
    estimates = estimates,  # Add estimates
    std_error = standard_errors,  # Add standard errors
    count = count  # Add the count of observations
  )
  
  # Bind to the master data frame
  results_data <- rbind(results_data, temp_df)
}

# Create a vector for custom x-axis labels with counts
custom_labels <- paste0(c(
  "100m", 
  "200m", 
  "300m", 
  "400m", 
  "500m", 
  "600m", 
  "700m", 
  "800m", 
  "900m", 
  "1000m"), 
  "\nN(", results_data$count, ")"
)

# Create the dot plot with error bars and counts at the bottom
ggplot(results_data, aes(x = model_name, y = estimates)) +
  geom_point() +  # Add dots for the estimates
  geom_errorbar(aes(ymin = estimates - std_error, ymax = estimates + std_error), width = 0.2) +  # Add error bars
  
  theme_minimal() +
  xlab("Distance to the Lakeshore") +
  ylab("Elasticity") +
  ggtitle("Variation of Elasticity with Proximity to Lakeshore - Midwest") +
  
  scale_x_discrete(labels = custom_labels) +  # Use custom labels with counts
  
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.02))+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),  # Adjust alignment
    axis.title.x = element_text(margin = margin(t = 10)),  # Increase space above x-axis
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )

######################################################################################

### NorthEast


df <- read_csv("./metadata/Meta_data_at_50m_distcal_elast.csv")%>%
  rename(distance=x)%>%
  rename(elast=y_value)%>%
  filter(northeast==1)


# Define the distances you want to iterate over
distances <- seq(100, 1000, by = 100)  # Change this range as needed

for (dist in distances) {
  df_wf <- df %>%
    filter(distance == dist) %>%  # Filter for the current distance
    ungroup() %>%
    mutate(obsid = row_number()) %>%
    group_by(study_name, region) %>%
    mutate(cluster_id = cur_group_id()) %>%
    ungroup() %>%
    mutate(vi = 1 / sampsize) %>%
    mutate(vi_log = 1 / log(sampsize)) %>%
    group_by(cluster_id) %>%
    mutate(count = n()) %>%
    mutate(w_1 = (1 / count)) %>%
    mutate(log_ss = log(sampsize)) %>%
    mutate(log_ss_within_clus = sum(log_ss)) %>%
    mutate(w_2 = log(sampsize) / log_ss_within_clus) %>%
    mutate(ave_ss_wt_clus = mean(sampsize)) %>%
    ungroup() %>%
    mutate(log_ave_ss_wt_clus = log(ave_ss_wt_clus)) %>%
    mutate(w_3 = log_ave_ss_wt_clus / sum(log_ave_ss_wt_clus))
  
  # Run the meta-analysis model
  re_wf <- rma.mv(yi = elast, vi_log, random = ~ 1 | cluster_id / obsid, data = df_wf, 
                  control = list(optimizer = "optim", maxiter = 5000))
  
  rstud_wf <- rstudent.rma.mv(re_wf, cluster = df_wf$cluster_id)
  
  df_wf <- df_wf %>%
    mutate(obsid = row_number())
  
  rstud_wf <- rstud_wf$obs %>%
    as.data.frame()
  
  rstud_wf <- rownames_to_column(rstud_wf, "obsid")
  
  rstud_wf_out <- rstud_wf %>%
    filter(!between(resid, -2, 2))
  
  df_250 <- subset(df_wf, !(obsid %in% rstud_wf_out$obsid))
  
  # Adjust the file name format
  file_name <- ifelse(dist < 1000, sprintf("%04dm", dist), paste0(dist, "m"))
  
  # Save the filtered data frame to a CSV file
  write.csv(df_250, paste0("./metadata/NewmetadataNorthEast/meta_data_distance_", file_name, ".csv"), row.names = FALSE)
  
}



# List all files in the metadata directory that match the pattern
files <- list.files("./metadata/NewmetadataNorthEast", pattern = "meta_data_distance_\\d+m.csv", full.names = TRUE)

# Create an empty list to store the model results
model_results <- list()

# Loop through each file, run the model, and store the results
for (file in files) {
  # Read the CSV file
  df_250 <- read.csv(file)
  
  # Run the meta-analysis model
  re_wf <- rma.mv(yi = elast, vi, random = ~ 1 | cluster_id / obsid, data = df_250)
  
  # Store the results in the list with the file name as the key
  model_results[[basename(file)]] <- re_wf
}

# Optional: Save the model results to an RData file for future use
save(model_results, file = "./metadata/NewmetadataNorthEast/model_results.RData")


# Create an empty data frame to store estimates, standard errors, and counts
results_data <- data.frame()

# Loop through each model result and extract estimates and standard errors
for (model_name in names(model_results)) {
  # Get the current model
  model <- model_results[[model_name]]
  
  # Extract estimates (coefficients) and standard errors
  estimates <- model$b  # Model estimates
  standard_errors <- model$se  # Standard errors
  
  # Count the number of observations used in the model
  count <- length(model$yi)  # Assuming yi contains the response variable
  
  # Create a data frame with estimates, standard errors, model info, and count
  temp_df <- data.frame(
    model_name = model_name,  # Add model name for each estimate
    estimates = estimates,  # Add estimates
    std_error = standard_errors,  # Add standard errors
    count = count  # Add the count of observations
  )
  
  # Bind to the master data frame
  results_data <- rbind(results_data, temp_df)
}

# Create a vector for custom x-axis labels with counts
custom_labels <- paste0(c(
  "100m", 
  "200m", 
  "300m", 
  "400m", 
  "500m", 
  "600m", 
  "700m", 
  "800m", 
  "900m", 
  "1000m"), 
  "\nN(", results_data$count, ")"
)

# Create the dot plot with error bars and counts at the bottom
ggplot(results_data, aes(x = model_name, y = estimates)) +
  geom_point() +  # Add dots for the estimates
  geom_errorbar(aes(ymin = estimates - std_error, ymax = estimates + std_error), width = 0.2) +  # Add error bars
  
  theme_minimal() +
  xlab("Distance to the Lakeshore") +
  ylab("Elasticity") +
  ggtitle("Variation of Elasticity with Proximity to Lakeshore - North East") +
  
  scale_x_discrete(labels = custom_labels) +  # Use custom labels with counts
  
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.02))+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),  # Adjust alignment
    axis.title.x = element_text(margin = margin(t = 10)),  # Increase space above x-axis
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )


##########################################################################################

## West

df <- read_csv("./metadata/Meta_data_at_50m_distcal_elast.csv") %>%
  rename(distance = x) %>%
  rename(elast = y_value) %>%
  filter(west == 1)

# Define the distances you want to iterate over
distances <- seq(100, 1000, by = 100)

for (dist in distances) {
  df_wf <- df %>%
    filter(distance == dist) %>%  # Filter for the current distance
    ungroup() %>%
    mutate(obsid = row_number()) %>%
    group_by(study_name, region) %>%
    mutate(cluster_id = cur_group_id()) %>%
    ungroup() %>%
    mutate(vi = 1 / sampsize) %>%
    mutate(vi_log = 1 / log(sampsize)) %>%
    group_by(cluster_id) %>%
    mutate(count = n()) %>%
    mutate(w_1 = (1 / count)) %>%
    mutate(log_ss = log(sampsize)) %>%
    mutate(log_ss_within_clus = sum(log_ss)) %>%
    mutate(w_2 = log(sampsize) / log_ss_within_clus) %>%
    mutate(ave_ss_wt_clus = mean(sampsize)) %>%
    ungroup() %>%
    mutate(log_ave_ss_wt_clus = log(ave_ss_wt_clus)) %>%
    mutate(w_3 = log_ave_ss_wt_clus / sum(log_ave_ss_wt_clus))
  
  # Check if df_wf has enough observations
  if (nrow(df_wf) <= 1) {
    message(paste("Not enough data for distance", dist))
    next  # Skip this iteration if not enough data
  }
  
  # Run the meta-analysis model with tryCatch to handle errors
  re_wf <- tryCatch({
    rma.mv(yi = elast, vi_log, random = ~ 1 | cluster_id / obsid, data = df_wf, 
           control = list(optimizer = "optim", maxiter = 5000))
  }, error = function(e) {
    message(paste("Error fitting model for distance", dist, ":", e$message))
    return(NULL)
  })
  
  # Skip to the next iteration if the model fitting fails
  if (is.null(re_wf)) next
  
  # Calculate studentized residuals
  rstud_wf <- rstudent.rma.mv(re_wf, cluster = df_wf$cluster_id)
  
  df_wf <- df_wf %>%
    mutate(obsid = row_number())
  
  rstud_wf <- rstud_wf$obs %>%
    as.data.frame()
  
  rstud_wf <- rownames_to_column(rstud_wf, "obsid")
  
  rstud_wf_out <- rstud_wf %>%
    filter(!between(resid, -2, 2))
  
  df_250 <- subset(df_wf, !(obsid %in% rstud_wf_out$obsid))
  
  # Adjust the file name format
  file_name <- ifelse(dist < 1000, sprintf("%04dm", dist), paste0(dist, "m"))
  
  # Save the filtered data frame to a CSV file
  write.csv(df_250, paste0("./metadata/NewmetadataWest/meta_data_distance_", file_name, ".csv"), row.names = FALSE)
}




# List all files in the metadata directory that match the pattern
files <- list.files("./metadata/NewmetadataWest", pattern = "meta_data_distance_\\d+m.csv", full.names = TRUE)

# Create an empty list to store the model results
model_results <- list()

# Loop through each file, run the model, and store the results
for (file in files) {
  # Read the CSV file
  df_250 <- read.csv(file)
  
  # Run the meta-analysis model
  re_wf <- rma.mv(yi = elast, vi, random = ~ 1 | cluster_id / obsid, data = df_250)
  
  # Store the results in the list with the file name as the key
  model_results[[basename(file)]] <- re_wf
}

# Optional: Save the model results to an RData file for future use
save(model_results, file = "./metadata/NewmetadataWest/model_results.RData")


# Create an empty data frame to store estimates, standard errors, and counts
results_data <- data.frame()

# Loop through each model result and extract estimates and standard errors
for (model_name in names(model_results)) {
  # Get the current model
  model <- model_results[[model_name]]
  
  # Extract estimates (coefficients) and standard errors
  estimates <- model$b  # Model estimates
  standard_errors <- model$se  # Standard errors
  
  # Count the number of observations used in the model
  count <- length(model$yi)  # Assuming yi contains the response variable
  
  # Create a data frame with estimates, standard errors, model info, and count
  temp_df <- data.frame(
    model_name = model_name,  # Add model name for each estimate
    estimates = estimates,  # Add estimates
    std_error = standard_errors,  # Add standard errors
    count = count  # Add the count of observations
  )
  
  # Bind to the master data frame
  results_data <- rbind(results_data, temp_df)
}

# Create a vector for custom x-axis labels with counts
custom_labels <- paste0(c(
  "100m", 
  "200m", 
  "300m", 
  "400m", 
  "500m", 
  "600m", 
  "700m", 
  "800m", 
  "900m", 
  "1000m"), 
  "\nN(", results_data$count, ")"
)

# Create the dot plot with error bars and counts at the bottom
ggplot(results_data, aes(x = model_name, y = estimates)) +
  geom_point() +  # Add dots for the estimates
  geom_errorbar(aes(ymin = estimates - std_error, ymax = estimates + std_error), width = 0.2) +  # Add error bars
  
  theme_minimal() +
  xlab("Distance to the Lakeshore") +
  ylab("Elasticity") +
  ggtitle("Variation of Elasticity with Proximity to Lakeshore - West") +
  
  scale_x_discrete(labels = custom_labels) +  # Use custom labels with counts
  
  scale_y_continuous(breaks = seq(-0.1, 0.3, by = 0.02))+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),  # Adjust alignment
    axis.title.x = element_text(margin = margin(t = 10)),  # Increase space above x-axis
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )


###########################################################################################

## South

df <- read_csv("./metadata/Meta_data_at_50m_distcal_elast.csv") %>%
  rename(distance = x) %>%
  rename(elast = y_value) %>%
  filter(south == 1)

# Define the distances you want to iterate over
distances <- seq(100, 1000, by = 100)

for (dist in distances) {
  df_wf <- df %>%
    filter(distance == dist) %>%  # Filter for the current distance
    ungroup() %>%
    mutate(obsid = row_number()) %>%
    group_by(study_name, region) %>%
    mutate(cluster_id = cur_group_id()) %>%
    ungroup() %>%
    mutate(vi = 1 / sampsize) %>%
    mutate(vi_log = 1 / log(sampsize)) %>%
    group_by(cluster_id) %>%
    mutate(count = n()) %>%
    mutate(w_1 = (1 / count)) %>%
    mutate(log_ss = log(sampsize)) %>%
    mutate(log_ss_within_clus = sum(log_ss)) %>%
    mutate(w_2 = log(sampsize) / log_ss_within_clus) %>%
    mutate(ave_ss_wt_clus = mean(sampsize)) %>%
    ungroup() %>%
    mutate(log_ave_ss_wt_clus = log(ave_ss_wt_clus)) %>%
    mutate(w_3 = log_ave_ss_wt_clus / sum(log_ave_ss_wt_clus))
  
  # Check if df_wf has enough observations
  if (nrow(df_wf) <= 1) {
    message(paste("Not enough data for distance", dist))
    next  # Skip this iteration if not enough data
  }
  
  # Run the meta-analysis model with tryCatch to handle errors
  re_wf <- tryCatch({
    rma.mv(yi = elast, vi_log, random = ~ 1 | cluster_id / obsid, data = df_wf, 
           control = list(optimizer = "optim", maxiter = 5000))
  }, error = function(e) {
    message(paste("Error fitting model for distance", dist, ":", e$message))
    return(NULL)
  })
  
  # Skip to the next iteration if the model fitting fails
  if (is.null(re_wf)) next
  
  # Calculate studentized residuals
  rstud_wf <- rstudent.rma.mv(re_wf, cluster = df_wf$cluster_id)
  
  df_wf <- df_wf %>%
    mutate(obsid = row_number())
  
  rstud_wf <- rstud_wf$obs %>%
    as.data.frame()
  
  rstud_wf <- rownames_to_column(rstud_wf, "obsid")
  
  rstud_wf_out <- rstud_wf %>%
    filter(!between(resid, -2, 2))
  
  df_250 <- subset(df_wf, !(obsid %in% rstud_wf_out$obsid))
  
  # Adjust the file name format
  file_name <- ifelse(dist < 1000, sprintf("%04dm", dist), paste0(dist, "m"))
  
  # Save the filtered data frame to a CSV file
  write.csv(df_250, paste0("./metadata/NewmetadataSouth/meta_data_distance_", file_name, ".csv"), row.names = FALSE)
}


# List all files in the metadata directory that match the pattern
files <- list.files("./metadata/NewmetadataSouth", pattern = "meta_data_distance_\\d+m.csv", full.names = TRUE)

# Create an empty list to store the model results
model_results <- list()

# Loop through each file, run the model, and store the results
for (file in files) {
  # Read the CSV file
  df_250 <- read.csv(file)
  
  # Run the meta-analysis model
  re_wf <- rma.mv(yi = elast, vi, random = ~ 1 | cluster_id / obsid, data = df_250)
  
  # Store the results in the list with the file name as the key
  model_results[[basename(file)]] <- re_wf
}

# Optional: Save the model results to an RData file for future use
save(model_results, file = "./metadata/NewmetadataSouth/model_results.RData")


# Create an empty data frame to store estimates, standard errors, and counts
results_data <- data.frame()

# Loop through each model result and extract estimates and standard errors
for (model_name in names(model_results)) {
  # Get the current model
  model <- model_results[[model_name]]
  
  # Extract estimates (coefficients) and standard errors
  estimates <- model$b  # Model estimates
  standard_errors <- model$se  # Standard errors
  
  # Count the number of observations used in the model
  count <- length(model$yi)  # Assuming yi contains the response variable
  
  # Create a data frame with estimates, standard errors, model info, and count
  temp_df <- data.frame(
    model_name = model_name,  # Add model name for each estimate
    estimates = estimates,  # Add estimates
    std_error = standard_errors,  # Add standard errors
    count = count  # Add the count of observations
  )
  
  # Bind to the master data frame
  results_data <- rbind(results_data, temp_df)
}

# Create a vector for custom x-axis labels with counts
custom_labels <- paste0(c(
  "100m", 
  "200m", 
  "300m", 
  "400m", 
  "500m", 
  "600m", 
  "700m", 
  "800m", 
  "900m", 
  "1000m"), 
  "\nN(", results_data$count, ")"
)

# Create the dot plot with error bars and counts at the bottom
ggplot(results_data, aes(x = model_name, y = estimates)) +
  geom_point() +  # Add dots for the estimates
  geom_errorbar(aes(ymin = estimates - std_error, ymax = estimates + std_error), width = 0.2) +  # Add error bars
  
  theme_minimal() +
  xlab("Distance to the Lakeshore") +
  ylab("Elasticity") +
  ggtitle("Variation of Elasticity with Proximity to Lakeshore - South") +
  
  scale_x_discrete(labels = custom_labels) +  # Use custom labels with counts
  
  scale_y_continuous(breaks = seq(-0.1, 0.3, by = 0.02))+
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),  # Adjust alignment
    axis.title.x = element_text(margin = margin(t = 10)),  # Increase space above x-axis
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )

#######################################################################################

## canada

df <- read_csv("./metadata/Meta_data_at_50m_distcal_elast.csv") %>%
  rename(distance = x) %>%
  rename(elast = y_value) %>%
  filter(canada == 1)

# Define the distances you want to iterate over
distances <- seq(100, 1000, by = 100)

for (dist in distances) {
  df_wf <- df %>%
    filter(distance == dist) %>%  # Filter for the current distance
    ungroup() %>%
    mutate(obsid = row_number()) %>%
    group_by(study_name, region) %>%
    mutate(cluster_id = cur_group_id()) %>%
    ungroup() %>%
    mutate(vi = 1 / sampsize) %>%
    mutate(vi_log = 1 / log(sampsize)) %>%
    group_by(cluster_id) %>%
    mutate(count = n()) %>%
    mutate(w_1 = (1 / count)) %>%
    mutate(log_ss = log(sampsize)) %>%
    mutate(log_ss_within_clus = sum(log_ss)) %>%
    mutate(w_2 = log(sampsize) / log_ss_within_clus) %>%
    mutate(ave_ss_wt_clus = mean(sampsize)) %>%
    ungroup() %>%
    mutate(log_ave_ss_wt_clus = log(ave_ss_wt_clus)) %>%
    mutate(w_3 = log_ave_ss_wt_clus / sum(log_ave_ss_wt_clus))
  
  # Check if df_wf has enough observations
  if (nrow(df_wf) <= 1) {
    message(paste("Not enough data for distance", dist))
    next  # Skip this iteration if not enough data
  }
  
  # Run the meta-analysis model with tryCatch to handle errors
  re_wf <- tryCatch({
    rma.mv(yi = elast, vi_log, random = ~ 1 | cluster_id / obsid, data = df_wf, 
           control = list(optimizer = "optim", maxiter = 5000))
  }, error = function(e) {
    message(paste("Error fitting model for distance", dist, ":", e$message))
    return(NULL)
  })
  
  # Skip to the next iteration if the model fitting fails
  if (is.null(re_wf)) next
  
  # Calculate studentized residuals
  rstud_wf <- rstudent.rma.mv(re_wf, cluster = df_wf$cluster_id)
  
  df_wf <- df_wf %>%
    mutate(obsid = row_number())
  
  rstud_wf <- rstud_wf$obs %>%
    as.data.frame()
  
  rstud_wf <- rownames_to_column(rstud_wf, "obsid")
  
  rstud_wf_out <- rstud_wf %>%
    filter(!between(resid, -2, 2))
  
  df_250 <- subset(df_wf, !(obsid %in% rstud_wf_out$obsid))
  
  # Adjust the file name format
  file_name <- ifelse(dist < 1000, sprintf("%04dm", dist), paste0(dist, "m"))
  
  # Save the filtered data frame to a CSV file
  write.csv(df_250, paste0("./metadata/NewmetadataCanada/meta_data_distance_", file_name, ".csv"), row.names = FALSE)
}


# List all files in the metadata directory that match the pattern
files <- list.files("./metadata/NewmetadataCanada", pattern = "meta_data_distance_\\d+m.csv", full.names = TRUE)

# Create an empty list to store the model results
model_results <- list()

# Loop through each file, run the model, and store the results
for (file in files) {
  # Read the CSV file
  df_250 <- read.csv(file)
  
  # Run the meta-analysis model
  re_wf <- rma.mv(yi = elast, vi, random = ~ 1 | cluster_id / obsid, data = df_250)
  
  # Store the results in the list with the file name as the key
  model_results[[basename(file)]] <- re_wf
}

# Optional: Save the model results to an RData file for future use
save(model_results, file = "./metadata/NewmetadataCanada/model_results.RData")


# Print a summary of one model result as an example
summary(model_results[[5]])  # Adjust this based on your preference



# Create an empty data frame to store estimates, standard errors, and counts
results_data <- data.frame()

# Loop through each model result and extract estimates and standard errors
for (model_name in names(model_results)) {
  # Get the current model
  model <- model_results[[model_name]]
  
  # Extract estimates (coefficients) and standard errors
  estimates <- model$b  # Model estimates
  standard_errors <- model$se  # Standard errors
  
  # Count the number of observations used in the model
  count <- length(model$yi)  # Assuming yi contains the response variable
  
  # Create a data frame with estimates, standard errors, model info, and count
  temp_df <- data.frame(
    model_name = model_name,  # Add model name for each estimate
    estimates = estimates,  # Add estimates
    std_error = standard_errors,  # Add standard errors
    count = count  # Add the count of observations
  )
  
  # Bind to the master data frame
  results_data <- rbind(results_data, temp_df)
}

# Create a vector for custom x-axis labels with counts
custom_labels <- paste0(c(
  "100m", 
  "200m", 
  "300m", 
  "400m", 
  "500m", 
  "600m", 
  "700m", 
  "800m", 
  "900m", 
  "1000m"), 
  "\nN(", results_data$count, ")"
)

# Create the dot plot with error bars and counts at the bottom
ggplot(results_data, aes(x = model_name, y = estimates)) +
  geom_point() +  # Add dots for the estimates
  geom_errorbar(aes(ymin = estimates - std_error, ymax = estimates + std_error), width = 0.2) +  # Add error bars
  
  theme_minimal() +
  xlab("Distance to the Lakeshore") +
  ylab("Elasticity") +
  ggtitle("Variation of Elasticity with Proximity to Lakeshore in Canada") +
  
  scale_x_discrete(labels = custom_labels) +  # Use custom labels with counts
  
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.02)) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),  # Adjust alignment
    axis.title.x = element_text(margin = margin(t = 10)),  # Increase space above x-axis
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank()   # Remove minor grid lines
  )




########################################################################################\

##### Regional






##########################RSTUD#####################################



library(dplyr)
library(ggplot2)
library(metafor)

# List of model results (from previous steps)
# load("./metadata/model_results.RData")

# Create an empty data frame to store residuals and model info
residuals_data <- data.frame()

# Loop through each model result and extract residuals
for (model_name in names(model_results)) {
  # Get the current model
  model <- model_results[[model_name]]
  
  # Extract residuals
  rstud <- rstudent.rma.mv(model)$resid
  
  # Create a data frame with residuals and model info
  temp_df <- data.frame(
    model_name = model_name,  # Add model name for each residual
    residuals = rstud
  )
  
  # Bind to the master data frame
  residuals_data <- rbind(residuals_data, temp_df)
}

# Create the box plot
ggplot(residuals_data, aes(x = model_name, y = residuals)) +
  geom_boxplot() +
  theme_minimal() +
  xlab("Model") +
  ylab("Residuals") +
  ggtitle("Residuals Box Plot for Each Meta-Analysis Model") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


