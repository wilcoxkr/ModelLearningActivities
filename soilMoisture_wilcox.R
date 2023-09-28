### Soil moisture submodel
###
### Author: Kevin Wilcox (k_wilcox@uncg.edu)
### Date created: September 28, 2023; last updated: Sept 28 2023

### Set up workspace
library(tidyverse)
setwd("C:\\Users\\K_WILCOX\\OneDrive - UNCG\\PredictiveEcolLab\\Learning how to model\\")

hello!

###
### Simulate forcing data -- precipitation by hour
###
{
num_of_days_to_sim <- 10
ppt_hrly <- data.frame(
  hour = 1:(24*num_of_days_to_sim),
  ppt_mm = rnorm(num_of_days_to_sim, mean=10, sd=5)
) 

ppt_hrly$ppt_mm[ppt_hrly$ppt_mm<0]=0 # replace negative values with 0

# include a defined number of no-rain-hours -- this will allow for more realistic dry down time periods
num_of_dry_hrs <- 200
hrs_to_make_dry <- sample(ppt_hrly$hour, num_of_dry_hrs)

ppt_hrly <- ppt_hrly %>%
  mutate(ppt_mm = replace(ppt_mm, hour %in% hrs_to_make_dry, 0))

## Look at the precipitation
ggplot(ppt_hrly, aes(x=hour, y=ppt_mm)) + geom_col() + theme_bw()

} # End of simulating forcing data

###
### Set parameters and state variable objects
###
{
s_thk <- 100 # thickness of soil layers -- currently only a single layer, units in cm
field_cap <- 0.4 # field capacity of water content
water_def <- 0 # starting point for water deficit

  
soil_moist <- data.frame(hour=0:(24*num_of_days_to_sim),
                      smoist=0,
                      runoff=0,
                      perc=0)


} # End of set parameters

###
### 
###
{
  
  for(HOUR in 1:nrow(ppt_hrly)){
    water_in <- ppt_hrly$ppt_mm[HOUR]
    prev_smoist <- soil_moist %>% filter(hour==HOUR-1) %>% pull(smoist)
    water_def <- ifelse(water_def<=field_cap,
                        field_cap-water_def,
                        0)
      
    
    # Add water to soil layer
    if(water_in > 0){
      water_to_add <- min(water_in, water_def*s_thk*10) # second element of min statement is the amount of rain the layer can accept
      smoist_current <- (prev_smoist*(s_thk*10)+water_to_add)/(s_thk*10)
      water_in <- water_in-water_to_add
      perc_current <- water_in ## Assumes all leftover water percolated deeper in soil -- should include a runoff estimator
    
      # Update soil_moist data frame
      soil_moist$smoist[HOUR] <- smoist_current
      soil_moist$perc[HOUR] <- perc_current
      }
    
    
    ## No action if no precipitation occurred
    # produce runoff during infoltration ???
    
  }# End of HOUR loop

  
  
  
} # End of model section

# NOTE NEED TO ADD SMOIST TO PREVIOUS SMOIST AND INCLUDE EVAPORATION
ggplot(soil_moist, aes(x=hour, y=smoist)) + geom_path()
