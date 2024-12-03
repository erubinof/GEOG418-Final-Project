GEOG 418 Final Project
# An Analysis on Winter Temperature's Impact on Forest Pest Infestation in BC: R Coding Tutorial
Created by: Ezra Rubinoff
## Introduction
From 2010-2020 in British Columbia, forest pests impacted 6,793,100 hectares a year on average (National Forestry Database). That is equal to about 11% of all forest land in BC! In the context of climate change, trees are a key factor in balancing the carbon in our atmosphere through carbon sequestration. As they grow, they absorb CO2, a greenhouse gas that is stored in the biomass of the tree, and only release it when the tree burns or decays. Insect damage directly leads to the decay of trees, and indirectly to increased CO2 in the atmosphere and therefore climate change (Forest Carbon, 2013). Exploring how temperature can impact these pests is important as we look to slow carbon emissions and manage our climate. Recent studies have shown that severe cold events can reduce the size of forest pest outbreaks, limiting their ability to cause tree decay (MacQuarrie et al., 2024). They also explain that the intensity of winter weather conditions can limit both the range and impact of those pests, preventing them from distributing and impacting more forests(MacQuarrie et al., 2024). In a study focused on BC's forest pests, researchers explain that increased winter temperatures is a climate change prediction with a high degree of confidence (Woods, 2011). The concern these researchers show is due to the Mountain Pine Beetles' (MPB) vulnerability to cold winter temperatures, so with the projected increases, the MPB epidemic that is occuring could get worse (Woods, 2011). This not only shows that there is a strong connection between forest pests and winter temperature, but also that a dangerous feedback loop could emerge from more tree decay causing increased CO2 in the atmosphere and therefore higher winter temperatures and more pest disturbance.

This tutorial aims to show how analysis can be done using R to analyse if winter temperatures explain higher incidences of forest pest disturbance in the following summer in British Columbia. At the end, we will be able to answer these three questions:
- What is the spatial distribution pattern of forest pest distrurbances in BC?
- What is the best method of interpolating average winter temperature data across BC?
-	Do lower winter temperatures lead to lower incidences of forest pest infestation events in the following summer?

## Setting Up the Workspace
For this tutorial, we will be using R to conduct our analysis. While the base installation of R provides many useful functions, more specialized calculations and visualizations often require additional packages and libraries. These packages can be installed and loaded through RStudio, expanding the functionality of R and enabling more complex workflows.

In this case, we are interested in capabilities such as creating thematic maps, performing spatial analysis, managing spatial data, handling dates and times, performing statistical modeling, and generating high-quality visualizations. These tasks require specific packages to be installed. To do so, we use the install.packages() function to add them to our system. Once installed, we can load them into our project with the library() function. With these packages in place, the functions and tools they provide will be ready for use in our analysis.
```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Install necessary libraries
install.packages("tmap")
install.packages("spdep")
install.packages("raster")
install.packages("sf")
install.packages("lubridate")
install.packages("dplyr")
install.packages("gstat")
install.packages("ggplot2")
install.packages("maps")
install.packages("viridis")
install.packages("spgwr")
install.packages("e1071")
install.packages("gridExtra")
install.packages("gtable")
install.packages("spatstat")
install.packages("knitr")
install.packages("shinyjs")

# Load necessary libraries
library(tmap)
library(spdep)
library(raster)
library(sf)
library(lubridate)
library(dplyr)
library(gstat)
library(ggplot2)
library(maps)
library(viridis)
library(spgwr)
library(e1071)
library(gridExtra)
library(gtable)
library(spatstat)
library(knitr)
library(shinyjs)
```
## Cleaning the Climate Data
Once you have downloaded the data from this github page and placed it into the folder you will be working in, the data is ready to be cleaned and prepared for analysis.

The first step is to set your working directory to inform the code where to look for the data and where to save your data and figures to. This working directory needs to be set for the project once, and everything from now on will be pulled from or saved into that folder.

Once that is complete, we can begin to bring in the data and save it in a format that will be usable. As this climate data was downloaded as individual station csv files from PCIC, we have to instruct the code to go into each one and extract the important temperature information. To get a more spatially diverse sampling of data, we had to collect it from multiple agencies with different ways of formatting the data. This means that in the cleaning process, we have to make sure all the different formats are accepted, and that all the outliers are taken out. To do this, the code only looks for the correct column for air temperature as defined by the subdirectory that the csv file is in. It also filters out any temperatures recorded between -50C and 60C, as that is the range of natural temperatures on the surface of the earth. Lastly, the code averages the data based on the date to make the final output an average temperature for November 2021 to March 2022 for each of the climate stations. The result of this code is a CSV that contains a value for temperature for each of the stations.
```{r CleanData, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Set working directory
dir <- "C:/Users/Ezra Rubinoff/Desktop/UVIC/Term 12-Fall 2024/GEOG 418/Final Project"
setwd(dir)

# Create an empty data frame with specified columns
empty_data <- data.frame(Native.ID = character(), TEMP = numeric(), 
                         Longitude = numeric(), Latitude = numeric(), stringsAsFactors = FALSE)

# Define the CSV file path for output
csv_file_name <- "./Data/BC_AVG_TEMP.csv"

# Write the empty data frame to a CSV file if it doesn't exist
if (!file.exists(csv_file_name)) {
  write.csv(empty_data, file = csv_file_name, row.names = FALSE)
}

# Base directory for the data
base_dir <- "C:/Users/Ezra Rubinoff/Desktop/UVIC/Term 12-Fall 2024/GEOG 418/Final Project/Data/BC_Temp_Data_2021to2023/pcds_data_ONLYMEANTEMP"

# Subdirectories to process
subdirectories <- c("BCH", "EC", "ENV-ASP")

# Loop through each subdirectory
for (subdir in subdirectories) {
  
  # Construct the full path to the subdirectory
  full_path <- file.path(base_dir, subdir)
  
  # List all CSV files in the specified subdirectory
  csv_files <- list.files(path = full_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Loop through each CSV file in the current subdirectory
  for (file in csv_files) {
    cat("Processing file:", file, "\n")  # Print the file being processed
    
    # Read each file, adjusting the format as necessary
    hourly_data <- read.csv(file, skip = 1, header = TRUE)
    
    # Check if the time column exists
    if (!"time" %in% colnames(hourly_data)) {
      cat("No 'time' column found in", file, "\n")
      next  # Skip this file if 'time' column is not found
    }
    
    # Convert time column to DateTime format
    hourly_data$time <- lubridate::ymd_hms(hourly_data$time)
    
    # Filter data to include records between November 2021 and March 2022
    hourly_data <- hourly_data %>%
      filter(time >= as.Date("2021-11-01") & time <= as.Date("2022-03-31"))
    
    # Determine which column to use for temperature based on the subdirectory
    if (subdir == "BCH") {
      temp_col <- "AirTemp"
    } else if (subdir == "EC") {
      temp_col <- "MAX_TEMP"
    } else if (subdir == "ENV-ASP") {
      temp_col <- "air_temp_1"
    } else {
      cat("Unknown subdirectory:", subdir, "\n")
      next
    }
    
    # Check if the temperature column exists and is numeric
    if (!temp_col %in% colnames(hourly_data)) {
      cat("No temperature column ('", temp_col, "') found in", file, "\n", sep = "")
      next
    }
    
    # Attempt to convert the temperature column to numeric
    hourly_data$AirTemp <- as.numeric(hourly_data[[temp_col]])
    
    # Check for conversion issues
    if (all(is.na(hourly_data$AirTemp))) {
      cat("All values in temperature column are NA for", file, "\n")
      next  # Skip this file if all values are NA
    }
    
    # Filter out NA values and out-of-range temperatures in the AirTemp column
    hourly_data <- hourly_data %>%
      filter(!is.na(AirTemp) & AirTemp >= -50 & AirTemp <= 60)
    
    # Calculate average temperature across the specified months for each station
    avg_temp_nov_mar <- hourly_data %>%
      summarize(avg_temp = mean(AirTemp, na.rm = TRUE), .groups = "drop")
    
    # Assign the filename to an object
    file_name_no_ext <- tools::file_path_sans_ext(basename(file))
    
    # Read the existing CSV file to append results
    data <- read.csv(csv_file_name)
    
    # Check if any valid temperature data exists for the period
    if (nrow(avg_temp_nov_mar) > 0 && !is.na(avg_temp_nov_mar$avg_temp)) {
      Roundedtemp <- round(avg_temp_nov_mar$avg_temp, 2)
    } else {
      cat("No valid temperature data for", file_name_no_ext, "\n")
      next  # Skip this file if no valid temperature found
    }
    
    # Ensure that Native.ID is character type
    data$Native.ID <- as.character(data$Native.ID)
    
    # Create a new data frame for the current file's results
    new_values <- data.frame(Native.ID = file_name_no_ext, 
                             TEMP = Roundedtemp, 
                             Longitude = NA,  # Set Longitude to NA for now
                             Latitude = NA,   # Set Latitude to NA for now
                             stringsAsFactors = FALSE)
    
    # Append new values to the existing data
    data <- bind_rows(data, new_values)
    
    # Save the updated data frame back to the CSV file
    write.csv(data, file = csv_file_name, row.names = FALSE)
  }
}
```
## Merging the Climate Data
Now that the climate data has been cleaned, we must turn the CSV with a temperature value and name of a station into spatial data. We can do this using the metadata we downloaded alongside the climate data. This metadata contains the location information for each of the stations with their IDs, which will allow us to link these two datasets together. This section of code outputs a csv with the temperature data and the latitude and longitude coordinates of each of the stations. 
```{r MergeClimateData, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
#Merge the climate data for each station with the location data found in the metadata file
metadata <- read.csv("./Data/BC_Temp_Data_2021to2023/station-metadata-by-history.csv")
climatedata <- read.csv("./Data/BC_AVG_TEMP.csv")

merged_data <- merge(metadata, climatedata, by = "Native.ID")

#Remove the last two columns which are duplicate Latitude and Longitude
merged_data <- merged_data[, -((ncol(merged_data)-1):ncol(merged_data))]

#Change column names for Latitude and Longitude to remove the x
colnames(merged_data)[colnames(merged_data) %in% c("Latitude.x", "Longitude.x")] <- c("Longitude", "Latitude")

#Omit NA's
merged_data <- na.omit(merged_data)

#There are erroneous temperature values. Filter data to remove these
merged_data <- merged_data[merged_data$TEMP <= 100, ]

#Write the dataset so that it  is stored
write.csv(merged_data, file = "./Data/ClimateData.csv", row.names = FALSE)
```
## Mapping the Climate Data
This next step is visualizing the data so we can interpret it. To do this, we will first create a shapefile of the data so it can be used later. Then, using the ggplot library, we will make a map with a descriptive title and informative legend.
```{r MapClimateData, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Ensure Latitude and Longitude columns are correctly formatted
# Assuming the columns are named "Latitude" and "Longitude"
climate_data <- read.csv("./Data/ClimateData.csv")
climate_data <- climate_data %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

# Create a simple feature object (sf) using Latitude and Longitude
climate_sf <- st_as_sf(climate_data, coords = c("Longitude", "Latitude"), crs = 4326)

# Optionally, you can select columns that you want to keep in the shapefile
# climate_sf <- climate_sf %>% select(Your_Columns_Here)

climate_sf <- st_transform(climate_sf, crs=3005)

# Write the shapefile to disk
st_write(climate_sf, "./Output/ClimateData.shp")

# Confirmation message
print("Shapefile has been created: ClimateData.shp")

# Load the shapefiles
climate_sf <- st_read("./Output/ClimateData.shp")
bc_boundary <- st_read("./Data/BC_Boundary_SHP.shp")
bc_boundary <- st_transform(bc_boundary, crs=3005)

# Create the map
ggplot() +
  geom_sf(data = bc_boundary, fill = "lightgrey", color = "black") +
  # Map the TEMP variable to color
  geom_sf(data = climate_sf, aes(color = TEMP), size = 2) + 
  scale_color_gradient(low = "skyblue", high = "darkred") + # Adjust color gradient as needed
  theme_minimal() +
  labs(title = "Map of Average Temperature in British Columbia for \nNovember 2021 - March 2022",
       subtitle = "Overlayed on BC Boundary",
       x = "Longitude",  # Use Longitude for x-axis
       y = "Latitude",   # Use Latitude for y-axis
       color = "Temperature (°C)") + # Label for color legend
  theme(legend.position = "bottom")
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/5f759e01-34bc-43a7-8501-4af7f67db67f" alt="Climate Map" width="1500" />
</div>
<p style="text-align: center;"><em>Figure 1: Map of 2021-2022 average winter temperature in BC for each of the climate stations used</em></p>

You can see a pattern start to emerge here as the coastal and southern areas appear to have warmer winters that the northern areas. It is also important to note that the distribution of climate stations are not even and some areas are more sampled then others. This means some of the less accessible areas will not have values. 
## Climate Data Interpolation
As we want to use this climate data to analyze our patterns of forest pest disturbance, points could cause a problem because not all of the pest disturbance events will land on an area with a climate station. For that reason, we will need to interpolate a surface to try and estimate the average temperature for all of BC. Interpolation is the process of turning a point dataset into a raster dataset, where each pixel of the study area has a value associated with it. These values are determined based on the method of interpolation. There are two methods that we will use for this, the first is Inverse Distance Weighting (IDW), and the second is Kriging. Both have their benefits and drawbacks so we will go through them to determine which will be best for this analysis.

### Inverse Distance Weighting (IDW)
The first interpolation method we will try is Inverse Distance Weighting or IDW. IDW is a spatial interpolation technique that estimates values at unsampled locations based on a weighted average of nearby known values. The main idea of IDW is that the closer a sample point is to the prediction location, the more influence it has on the predicted value. The power value in IDW is an important factor that influences the smoothness of the interpolated surface. It determines the rate at which the weight of a sample point decreases when increasing distance from the prediction location. Lower power values, like 1 or 2, result in smoother interpolated surfaces, as the influence of further points is higher. This can be useful when the underlying spatial process is believed to be smooth and continuous. Higher power values, like 3 or 4, lead to more localized and detailed interpolated surfaces, as the influence of distant points is decreased. This can be appropriate when the spatial process is contains rapid variations or local trends.

For this application, there are 2 parameters to enter. The first is the grid size for the output. A higher grid size would lead to a smoother surface and less computational power required, but it could miss important features that a finer grid could catch. In this case, we selected a cellsize of 25,000 to try and find a balance between both. The other parameter is the power value (idp), which we have selected 2 as the temperature should be smooth not have too many local trends acorss BC. Once this has all been set, we can run the interpolation and output a map of the result. To make the map more readable, we also have clipped it to the boundary of BC.
```{r IDWInterpolation, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Read the shapefile
climate_shp <- st_read("./Output/ClimateData.shp")

# Check the structure of the data to ensure it contains the TEMP variable
print(head(climate_shp))

# Create a grid for the interpolation
# Adjust the extent and resolution of the grid according to your needs
bbox <- st_bbox(climate_shp)
grid <- st_make_grid(st_as_sfc(bbox), cellsize = c(25000, 25000))  # Adjust the cell size

# Interpolate using IDW
idw_result <- gstat::idw(TEMP ~ 1, 
                         locations = climate_shp, 
                         newdata = st_as_sf(grid), 
                         idp = 2)

# Convert idw_result to an sf object
idw_sf <- st_as_sf(idw_result)

# Extract coordinates 
idw_sf <- st_as_sf(idw_result)


# Plot the results using geom_sf() for better handling of sf objects
ggplot(data = idw_sf) +
  geom_sf(aes(fill = var1.pred), color = NA) +  # Fill based on predicted values
  scale_fill_viridis_c() +
  labs(title = "IDW Interpolation of Temperature", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right")

# Save the result to a shapefile if needed
st_write(idw_sf, "./Output/IDW_Result.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)

# Verify the structure of the polygon shapefile
print(head(bc_boundary))
# Check the CRS of both objects
crs_idw <- st_crs(idw_sf)  # CRS of IDW result
crs_polygon <- st_crs(bc_boundary)  # CRS of the polygon shapefile

print(crs_idw)
print(crs_polygon)

# Step to transform the CRS of either shapefile if they do not match
if (crs_idw != crs_polygon) {
  # Transform the IDW result to match the CRS of the polygon
  idw_sf <- st_transform(idw_sf, crs = crs_polygon)  # Transform IDW result to polygon's CRS
  message("Transformed IDW result CRS to match the polygon.")
} else {
  message("CRS of IDW result and polygon already match.")
}

# Now attempt the intersection again
idw_clipped <- st_intersection(idw_sf, bc_boundary)

# Check the results of clipping
print(st_geometry(idw_clipped))  # Check geometry to ensure it's clipped correctly


# Step 3: Create the map of the clipped results
ggplot(data = idw_clipped) +
  geom_sf(aes(fill = var1.pred), color = NA) +  # Fill based on predicted temperature values
  scale_fill_viridis_c(option = "D") +  # Use viridis color scale for better readability
  labs(title = "IDW Interpolation of Temperature Across British Columbia \nfrom November 2021 to March 2022",
       fill = "Temperature (°C)",  # Change label as appropriate
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right")

# Step 4: Save the map as an image file (optional)
ggsave("./Output/Clipped_IDW_Interpolation_Map.png", width = 10, height = 8, dpi = 300)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/00e3617e-fdb5-4916-8936-5f8b2b4bc4ec" alt="IDW Map" width="500" />
</div>
<p style="text-align: center;"><em>Figure 2: Map of IDW interpolation of the average winter temperature in BC from 2021-2022</em></p>

This shows one method of interpolation that creates a surface with each pixel representing an average winter temperature from November 2021-March 2022. This somewhat follows the pattern that we could see when we just mapped the climate station points.
### Kriging
The second interpolation method we will use is Kriging. Kriging is a spatial interpolation technique that predicts values at unsampled locations based on the spatial autocorrelation of the known data points. Unlike IDW, which relies on distance-based weights, Kriging incorporates a statistical model of the spatial relationships between points, represented by a semivariogram. The semivariogram is a key component in Kriging, as it describes how the variance between values changes with distance, allowing the interpolation to account for both the strength and range of spatial dependency. This makes Kriging strong for capturing complex spatial patterns. Kriging is often considered a global interpolation method because it uses data from the entire dataset, rather than focusing only on nearby points, to predict values. This global approach can provide more accurate results when there are clear spatial trends or when the data exhibits consistent patterns over larger areas.

We will now walk through a process to prepare for the Kriging interpolation using our climate data. First we define the model formula as TEMP ~ 1, which tells the Kriging algorithm to predict the temperature across the study area without including any additional predictors. Then, we create the semivariogram using our climate data. This step involves analyzing how the temperature values vary with distance between data points. We can fit a spherical semivariogram model ("Sph") with the following parameters: a nugget of 8, a sill of 40 , and a range of 600,000. After fitting the model, we’ll visualize it with a plot to confirm the fit.

```{r KrigingSemivariance, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
f.0 <- as.formula(TEMP ~ 1) 

# Create variogram. Be sure to test out the three different models.
var.smpl <- variogram(f.0, climate_shp, cloud = FALSE) 
dat.fit  <- fit.variogram(var.smpl, fit.ranges = TRUE, fit.sills = TRUE,
                          vgm(model="Sph", nugget = 8, psill = 40, 
                              range = 600000))
plot(var.smpl, dat.fit)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/35291b63-e528-4c8f-b1ac-710a3a425a00" alt="Semivariogram" width="700" />
</div>
<p style="text-align: center;"><em>Figure 3: Plot of semivariogram</em></p>

As the plot shows, the line closely follows the points and therefore is a good fit. We can now continue with our interpolation.

The next step is to define the grid, which we have already determined to be a 25,000 square metre cell size to match our IDW resolution. Lastly, we can apply the Kriging method using the semivariogram model, the formula, the climate data, and the prediction grid. This step calculates predicted temperature values at each grid point based on the spatial relationships derived from the semivariogram. The result is a detailed spatial map of predicted temperatures across the study area, which we then clip to the BC boundary and output as a formatted map.
```{r KrigingInterpolation, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Define the grid
xmin <- st_bbox(bc_boundary)$xmin
xmax <- st_bbox(bc_boundary)$xmax
ymin <- st_bbox(bc_boundary)$ymin
ymax <- st_bbox(bc_boundary)$ymax

# Create a regular grid
n <- 25000  # Number of points
grd <- st_as_sf(expand.grid(x = seq(xmin, xmax, length.out = sqrt(n)),
                            y = seq(ymin, ymax, length.out = sqrt(n))),
                coords = c("x", "y"), crs = st_crs(climate_shp))

dat.krg <- krige(f.0, climate_shp, grd, dat.fit, debug.level=0)

# Convert the kriging output to an sf object
kriging_results_sf <- st_as_sf(dat.krg)

# Create a Raster from Kriging Results
# 1. Convert to a data frame with coordinates for raster creation
coords_df <- as.data.frame(st_coordinates(kriging_results_sf))
coords_df$predicted_temp <- kriging_results_sf$var1.pred  # Replace with your prediction column

# 2. Create the raster from the resulting data frame
predicted_raster <- rasterFromXYZ(coords_df)

# 3. Clip the raster using the polygon (bc_boundary)
# First, align the CRS (coordinate reference system) of both raster and polygon
crs(predicted_raster) <- crs(bc_boundary)
predicted_raster_cropped <- crop(predicted_raster, extent(bc_boundary))
kriging_clipped <- mask(predicted_raster_cropped, bc_boundary)

# 4.Visualize the raster
kriging_map <- tm_shape(kriging_clipped) +
  tm_raster(palette = "viridis", 
            title = "Predicted Temperature") +
  tm_layout(
    title = "Kriging Results for Temperature Across BC \nfrom November 2021 to March 2022",
    title.position = c("center", "top"), # Centers title at the top outside the main map
    legend.position = c("left", "bottom"), # Moves the legend to the bottom left
    inner.margins = c(0.1, 0.1, 0.1, 0.1) # Adjusts margins to create extra space around elements
  ) +
  tm_compass(
    position = c("right", "bottom") # Moves the compass to the bottom right
  ) +
  tm_scale_bar(
    position = c("left", "bottom") # Places the scale bar at the bottom left
  )

kriging_map

# Save the map
tmap_save(kriging_map, filename = "./Output/Kriging_map.png", width = 10, height = 8, dpi = 300)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/0c7bc2a8-8dcc-4c4c-96f6-923cdbf928cf" alt="Kriging Map" width="700" />
</div>
<p style="text-align: center;"><em>Figure 4: Map of interpolated climate data across BC for November 2021-March 2022 using Kriging</em></p>

Now we have interpolated with both IDW and Kriging, we can see some of of their benefits and drawbacks. For this data, the best option to continue on with is IDW, as it seems to handle the unevenly distributed data much better than Kriging. In the Kriging map above, it is clear that the areas with more samples sees the most differences and IDW is able to capture some of the more localized trends in the interior of the province.
# Forest Pest Disturbance Descriptive Statistics
Moving along from the climate data, we can begin to prepare and understand our forest pest disturbance event data that we will need to conduct our analysis. This data was downloaded from the BC Data Catalogue. Each point contains the year of survey, forest health factor (disturbance agent), severity class, area, numbers of trees estimated in the spot, and host (BC Data Catalogue, 2023). For this project, we will filter the data to only use points that mark the year as 2022.

The first step we must take to understand the dataset is calculate some descriptive statistics. For this data, we will use the number of trees that each point represents as our value. This will give us good insight into the types of events that we are dealing with and what the damage looks like.
```{r PestDescriptiveStats, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Load your point data and filter it to 2022
Pest_Infest_point <- st_read("./Data/BC_Pest_Data/BCGW_7113060B_1729806658214_18952/PEST_INFESTATION_POINT/PST_IF_PT_point.shp")
Pest_Infest_2022 <- subset(Pest_Infest_point, CPTR_YR == 2022)

#Descriptive Statistics and Point Pattern Analysis
# Calculate descriptive stats
mean_numtrees <- mean(Pest_Infest_2022$NUM_TREES)
sd_numtrees <- sd(Pest_Infest_2022$NUM_TREES, na.rm = TRUE)
mode_numtrees <- as.numeric(names(sort(table(Pest_Infest_2022$NUM_TREES), decreasing = TRUE))[1]) 
median_numtrees <- median(Pest_Infest_2022$NUM_TREES, na.rm = TRUE)
skew_numtrees <- skewness(Pest_Infest_2022$NUM_TREES, na.rm = TRUE)[1]
kurt_numtrees <- kurtosis(Pest_Infest_2022$NUM_TREES, na.rm = TRUE)[1]
CoV_numtrees <- (sd_numtrees / mean_numtrees) * 100
set.seed(123)  # Set seed for reproducibility
sample_numtrees <- sample(Pest_Infest_2022$NUM_TREES, size = 5000) # Take random sample to conduct shapiro test
norm_numtrees_PVAL <- shapiro.test(sample_numtrees)$p.value

# Round the values for better readability
mean <- round(mean_numtrees, 3)
sd <- round(sd_numtrees, 3)
median <- round(median_numtrees, 3)
mode <- round(mode_numtrees,3)
skewness <- round(skew_numtrees,3)
kurtosis <- round(kurt_numtrees,3)
CoV <- round(CoV_numtrees, 3)
normality <- round(norm_numtrees_PVAL, 5)

# Create tables to display the values
data.for.table1 = data.frame(mean, sd, median, mode)
data.for.table2 = data.frame(skewness, kurtosis, CoV, normality)
outCSV <- data.frame(mean, sd, median, mode, skewness, kurtosis, CoV, normality)
write.csv(outCSV, "./Output/PestDescriptiveStats_2022.csv", row.names = FALSE)

table1 <- tableGrob(data.for.table1, rows = c("")) #make a table "Graphical Object" (GrOb) 
t1Caption <- textGrob("Table 1: Descriptive Statistics for Number of \nTrees impacted by Pest Infestations \nin BC in 2022", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table1 <- gtable_add_rows(table1, 
                          heights = grobHeight(t1Caption) + padding, 
                          pos = 0)

table1 <- gtable_add_grob(table1,
                          t1Caption, t = 1, l = 2, r = ncol(data.for.table1) + 1)


table2 <- tableGrob(data.for.table2, rows = c(""))
t2Caption <- textGrob("Table 2: Additional Descriptive Statistics for Number of \nTrees impacted by Pest Infestations in BC in 2022", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table2 <- gtable_add_rows(table2, 
                          heights = grobHeight(t2Caption) + padding, 
                          pos = 0)

table2 <- gtable_add_grob(table2,
                          t2Caption, t = 1, l = 2, r = ncol(data.for.table2) + 1)

grid.arrange(table1, newpage = TRUE)
grid.arrange(table2, newpage = TRUE)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/f516db20-14a0-467b-891f-475f78cb0fc7" alt="Descrptive Statistics Table 1" width="500" />
  <img src="https://github.com/user-attachments/assets/7dca8c30-c831-4624-bbae-8f2778930550" alt="Descrptive Statistics Table 2" width="500" />
</div>
<p style="text-align: center;"><em> Figure 5: Descriptive Statistics Tables</em></p>

These tables show that the distribution of the number of trees affected is highly variable and positively skewed, with most infestation points affecting a small number of trees, but a few points experiencing significantly higher impacts. These patterns could be explained by the pest outbreaks, where a few severe infestations disproportionately affect large numbers of trees.

# Mapping the Forest Pest Disturbance Events
With this insight into what the events look like, it will now be useful to look into the spatial distribution of them. To do that, we will create a map of all of the points with the mean centre marked. 
```{r PestMapping, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Map all the pest infestation points with the mean centre
# Extract coordinates
coords <- st_coordinates(Pest_Infest_2022)

# Calculate the mean x and y coordinates
mean_x <- mean(coords[, "X"])
mean_y <- mean(coords[, "Y"])

# Create a mean center point as an sf object
mean_center <- st_sfc(st_point(c(mean_x, mean_y)), crs = st_crs(Pest_Infest_2022))

# Enhanced map with corrected arguments and legend adjustments
map_TM <- tm_shape(bc_boundary) + 
  tm_fill(col = "gray90", border.col = "gray60") +  # Light gray for boundary fill
  tm_borders(lwd = 1.2, col = "gray60") +  # Define border thickness and color
  
  # Pest infestation points
  tm_shape(Pest_Infest_2022) +
  tm_symbols(col = "darkgreen", alpha = 0.5, size = 0.1, shape = 19, 
             title.col = "Pest Infestation Points") +  # Title for color legend
  
  # Mean center point
  tm_shape(mean_center) +
  tm_symbols(col = "red", alpha = 0.9, size = 0.3, shape = 19, 
             title.col = "Mean Center") +  # Title for color legend
  
  # Add legend explicitly
  tm_add_legend(type = "symbol", labels = c("Pest Infestations", "Mean Center"), 
                col = c("darkgreen", "red"), shape = c(19, 19)) +
  
  # Layout adjustments for aesthetics
  tm_layout(
    title = "Pest Infestation Locations (2022)",  # Updated title text
    title.size = 1.5,  # Adjust title size
    title.position = c("center", "top"),  # Centered title
    legend.position = c("left", "bottom"),
    legend.title.size = 1.5,
    legend.text.size = 0.8,  # Adjust legend text size
    legend.width = 1.2,  # Increase width to accommodate text
    inner.margins = c(0.05, 0.05, 0.15, 0.05)  # Reduced margins for better fit
  )

map_TM

tmap_save(map_TM, "./Output/PestInfestLocation_MeanCentre.png", width = 10, height = 8, dpi = 300)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/fa0cbb20-a6c5-48b3-b3d0-399591236e6c" alt="Mean Centre Map" width="800" />
</div>
<p style="text-align: center;"><em> Figure 6: Map of Forest Pest Infestation Points for 2022 in BC with the Mean Centre</em></p>

# Point Pattern Analysis
To continue exploring the spatial distribution, we will use point pattern analysis. Point pattern analysis is a set of methods used to study the spatial arrangement of individual events or objects within a defined study area. This type of analysis helps us determine whether the points are randomly distributed, clustered, or regularly spaced. In the context of forest pests, point pattern analysis can help understand whether infestations are concentrated in certain regions or evenly spread out across the landscape. By analyzing the spatial patterns, we can identify potential environmental or ecological influence behind these distributions and assess whether the observed patterns are statistically significant.

When analyzing spatial point patterns, we often consider first-order and second-order variability to understand the processes influencing the arrangement of points. First-order variability arises from underlying landscape factors that influence the overall distribution of points across the study area. These are large-scale trends driven by environmental or socioeconomic factors that vary across space, like rainfall causing more landslides.

Second-order variability, on the other hand, focuses on the spatial relationships between points themselves. This is about how the location of one point might influence the location of others nearby. For instance, on a beach, ice cream stands might cluster together because their owners want to take advantage of popular spots or create competition. 

## Nearest Neighbour Analysis
For our analysis we will conduct three different types of point pattern analysis, Nearest Neighbour, Quadrat, and K-Function. Starting off, Nearest Neighbour Analysis is a statistical method used to evaluate spatial distribution by measuring the distance between each point and its nearest neighbor. These distances are then summed and divided by the total number of points to calculate the average nearest neighbor distance, also known as the mean NND. 

By calculating distances between each point and its closest neighbor, it tests whether the pattern of infestation events are random, clustered, or regularly spaced. The results will provide a statistical summary, including the average nearest neighbor distance and a Z-score, to determine the significance of the observed pattern. Nearest neighbor analysis is primarily a second-order measure as it evaluates the spatial relationships between points by examining the distances to their closest neighbors.
```{r NearestNeighbourAnalysis, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
### Nearest Neighbour Analysis

# Create an observation window
bc_extent <- as.matrix(st_bbox(bc_boundary))
window <- as.owin(list(xrange = c(bc_extent[1], bc_extent[3]), 
                        yrange = c(bc_extent[2], bc_extent[4])))

# Remove duplicate points
unique_coords <- unique(coords)

# Create ppp object with unique points
pests.ppp <- ppp(
  x = unique_coords[, 1], 
  y = unique_coords[, 2], 
  window = window
)

##Nearest Neighbour Analysis
#Conduct Nearest Neighbour Analysis
nearestNeighbour <- nndist(pests.ppp)

##Convert the nearestNeighbor object into a dataframe.
nearestNeighbour=as.data.frame(as.numeric(nearestNeighbour))

##Change the column name to "Distance"
colnames(nearestNeighbour) = "Distance"

##Calculate the nearest neighbor statistic to test for a random spatial distribution.

#First calculate the mean nearest neighbour
nnd = sum(nearestNeighbour$Distance)/nrow(nearestNeighbour)

#Next, calculate the mean nearest neighbour for random spatial distribution
studyArea <- area(bc_boundary)
pointDensity <- nrow(nearestNeighbour) / studyArea
r.nnd = 1 / (2 * sqrt(pointDensity))
d.nnd = 1.07453 / sqrt(pointDensity)
R = nnd / r.nnd

#Calculate the standard deviation
SE.NND <- .26136 / sqrt(nrow(nearestNeighbour) * pointDensity)

#Calculate the Z score
z = (nnd - r.nnd)/SE.NND

#Create a dataframe of the results
nndResults <- data.frame(StudyArea = round(studyArea, 2),
                         NNDd = round(d.nnd, 2), 
                         NNDr = round(r.nnd, 2), 
                         NND = round(nnd, 2), 
                         Zscore = round(z, 2), 
                         Ratio = round(R, 2))

#print a table of the results.
print(nndResults)

# Create table to display the values
nndtable <- tableGrob(nndResults, rows = c("")) #make a table "Graphical Object" (GrOb) 
nndCaption <- textGrob("Nearest Neighbour Distance Table", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

nndtable <- gtable_add_rows(nndtable, 
                          heights = grobHeight(nndCaption) + padding, 
                          pos = 0)

nndtable <- gtable_add_grob(nndtable,
                          nndCaption, t = 1, l = 2, r = ncol(nndResults) + 1)

grid.arrange(nndtable, newpage = TRUE)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/f69fff6d-5a80-46a5-b8d1-1a7c5e337b29" alt="NearestNeighbourTable" width="800" />
</div>
<p style="text-align: center;"><em> Figure 7: Nearest Neighbour Results Table</em></p>

The results show that the points in the study area are clustered together, not spread out randomly or evenly. The average distance between points is much smaller than what would be expected if the points were placed randomly or evenly. The Z-score of -144.06 confirms that this clustering is very significant and not just due to chance. The ratio of 0.45 also supports this because it’s less than 1, meaning the points are much closer to each other than we’d expect in a random distribution.

## Quadrat Analysis
Quadrat analysis is another method used to evaluate the spatial distribution of points, helping determine whether they are clustered, evenly spaced, or randomly distributed. It works by dividing the study area into a grid of equally sized quadrats and counting the number of points within each quadrat. The variation in these counts across the grid is then analyzed statistically to identify patterns in the distribution.

By calculating a variance-to-mean ratio (VMR), quadrat analysis provides insight into the overall spatial pattern. A VMR near 1 suggests a random distribution, a value larger than 1 indicates clustering, and a value less than 1 points to an even distribution. To assess the significance of these patterns, we use a statistical test called the chi-squared test.

This approach is useful for examining large-scale patterns but has limitations, including sensitivity to the size of the quadrats and the inability to capture finer details about point locations within each cell. Quadrat analysis focuses on first-order variability, as it is influenced by underlying landscape factors that affect point density. 

For this analysis, we have selected 12 as the value for quads, which means there will be 12x12 or 144 quadrats defining our study area. 144 was selected to get a good amount of coverage across the province without having too many cells and increacing the computational requirements.
```{r QuadratAnalysis, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
##Quadrat Analysis
##First, determine the number of quadrats. You need to specify a number that makes sense given the number of points and the size of the study area. Note that quads equals the number of rows or columns in your quads dataset, therefore the actual number is quads^2 
quads <- 12

qcount <- quadratcount(pests.ppp, nx = quads, ny = quads)

#You can use the two lines of code below to see what the quadrats look like when placed on your study site.
#plot(pests.ppp, pch = "+", cex = 0.5)
#plot(qcount, add = T, col = "red")

#Transform the qcount object into a dataframe.
qcount.df <- as.data.frame(qcount)

##Count the number of quadrats with a distinct number of points. This will be used in your Quadra Analysis formula below. If you are unsure that this object looks like, you can always print it in the console by simply typling `qcount.df`
qcount.df <- plyr::count(qcount.df,'Freq')

##Change the column names so that x=number of points and f=frequency of quadrats with x point.
colnames(qcount.df) <- c("x","f")

#Caluclate the Quadrat Analysis statistics
sum.f.x2 <- sum(qcount.df$f * (qcount.df$x^2))
M <- sum(qcount.df$f)
N <- sum(qcount.df$x * qcount.df$f)
sum.fx.2 <- (sum(qcount.df$x * qcount.df$f)) ^ 2
VAR <- ((sum.f.x2) - (sum.fx.2 / M)) / (M - 1)
MEAN <- (N/M)
VMR <- (VAR/MEAN)

##Finally, perform the test statistic to test for the existence of a random spatial pattern.
chi.square = VMR * (M - 1)
p = 1 - pchisq(chi.square, (M - 1))

quadResults <- data.frame(Quadrats = quads * quads, 
                          Variance = round(VAR, 2), 
                          Mean = round(MEAN, 2), 
                          VMR = round(VMR, 2), 
                          Chisquare = round(chi.square, 2))

#Print a table of the results.
print(quadResults)

# Create table to display the values
quadtable <- tableGrob(quadResults, rows = c("")) #make a table "Graphical Object" (GrOb) 
quadCaption <- textGrob("Quadrat Analysis Table", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

quadtable <- gtable_add_rows(quadtable, 
                            heights = grobHeight(quadCaption) + padding, 
                            pos = 0)

quadtable <- gtable_add_grob(quadtable,
                            quadCaption, t = 1, l = 2, r = ncol(quadResults) + 1)

grid.arrange(quadtable, newpage = TRUE)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/b6a40d47-7f1c-4a2b-9d2e-f0321ce07022" alt="QuadratTable" width="800" />
</div>
<p style="text-align: center;"><em> Figure 8: Quadrat Analysis Results Table</em></p>

These results show that the points in our study area are highly clustered. The variance is much larger than the mean, indicating a big difference in the number of points across the quadrats, and with the VMR being 497.33, this suggests that the points are clustered together. The chi-square value is very large, confirming that this clustering is statistically significant.

## K-Function
Our last point pattern analysis method, K-function, is a powerful tool for understanding spatial patterns and how they change across different distances. Unlike other methods that look at a single scale, the K-function helps us explore patterns at multiple scales, providing a deeper understanding of clustering or dispersion in the data.

The K-function works by comparing the actual distribution of points to what would be expected in a completely random pattern. It calculates the number of points within varying distances of each other and examines whether there are more or fewer points than expected at those distances. If more points are found than expected, the pattern is likely clustered, but if fewer points are found, it suggests dispersion.

This method is useful for analyzing second-order variability, as it focuses on the relationships between points at different distances. For example, it can show whether clustering occurs at close distances but transitions to random or dispersed patterns farther away.

The results are visualized in a graph comparing the observed pattern to an expected random pattern. This makes it easy to see where clustering or dispersion occurs and at what scales these patterns emerge.
```{r KFunction, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
##K-FUNCTION 
#Create a basic k-function
k.fun <- Kest(pests.ppp, correction = "Ripley")
#You can use plot(k.fun) to print the plot you just created.

#use simulation to test the point pattern against CSR
k.fun.e <- envelope(pests.ppp, Kest, nsim = 99, correction = "Ripley", verbose = FALSE)
plot(k.fun.e, main = "")
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/dca9a09c-e0cb-419d-8e37-5c008657f924" alt="KFunPlot" width="800" />
</div>
<p style="text-align: center;"><em> Figure 9: K-Function Plot</em></p>

The K-function plot shows that at smaller distances, the observed point pattern closely follows the expected random distribution (Red line), indicating a random arrangement of points at these scales. However, as the distance increases, the observed K-function moves further away from the expected line, suggesting that the points become increasingly clustered at larger distances. This shows that the points may appear randomly distributed on smaller scales, but they tend to cluster in certain areas when you get to larger scales.

Overall, the results from these three point pattern analyses indicate that the points are clustered rather than randomly distributed. Both the Nearest Neighbour and Quadrat Analyses suggest clustering, and the K-function analysis supports this by showing that the observed distribution deviates from the expected random pattern as the distance increases. These findings highlight a clear pattern of clustering across all three methods. 

# Mapping the Forest Pest Disturbance Density
Now that we have determined the spatial distribution of points, we can move on to creating a density map of the events. This will provide us with a visual aid to understand the patterns of the events across BC. The only parameter needed for this section is the resolution of the final output. In an effort to be consistant across this project, we will match the resolution of 25,000 that we used in IDW and Kriging.
```{r MapPestDensity, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Ensure bbox2 is valid and formatted correctly
bbox2 <- st_bbox(bc_boundary)

raster_res <- 25000  # This resolution is 50000 meters 
raster_template <- raster(extent(bbox2), res = c(raster_res, raster_res))

# Estimate density using kernel density estimate
density_raster <- raster::rasterize(st_as_sf(Pest_Infest_point), raster_template, fun = "count", field = 1)

# Ensure all NAs are turned to zeros in the raster
density_raster[is.na(density_raster)] <- 0

# Convert the raster to a data frame and replace any potential NAs with zeros
density_df <- as.data.frame(density_raster, xy = TRUE)
density_df[is.na(density_df)] <- 0  # Replace NAs in the data frame with zeros

# Step to rename the 'layer' column to 'Pest_Infestations' if applicable
colnames(density_df)[colnames(density_df) == "layer"] <- "Pest_Infestations"

# Convert to a spatial points data frame using sf (if needed later)
density_sf <- st_as_sf(density_df, coords = c("x", "y"), crs = st_crs(bc_boundary))

# Plotting the density map with the polygon boundary
ggplot() +
  geom_raster(data = density_df, aes(x = x, y = y, fill = Pest_Infestations)) +  # Use 'Pest_Infestations' from the data frame
  geom_sf(data = bc_boundary, fill = NA, color = "white") + # Boundary polygon
  scale_fill_viridis_c(option = "plasma") +  # Using a color scale
  theme_minimal() +
  labs(title = "Density Map of Pest Infestations in BC in 2022",
       x = "Longitude",
       y = "Latitude",
       fill = "Density")

ggsave("./Output/Pest_Infestation_DensityMap.png", width = 10, height = 8, dpi = 300)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/e71278ae-8ad7-4902-8200-35a4077188b2" alt="Pest Density Map" width="800" />
</div>
<p style="text-align: center;"><em> Figure 10: Map of Forest Pest Infestation Event Density Across BC in 2022</em></p>
As you can see in the resulting map above, there are a couple clusters in the middle of the province. It also shows that the overall pattern of disturbance is mostly in the southern half.

There are also other ways to display this data on a map. One way, shown below, is with points coloured by density.
```{r MapPestDensityPoint, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Convert the raster to a data frame
density_df <- as.data.frame(density_raster, xy = TRUE)

# Rename the 'layer' column to 'Pest_Infestations'
colnames(density_df)[colnames(density_df) == "layer"] <- "Pest_Infestations"

# Replace NA values with zeros
density_df[is.na(density_df$Pest_Infestations), "Pest_Infestations"] <- 0

# Convert to a spatial points data frame using sf
density_sf <- st_as_sf(density_df, coords = c("x", "y"), crs = st_crs(bc_boundary))

# Write to a shapefile
st_write(density_sf, "./Output/density_points.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)

density_sf_clipped <- st_intersection(density_sf, bc_boundary)

# Create a simple map
ggplot() +
  geom_sf(data = bc_boundary, fill = NA, color = "black") +  # Plot the boundary polygon
  geom_sf(data = density_sf_clipped, aes(color = Pest_Infestations), size = 1) +  # Plot the density points with color mapping
  scale_color_viridis_c(option = "plasma", name = "Density of Pest Infestations") +  # Color scale for density values
  theme_minimal() +
  labs(title = "Density of Pest Infestations within Boundary",
       x = "Longitude",
       y = "Latitude")

ggsave("./Output/Pest_DensityPointMap.png", width = 10, height = 8, dpi = 300)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/65a73ef8-82b5-4dfa-8ac5-6f07ef7f9ff0" alt="Pest Density Point Map" width="800" />
</div>
<p style="text-align: center;"><em> Figure 11: Map of Forest Pest Infestation Event Density Points Across BC in 2022</em></p>

The last method of mapping the density of the pest infestation events is to clip the data to the BC boundary and display the lines of the raster. This makes the map more readable and presentable. In this section of data, we will also combine the temperature data from the IDW interpolation and the pest infestation data into one dataset. 
```{r MapPestDensityClipped, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Perform the spatial join
joined_data <- st_join(idw_clipped, density_sf, join = st_intersects)

# Select needed columns
final_data <- joined_data[, c("var1.pred", "Pest_Infestations")]

# Rename column
final_data <- final_data %>%
  rename(temperature = var1.pred)

# Replace NA values in the Pest_Infestations column with 0
final_data <- final_data %>%
  mutate(Pest_Infestations = ifelse(is.na(Pest_Infestations), 0, Pest_Infestations))

ggplot(data = final_data) +
  geom_sf(aes(fill = Pest_Infestations)) +
  scale_fill_viridis_c(option = "C", name = "Pest Infestations Density") +  # Updated legend for infestations
  theme_minimal() +
  labs(
    title = "Pest Infestation Density Across BC (2022)",
    fill = "Pest Infestations Density (Count per Area)"
  ) +
  theme(legend.position = "right")

ggsave("./Output/Pest_DensityFinalMap.png", width = 10, height = 8, dpi = 300)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/8dcc9d56-f6ed-4005-9877-34fee66bcf30" alt="Pest Density Final Map" width="800" />
</div>
<p style="text-align: center;"><em> Figure 12: Clipped Map of Forest Pest Infestation Event Density Across BC in 2022</em></p>

Now that we can create a presentable density map, we can also apply it to the temperature data for visualization. 
## Mapping Temperature Density
```{r MapTempClipped, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# For temperature, if adding a second scale for temperature:
ggplot(data = final_data) +
  geom_sf(aes(fill = temperature)) +
  scale_fill_viridis_c(option = "plasma", name = "Temperature (°C)") +
  theme_minimal() +
  labs(
    title = "Predicted Temperature Across BC \nfrom November 2021 to March 2022",
    fill = "Temperature (°C)"
  ) +
  theme(legend.position = "right")

ggsave("./Output/TemperatureDensityMap.png", width = 10, height = 8, dpi = 300)

# Save final_data as a shapefile
st_write(final_data, "./Output/final_data.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)

# Convert final_data to a data frame
final_data_df <- st_drop_geometry(final_data)

# Write as CSV
write.csv(final_data_df, "./Output/final_data.csv", row.names = FALSE)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/8b56b250-fa77-4f7f-afb5-befcd04ec348" alt="Temperature Raster Map" width="800" />
</div>
<p style="text-align: center;"><em> Figure 13: Map of Predicted Average Temperature Across BC from November 2021 to March 2022</em></p>

Now we have the raster data that has values for both forest pest infestation events in 2022 and predicted temperature for the winter beforehand. Using this data, we can look into if there is an association between the two. 
# Ordinary Least Squares Regression
Ordinary Least Squares (OLS) regression is a statistical method used to examine the relationship between one dependent variable and one or more independent variables. It does this by finding the line that minimizes the sum of the squared differences between the observed values and the values predicted by the model. In this case, we will use OLS regression to explore whether there is a relationship between the predicted average temperature for winter 2021/2022 and forest pest infestations in 2022. By fitting a regression model, we can assess how well temperature may explain variations in pest infestations and determine whether temperature acts as a significant predictor in this context.

The following code will calculate the residuals from the model and visualize the residuals on a map. This process will help us understand how well the predicted winter temperatures explain the variation in forest pest infestations. By calculating and visualizing the residuals, we can identify any patterns that are not accounted for by the temperature data. If significant spatial patterns remain in the residuals, it suggests that other factors beyond temperature may be influencing forest pest infestations.
```{r OLSRegression, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Read the shapefile
final_data_sf <- st_read("./Output/final_data.shp")

# Fit the OLS regression model on the entire spatial data
# Use "temprtr" instead of "temperature"
ols_model <- lm(Pst_Inf ~ temprtr, data = final_data_sf)

# Add residuals to the original spatial data frame
final_data_sf$residuals <- resid(ols_model)

# Inspect the updated spatial object to verify residuals are added
print(head(final_data_sf))

# (Optional) Save the updated shapefile with residuals
st_write(final_data_sf, "./Output/final_data_with_residuals.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)

# Create a map of residuals from the OLS regression
ggplot(data = final_data_sf) +
  geom_sf(aes(fill = residuals)) + # Map the residuals to fill color
  scale_fill_viridis_c(option = "C", name = "Residuals") + # Use a color scale
  theme_minimal() +
  labs(title = "Map of Residuals from OLS Regression",
       fill = "Residuals") +
  theme(legend.position = "right")

# Optional: Save the plot if desired
ggsave("./Output/residuals_map.png", width = 10, height = 8, dpi = 300)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/7975bbe3-5d57-4e37-a2c9-c9e7233ca4b0" alt="OLS Regression Residuals Map" width="800" />
</div>
<p style="text-align: center;"><em> Figure 14: Ordinary Least Squares Regression Residuals Map</em></p>

The residuals map shows significant variability in the difference between observed and predicted forest pest infestation event densities, with a wide range of positive and negative residuals. This suggests that the model does not fully capture the spatial variation in pest infestations, as some areas are significantly over- or under-predicted. The large spread of residuals, from negative to very high positive values, indicates that temperature alone may not be a good predictor for forest pest infestations, and there may be other unaccounted factors influencing the distribution. This variability could also suggest the presence of spatial autocorrelation in the residuals, something that we will investigate.

## Spatial Autocorrelation of OLS Residuals
After analyzing the residuals from the Ordinary Least Squares (OLS) regression, we can now investigate whether spatial autocorrelation plays a role in the distribution of pest infestations.

Spatial autocorrelation refers to the degree to which a variable at one location is related to the values of the same variable at nearby locations. It helps identify patterns of spatial dependence, where observations that are closer in space, exhibit similar values, or obervations further away show dissimilar values. Positive spatial autocorrelation indicates clustering of similar values, while negative autocorrelation suggests dispersion or the presence of opposing values in close proximity. No autocorrelation means there is a random distribution of values.

This next section of code uses the concept of spatial autocorrelation to examine the relationship between residuals of the pest infestation model and their spatial arrangement. We first create a neighborhood matrix using Inverse Distance Weighting, where the proximity of each observation to its neighbors is taken into account and further neighbours have less weight in the matrix. Using this matrix, we then compute Global Moran's I, which helps identify whether there is significant spatial clustering of the residuals. A positive Global Moran's I suggests clustering, while a negative value indicates dispersion. Additionally, we conduct a Local Moran's I or Local Indicators of Spatial Association (LISA) test to identify localized areas where this clustering is most pronounced. The results of these analyses are displayed in maps and scatter plots to provide a clearer understanding of the spatial patterns present in the data.

```{r MoransI, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Making a neighbourhood matrix with Inverse Distance Weighting
# Define a maximum distance threshold (e.g., 100 km)
max_distance <- 100000  # in meters

# Calculate centroids of polygons
final_data_centroids <- st_centroid(final_data_sf)

# Extract coordinates
final_data_coords <- st_coordinates(final_data_centroids)

# Create neighbor object based on distances
pest.nb <- dnearneigh(final_data_coords, d1 = 0, d2 = max_distance)

# Convert to a listw object with inverse distance weights
pest.listw <- nb2listw(pest.nb, glist = lapply(nbdists(pest.nb, final_data_coords), function(d) 1 / d), style = "W")

# Calculate Global Moran's I
miPest <- moran.test(final_data_sf$residuals, pest.listw, zero.policy = TRUE)

#Extract Global Moran's I results for French
mIPest <- miPest$estimate[[1]]
eIPest <- miPest$estimate[[2]]
varPest <- miPest$estimate[[3]]

#Function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#Calculate the range for the variable
range <- moran.range(pest.listw)
minRange <- range[1]
maxRange <- range[2]

print(minRange)
print(maxRange)

#Calculate z-test
zPest <- (mIPest - eIPest) / (sqrt(varPest))

print(zPest)

# Create a table to display Global Moran's I results
results_table <- data.frame(
  Variable = c("Residuals"),
  Moran_I = c(mIPest),
  Expected_I = c(eIPest),
  Variance = c(varPest),
  Z_Score = c(zPest)
)

# Print the table
print(results_table)

# Create table to display the values
moransitable <- tableGrob(results_table, rows = c("")) #make a table "Graphical Object" (GrOb) 
moransiCaption <- textGrob("Moran's I Results Table", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

moransitable <- gtable_add_rows(moransitable, 
                            heights = grobHeight(moransiCaption) + padding, 
                            pos = 0)

moransitable <- gtable_add_grob(moransitable,
                            moransiCaption, t = 1, l = 2, r = ncol(results_table) + 1)

grid.arrange(moransitable, newpage = TRUE)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/d26d8e5f-c1d2-49d1-ad87-137c4bd4524a" alt="Morans I Table" width="800" />
</div>
<p style="text-align: center;"><em> Figure 15: Moran's I Results Table</em></p>

The Moran's I results indicate a significant positive spatial autocorrelation in the residuals from the OLS regression model. The observed Moran's I value of 0.608 is much higher than the expected value of approximately -0.0006, which would occur if the pattern was random. The high Z-score of 100.42 shows that this level of spatial clustering is highly significant. This means that even after accounting for temperature in the regression model, the residuals show a strong spatial pattern, indicating that other factors may influence forest pest infestation events and winter temperature is not the only reason.

Next, we can move onto the Local Moran's I and create a map to visualize the spatial autocorrelation.
```{r LocalMoransI, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Local spatial autocorrelation

# Calculate LISA test
lisa.testPest <- localmoran(final_data_sf$residuals, pest.listw)

#Extract LISA test results
final_data_sf$Ii <- lisa.testPest[,1]
final_data_sf$E.Ii<- lisa.testPest[,2]
final_data_sf$Var.Ii<- lisa.testPest[,3]
final_data_sf$Z.Ii<- lisa.testPest[,4]
final_data_sf$P<- lisa.testPest[,5]

#Map LISA z-score
map_LISA_Pest <- tm_shape(final_data_sf) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores for residuals",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(final_data_sf$Z.Ii),-1.96,1.96,max(final_data_sf$Z.Ii)),
              palette = "PiYG", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

map_LISA_Pest

tmap_save(map_LISA_Pest, "./Output/LocalMoransI.png", width = 10, height = 8, dpi = 300)
```
<div style="display: flex;">
  <img src="https://github.com/user-attachments/assets/cfbd51ca-49c2-4b0f-b008-ed06a81cd4e4" alt="Local Morans I Map" width="800" />
</div>
<p style="text-align: center;"><em> Figure 16: Local Moran's I Map</em></p>

```{r MoransIScatterplot, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
#Create Moran's I scatter plot
moran.plot(final_data_sf$residuals, pest.listw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Pest Infestation Residuals", 
           ylab="Spatially Lagged Pest Infestation Residuals", quiet=NULL)
```
# Geographic Weighted Regression

```{r GWRRegression, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
# Preview the data to check variable names and content
print(head(final_data_sf))
print(colnames(final_data_sf))

# Convert the sf object to Spatial object
final_data_sp <- as_Spatial(final_data_sf)

# Create neighborhood structure
neighbors <- poly2nb(final_data_sp, queen = TRUE)

# Check neighbors for any issues
print(summary(neighbors))

# Check for any empty neighbors
if (any(sapply(neighbors, length) == 0)) {
  warning("Some polygons have no neighbors. This may cause issues for GWR.")
}

# Prepare the dependent and independent variables
dependent_var <- final_data_sp@data$Pst_Inf
independent_vars <- final_data_sp@data$temprtr

# Check if both variables are numeric
if (!is.numeric(dependent_var) || !is.numeric(independent_vars)) {
  stop("Dependent and independent variables must be numeric.")
}

# Run GWR with a fixed bandwidth of 200 km
fixed_bandwidth <- 200000  # Bandwidth in meters (200 km)

gwr_model_fixed <- gwr(dependent_var ~ independent_vars, 
                       data = final_data_sp, 
                       bandwidth = fixed_bandwidth, 
                       se.fit = TRUE)

# Validate that the model ran successfully
if (is.null(gwr_model_fixed)) {
  stop("The GWR model did not return any results.")
}

if (is.null(gwr_model_fixed$SDF)) {
  stop("The GWR model SDF is NULL, indicating it might not have calculated properly.")
}

# Print GWR summary
print(summary(gwr_model_fixed))

# Extract coefficients and create a dataframe for visualization
gwr_results_fixed <- as.data.frame(gwr_model_fixed$SDF)

# Extract coordinates from the original spatial data
coordinates_fixed <- st_coordinates(st_centroid(final_data_sf))

# Combine the GWR results with the coordinates
# Assuming GWR results correspond directly (else we may need to adjust identifiers),
# Make sure to bind them under the known column names for proper mapping.
gwr_results_fixed <- cbind(gwr_results_fixed, coordinates_fixed)

# Convert to an sf object for visualization
# Adjusting the coordinate column names based on what exists in gwr_results_fixed
# Normally, standard output names would have been “coords.X1” and “coords.Y” or similar
gwr_output_sf_fixed <- st_as_sf(gwr_results_fixed, coords = c("X", "Y"), crs = st_crs(final_data_sf))

# Create the map with points
tm_gwr <- tm_shape(gwr_output_sf_fixed) + 
  tm_dots(col = "gwr.e", palette = "viridis", size = 0.3, title = "GWR Estimate") +  # Color points by gwr.e values
  tm_layout(
    title = "GWR Coefficients with Fixed Bandwidth of 200 km",
    inner.margins = c(0.05, 0.1, 0.2, 0.1),  # Adjust margins to create space for title
    title.size = 5,             # Increase the size of the title
    title.position = c("left", "0.9"),  # Ensure title is centered at the top
    legend.position = c(0.7, 0.45),  # Adjust legend position (x, y) to move it lower
    legend.title.size = 2,  # Adjust legend title size if needed
    legend.text.size = 1.5,    # Adjust legend text size if needed
    legend.width = 1
  ) +
  # Add scale bar
  tm_scale_bar(position = c("left", "bottom"), width = 0.2, text.size = 0.8) + 
  # Add north arrow
  tm_compass(position = c("right", "bottom"), size = 1.5)
tm_gwr

# Optional: Save the plot
tmap_save(tm_gwr, "./Output/gwr_coefficients_fixed_bandwidth.png", width = 10, height = 8, dpi = 300)
```
# Conclusion

# References
