library(tidyverse)
library(ggplot2)
library(tidycensus)
library(sf)
library(tigris)
library(gt)
library(ggpattern)
library(ggnewscale)
library(tidyterra)
library(terra)

# Data Pull for Census ACS ------------------------------------------------

    # ACS Variables I want to pull from the census data using the TidyCensus package
Variables <- c(
  White = "B03002_003",
  Black = "B03002_004", 
  Asian = "B03002_006",
  AIAN = "B03002_005",
  Hispanic = "B03002_012",
  Other = "B03002_008",
  Below_Poverty = "B17020_002",
  Total_Population = "B17020_001")

    # Pulling the ACS data from TidyCensus and making poverty and % non-white groupings
Cuyahoga_ACS <- get_acs(
  geography = "tract", 
  state = "OH",  
  county = "Cuyahoga", # You would change this for different counties of interest
  variables = Variables, 
  summary_var = c(total_pop = "B03002_001"),
  output = "wide",
  geometry = TRUE) %>% 
  mutate(
    total_nonwhite = BlackE + AsianE + HispanicE + AIANE + OtherE,
    percent_nonwhite = 100 * total_nonwhite / summary_est,
    group = case_when(
      percent_nonwhite < 20 ~ "0-20%",
      percent_nonwhite < 50 ~ "20-50%",
      percent_nonwhite < 80 ~ "50-80%",
      percent_nonwhite <= 100 ~ "80-100%"),
    Poverty_Rate = (Below_PovertyE / Total_PopulationE) * 100,
    Poverty_Group = case_when(
      Poverty_Rate < 10 ~ "Low Poverty (<10%)",
      Poverty_Rate >= 10 & Poverty_Rate < 20 ~ "Moderate Poverty (10-20%)",
      Poverty_Rate >= 20 & Poverty_Rate < 30 ~ "High Poverty (20-30%)",
      Poverty_Rate >= 30 ~ "Very High Poverty (30%+)"),
    Poverty_Group = factor(
      Poverty_Group,
      levels = c(
        "Low Poverty (<10%)",
        "Moderate Poverty (10-20%)",
        "High Poverty (20-30%)",
        "Very High Poverty (30%+)"))) %>% 
  st_transform(., 3734)

# Data Pull ---------------------------------------------------------------
LIHTC_Points = read_sf("./Data/Low_Income_Housing_Tax_Credit_Properties/Cuyahoga") 
Housing_Choice_Tracts <- read_sf("./Data/Housing_Choice_Vouchers_by_Tract/Cuyahoga")
CountyACS <- Cuyahoga_ACS

    # This is a table theme for the gt package so that my function produces easily viewed tables
custom_tabletheme <- function(gt_table) {
  gt_table %>%
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom", "left", "right"),
        color = "black",
        weight = px(1),
        style = "solid"),
      locations = list(
        cells_body(),
        cells_column_labels(),
        cells_stubhead(),
        cells_stub(),
        cells_row_groups(),
        cells_column_spanners())) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels())
}


# LIHTC Function ----------------------------------------------------------
LIHTC_Function <- function(LIHTC_Points, CountyACS) {
  
  LIHTC_Points <- st_transform(LIHTC_Points, 3734)
  CountyACS <- st_transform(CountyACS, 3734)
  
  # Spatial join
  points_in_tracts <- st_join(LIHTC_Points, CountyACS, join = st_within)
  
  # Count points per tract using dplyr
  tract_counts <- points_in_tracts %>%
    count(GEOID)
  
  # Drop geometry from tract_counts to make it a regular data frame
  tract_counts <- tract_counts %>%
    rename(LIHTC_Cnt = n) %>%
    st_drop_geometry()
  
  # Join with the census tracts to get the count column
  LIHTC_Nonwhite_DF <- CountyACS %>%
    left_join(tract_counts, by = "GEOID") %>%
    mutate(LIHTC_Cnt = replace_na(LIHTC_Cnt, 0)) %>% 
    mutate(LIHTC_Pct = (LIHTC_Cnt/(sum(tract_counts$LIHTC_Cnt)))*100) %>% 
    mutate(NAME = str_remove(NAME, "Census Tract ")) %>%
    rename(Census_Tract = NAME,) %>% 
    select(GEOID, Census_Tract, group, LIHTC_Cnt, LIHTC_Pct, Poverty_Group, Poverty_Rate)
  
  #### TABLE - LIHTC BY NON-WHITE ####
  LIHTC_Nonwhite_Table <- LIHTC_Nonwhite_DF %>% 
    group_by(group) %>% 
    summarize(Count = sum(LIHTC_Cnt), Percent = sum(LIHTC_Pct)) %>% 
    st_drop_geometry()
  
  Nonwhite_Table <- LIHTC_Nonwhite_Table %>%
    mutate(group = replace_na(group, "80-100%")) %>%
    rename(
      `Percent Non-White` = group,
      `Number of LIHTC` = Count,
      `Percent of Total LIHTC` = Percent) %>%
    gt() %>%
    custom_tabletheme()
  
  #### TABLE - LIHTC BY POVERTY ####
  LIHTC_Poverty_Table <- LIHTC_Nonwhite_DF %>% 
    group_by(Poverty_Group) %>% 
    filter(!is.na(Poverty_Rate)) %>%
    mutate(Poverty_Group = factor(Poverty_Group, 
                                  levels = c("Low Poverty (<10%)", 
                                             "Moderate Poverty (10-20%)", 
                                             "High Poverty (20-30%)", 
                                             "Very High Poverty (30%+)"))) %>% 
    summarize(Count = sum(LIHTC_Cnt), Percent = sum(LIHTC_Pct)) %>% 
    st_drop_geometry()
  
  Poverty_Table <- LIHTC_Poverty_Table %>%
    rename(
      `Poverty Rate` = Poverty_Group,
      `Number of LIHTC` = Count,
      `Percent of Total LIHTC` = Percent) %>%
    gt() %>%
    custom_tabletheme()
  
  ### Bar chart - LIHTC by poverty
  LIHTC_Poverty_BarChart <- ggplot(LIHTC_Poverty_Table, aes(x = Poverty_Group, y = Percent, fill = Poverty_Group)) +
    geom_col(show.legend = FALSE, fill = "red") + 
    labs(
      title = "Distribution of LIHTC Properties by Poverty Group",
      x = "Poverty Group",
      y = "Percent of Total LIHTC Properties",
      caption = "Data Source: American Community Survey 2018-2022 5 year estimates. Calculations by FHC.") +
    theme_minimal() +  
    theme(
      strip.text = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 16, face = "bold"),
      plot.caption = element_text(size = 10, color = "gray"),
      panel.grid.major = element_line(color = "gray90", size = 0.5),  
      panel.grid.minor = element_blank())
  
  #### TABLE - LIHTC BY CONCENTRATED POVERTY ####
  LIHTC_Poverty_Table_Concentrated <- LIHTC_Nonwhite_DF %>% 
    filter(!is.na(Poverty_Rate)) %>%
    mutate(Concentrated_Poverty_Group = ifelse(Poverty_Rate < 30, 
                                               "Non-Concentrated Poverty (<30%)", 
                                               "Concentrated Poverty (30%+)"),
           Concentrated_Poverty_Group = factor(Concentrated_Poverty_Group,
                                               levels = c("Non-Concentrated Poverty (<30%)",
                                                          "Concentrated Poverty (30%+)"))) %>%
    group_by(Concentrated_Poverty_Group) %>% 
    summarize(Count = sum(LIHTC_Cnt), 
              Percent = sum(LIHTC_Pct)) %>%
    st_drop_geometry()
  
  Concentrated_Table <- LIHTC_Poverty_Table_Concentrated %>%
    rename(
      `Poverty Rate` = Concentrated_Poverty_Group,
      `Number of LIHTC` = Count,
      `Percent of Total LIHTC` = Percent) %>%
    gt() %>%
    custom_tabletheme()
  
  #### MAP - PERCENT NON-WHITE ####
  Nonwhite_Map <- ggplot(data = LIHTC_Nonwhite_DF) +
    geom_sf_pattern(
      aes(fill = group, pattern = group),
      pattern_colour = NA,
      pattern_fill = "black",
      pattern_density = 0.2,
      pattern_spacing = 0.02) +
    scale_fill_manual(
      values = c(
        "0-20%" = "transparent",  
        "20-50%" = "transparent",  
        "50-80%" = "transparent",  
        "80-100%" = "red"),
      name = "Percent Non-White") +
    scale_pattern_manual(
      values = c(
        "0-20%" = "none",     
        "20-50%" = "circle",   
        "50-80%" = "stripe",  
        "80-100%" = "none"),
      name = "Percent Non-White") +
    theme_classic() +
    labs(title = "Percentage non-white distribution across census tracts",
         caption = 'Data Source: American Community Survey 2018-2022 5 year estimates. Calculations by FHC.') +
    theme(
      legend.position = "right",
      plot.caption.position = "plot",
      legend.text = element_text(size = 14),   
      legend.title = element_text(size = 16),
      plot.caption = element_text(hjust = .5),
      plot.title = element_text(size = 23, hjust = 0.5),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank())
  
  #### MAP - BUBBLE LIHTC PROPS #### 
  tracts_centroids <- LIHTC_Nonwhite_DF %>%
    st_centroid() %>%
    filter(LIHTC_Cnt > 0)
  
  Bubble_LIHTC <- ggplot() +
    # Base map with tract outlines
    geom_sf(data = LIHTC_Nonwhite_DF, fill = "gray90", color = "black", size = 0.3) +  
    # Bubble overlay at centroids with size and color mapping
    geom_sf(data = tracts_centroids, aes(size = LIHTC_Cnt, fill = LIHTC_Cnt), 
            alpha = 0.5, color = "black", shape = 21) + 
    scale_size(range = c(3, 8), name = "Quantity of LIHTC Properties") +  
    scale_fill_viridis_c(option = "C", name = "Quantity of LIHTC Properties") +
    theme_classic() +
    labs(title = "LIHTC distribution across census tracts",
         caption = 'Data Source: U.S. Department of Housing and Urban Development, U.S. Census Bureau. Calculations by FHC.') +
    theme(
      legend.position = "right",
      plot.caption.position = "plot",
      legend.text = element_text(size = 14),   
      legend.title = element_text(size = 16),
      plot.caption = element_text(hjust = .5),
      plot.title = element_text(size = 23, hjust = 0.5),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank())
  
  #### MAP - INCOME ####
  Income_Map <- ggplot(data = LIHTC_Nonwhite_DF) +
    geom_sf_pattern(
      aes(fill = Poverty_Group, pattern = Poverty_Group),
      pattern_colour = NA,
      pattern_fill = "black",
      pattern_density = 0.2,
      pattern_spacing = 0.02) +
    scale_fill_manual(
      values = c(
        "Low Poverty (<10%)" = "white",
        "Moderate Poverty (10-20%)" = "white",
        "High Poverty (20-30%)" = "white",
        "Very High Poverty (30%+)" = "red"),
      name = "Poverty Rate") +
    scale_pattern_manual(
      values = c(
        "Low Poverty (<10%)" = "none", 
        "Moderate Poverty (10-20%)" = "circle",
        "High Poverty (20-30%)" = "stripe",     
        "Very High Poverty (30%+)" = "none"),
      name = "Poverty Rate") +
    theme_classic() +
    labs(title = "Poverty rate distribution across census tracts",
         caption = 'Data Source: American Community Survey 2018-2022 5 year estimates. Calculations by FHC.') +
    theme(
      legend.position = "right",
      plot.caption.position = "plot",
      legend.text = element_text(size = 14),   
      legend.title = element_text(size = 16),
      plot.caption = element_text(hjust = .5),
      plot.title = element_text(size = 23, hjust = 0.5),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank())
  
  # Return all outputs as a list to pull up later
  return(list(
    dataframes = list(
      LIHTC_Nonwhite_DF),
    tables = list(
      Nonwhite_Table,
      Poverty_Table,
      Concentrated_Table),
    maps = list(
      nonwhite_map = Nonwhite_Map,
      bubble_map = Bubble_LIHTC,
      income_map = Income_Map)))
}


# LIHTC Analysis Results --------------------------------------------------
LIHTC_results <- LIHTC_Function(LIHTC_Points, CountyACS)

    # Saving the maps is the fastest and easiest way to view them from the function in this case
ggsave("nonwhite_map.png", plot = LIHTC_results$maps$nonwhite_map, width = 10, height = 8)
ggsave("bubble_LIHTC_map.png", plot = LIHTC_results$maps$bubble_map, width = 10, height = 8)
ggsave("income_map.png", plot = LIHTC_results$maps$income_map, width = 10, height = 8)
    
    # Print to view all the tables created
print(LIHTC_results$tables[[1]])
print(LIHTC_results$tables[[2]])
print(LIHTC_results$tables[[3]])



# HCV Function ------------------------------------------------------------
HCV_Function <- function(Housing_Choice_Tracts, CountyACS){
  
  # Processing of HCV data to get sum and percent by county
  Housing_Choice_Tracts1 <- Housing_Choice_Tracts %>%
    st_drop_geometry() %>%
    mutate(HCV_PUBLIC = as.numeric(unlist(HCV_PUBLIC))) %>% 
    mutate(HCV_PUBLIC_pct = HCV_PUBLIC/(sum(Housing_Choice_Tracts$HCV_PUBLIC)) * 100)
  
  # Join to County ACS Data to combine with non-white/poverty info
  ACS_Housing_Choice <- CountyACS %>%
    left_join(Housing_Choice_Tracts1, by = "GEOID") %>% 
    mutate(HCV_PUBLIC = replace_na(HCV_PUBLIC, 0)) %>% 
    mutate(HCV_PUBLIC_pct = replace_na(HCV_PUBLIC_pct, 0))
  
  ### TABLE - HCV BY % NON-WHITE ####
  Nonwhite_Housing_Choice_Table <- ACS_Housing_Choice %>% 
    group_by(group) %>%
    st_drop_geometry() %>% 
    summarize(HCV_Sum = sum(HCV_PUBLIC), Percent = sum(HCV_PUBLIC_pct))
  
  print(Nonwhite_Housing_Choice_Table %>%
          mutate(group = replace_na(group, "80-100%")) %>%  
          rename(
            `Percent Non-White` = group,
            `Number of HCV` = HCV_Sum,
            `Percent of Total HCV` = Percent) %>%
          gt() %>%
          custom_tabletheme())
  
  ### TABLE - HCV BY % INCOME GROUP ####
  HCV_Poverty_Table <- ACS_Housing_Choice %>% 
    group_by(Poverty_Group) %>% 
    filter(!is.na(Poverty_Rate)) %>%
    mutate(Poverty_Group = factor(Poverty_Group, 
                                  levels = c("Low Poverty (<10%)", 
                                             "Moderate Poverty (10-20%)", 
                                             "High Poverty (20-30%)", 
                                             "Very High Poverty (30%+)"))) %>% 
    st_drop_geometry() %>% 
    summarize(HCV_Sum = sum(HCV_PUBLIC), Percent = sum(HCV_PUBLIC_pct))
  
  print(HCV_Poverty_Table %>%
          rename(
            `Poverty Rate` = Poverty_Group,
            `Number of HCV` = HCV_Sum,
            `Percent of Total HCV` = Percent) %>%
          gt() %>%
          custom_tabletheme())
  
  #### TABLE - HCV BY CONCENTRATED POVERTY ####
  HCV_Poverty_Table_Concentrated <- ACS_Housing_Choice %>% 
    filter(!is.na(Poverty_Rate)) %>%
    mutate(Concentrated_Poverty_Group = ifelse(Poverty_Rate < 30, 
                                               "Non-Concentrated Poverty (<30%)", 
                                               "Concentrated Poverty (30%+)"),
           Concentrated_Poverty_Group = factor(Concentrated_Poverty_Group,
                                               levels = c("Non-Concentrated Poverty (<30%)",
                                                          "Concentrated Poverty (30%+)"))) %>%
    group_by(Concentrated_Poverty_Group) %>% 
    summarize(HCV_Sum = sum(HCV_PUBLIC), Percent = sum(HCV_PUBLIC_pct)) %>% 
    st_drop_geometry()
  
  print(HCV_Poverty_Table_Concentrated %>%
          rename(
            `Poverty Rate` = Concentrated_Poverty_Group,
            `Number of HCV` = HCV_Sum,
            `Percent of Total HCV` = Percent) %>%
          gt() %>%
          custom_tabletheme())
  
  #### MAP - HCV BUBBLE ####
  tracts_centroids <- ACS_Housing_Choice %>%
    st_centroid() %>%
    filter(HCV_PUBLIC > 0)  # Remove zeros for mapping
  
  Bubble_HCV <- ggplot() +
    # Base map with tract outlines
    geom_sf(data = ACS_Housing_Choice, fill = "gray90", color = "black", size = 0.3) +  
    # Bubble overlay at centroids with size and color mapping
    geom_sf(data = tracts_centroids, aes(size = HCV_PUBLIC, fill = HCV_PUBLIC), 
            alpha = 0.5, color = "black", shape = 21) +  # shape = 21 for filled bubbles
    scale_size(range = c(2, 6), name = "Quantity of Vouchers") +  
    scale_fill_viridis_c(option = "C", name = "Quantity of Vouchers") +  
    theme_classic() +
    labs(title = "Housing choice voucher distribution across census tracts",
         caption = 'Data Source: U.S. Department of Housing and Urban Development, U.S. Census Bureau. Calculations by FHC.') +
    theme(
      legend.position = "right",
      plot.caption.position = "plot",
      legend.text = element_text(size = 14),   
      legend.title = element_text(size = 16),
      plot.caption = element_text(hjust = .5),
      plot.title = element_text(size = 18, hjust = 0.5),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank())
  
  
  return(list(
    tables = list(
      HCV_Nonwhite = table1,
      HCV_Income = table2,
      HCV_Concentrated = table3),
    maps = list(
      Bubble_HCV_Map = Bubble_HCV)))
  
}


# HCV Function Results ----------------------------------------------------
HCV_Results <- HCV_Function(Housing_Choice_Tracts, CountyACS)

ggsave("Bubble_HCV_map.png", plot = HCV_Results$maps$Bubble_HCV_Map, width = 10, height = 8)



# Cuyahoga County Impervious Surface Assessment ---------------------------

    # This is the full df that contains LIHTC and demographic info
Full_Cuyahoga_LIHTC <- LIHTC_results$dataframes[[1]]

    # Load and project raster
Imp_surface <- terra::rast("./Data/Clipped_Impervious/Clipped_Impervious_Surface.tif") %>% 
  project("EPSG:3734")

    # Extract values with weights
values <- terra::extract(Imp_surface, Full_Cuyahoga_LIHTC, weights = TRUE)

    # Weighted average impervious surface per tract
df_impervious <- values %>% 
  group_by(ID) %>%
  summarise(
    avg_impervious = weighted.mean(Clipped_Impervious_Surface, weight, na.rm = TRUE))

    # Join results back to the full df
Full_Cuyahoga_LIHTC <- Full_Cuyahoga_LIHTC %>% 
  tidyterra::mutate(simpleid = seq(1:nrow(.))) %>%
  left_join(df_impervious, by = c("simpleid" = "ID"))



# Cuyahoga County Tree Cover Assessment -----------------------------------

    # Load in tree cover raster from NLCD
tree_cover_rast <- terra::rast("./Data/Tree_Cover.tiff") %>% 
  project("EPSG:3734")

    # Extract tree cover values for each tract
values <- terra::extract(tree_cover_rast, Full_Cuyahoga_LIHTC, weights = TRUE)

    # Calculate average tree cover per tract
df_treecover <- values %>% 
  group_by(ID) %>%
  summarise(
    avg_treecover = weighted.mean(Layer_1, weight, na.rm = TRUE))

    # Join the results back to the tract data
Full_Cuyahoga_LIHTC <- Full_Cuyahoga_LIHTC %>% 
  left_join(df_treecover, by = c("simpleid" = "ID"))



# Chart Surface cover by % non-white --------------------------------------

    # Unused work to create a plot of average impervious and tree cover by poverty
    # Was not an effective visual
Landcover_summary <- Full_Cuyahoga_LIHTC %>% 
  group_by(Poverty_Group) %>% 
  summarize(Avg.Impervious = mean(avg_impervious), Avg.Treecover = mean(avg_treecover), Percent = sum(LIHTC_Pct)) %>% 
  st_drop_geometry()

Landcover_summary_long <- Landcover_summary %>%
  pivot_longer(
    cols = c(Avg.Treecover, Percent),
    names_to = "cover_type",
    values_to = "average_cover") %>% 
  drop_na()

my_colors <- c(
  "Impervious Surface Cover" = "#4B9CD3", 
  "Percent of LIHTC Properties" = "#E07B91")

ggplot(Landcover_summary_long, aes(x = Poverty_Group, y = average_cover, fill = cover_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = my_colors) +
  labs(
    x = NULL,
    y = "Percent (%)",
    fill = NULL) +
  geom_text(aes(label = round(average_cover, 1)),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3.5) + 
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 20, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank())


# Impervious surface map and analysis -------------------------------------

      # Pull in redlining data and filter out unnecessary polygons
Redlining <- read_sf("./Data/mappinginequality.gpkg") %>% 
  filter(city == "Cleveland") %>% 
  filter(fill != "#000000") %>% 
  st_transform("EPSG:4326")
  
      # Create map
ggplot() +
    # Base layer: impervious surface
  geom_sf(data = Full_Cuyahoga_LIHTC, aes(fill = avg_impervious), color = NA) +
    # Overlay: redlining polygons (only grades C and D)
  geom_sf(
    data = Redlining %>% filter(grade %in% c( "D")),
    fill = "grey40",
    color = NA,  
    alpha = 0.5) +
    # Color scale for impervious surface
  scale_fill_viridis_c(
    option = "plasma",
    name = "Impervious Surface (%)") +
  labs(
    title = "Impervious Surface Cover and Historical Redlining (Grade D)",
    caption = "Data Source: NLCD, University of Richmond. Calculations by FHC.") +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank())


    # Pearson Correlation analysis LIHTC and impervious surfaces
cor_result_LIHTCImp <- cor.test(Full_Cuyahoga_LIHTC$LIHTC_Cnt, Full_Cuyahoga_LIHTC$avg_impervious, 
                       method = "pearson")

print(cor_result_LIHTCImp)

    # Pearson Correlation analysis LIHTC and impervious surfaces
cor_result_LIHTCpov <- cor.test(Full_Cuyahoga_LIHTC$Poverty_Rate, Full_Cuyahoga_LIHTC$avg_impervious, 
                       method = "pearson")

print(cor_result_LIHTCpov)



