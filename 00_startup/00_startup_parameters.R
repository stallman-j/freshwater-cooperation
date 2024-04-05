# ______________________________#
# Environment: Parameters
# 
# Stallman
# Started 2023-04-16
# Last edited: 
#
# ______________________________#

# Parameters ----

  # Catchment buffer size
  buffer_size <- 10000 #  meters
  
  # years included in the irrigated farmland raster
  #year <- c(2018:2019)
  
  # projected crs
  
  #crs_for_calcs <- 7801 # just because
  
  # which continent we working with?
  
  continent_name = "Africa"
  
include_themes <- c(
  "WATER_SECURITY", 
  #"ENV_WATERWAYS", # mostly related to transportation or things happening on rivers, not water quantity
  "ENV_HYDRO",
  #"WB_137_WATER", # way too broad
  "WB_138_WATER_SUPPLY",
  "WB_140_AGRICULTURAL_WATER_MANAGEMENT",
  "WB_141_WATER_RESOURCES_MANAGEMENT",
  "WB_142_ENERGY_AND_WATER", # really small
  "WB_143_RURAL_WATER",
  "WB_144_URBAN_WATER",
  "WB_155_WATERSHED_MANAGEMENT",
  "WB_156_GROUNDWATER_MANAGEMENT",
  #"WB_157_ENVIRONMENTAL_WATER_USE_AND_CATCHMENT_PROTECTION", 0 hits from 2016 to 2022 in Africa
  "WB_161_DAMS_AND_RESERVOIRS",
  "WB_159_TRANSBOUNDARY_WATER",
  "WB_423_INTEGRATED_URBAN_WATER_MANAGEMENT",
  "WB_427_WATER_ALLOCATION_AND_WATER_ECONOMICS",
  #"WB_525_RENEWABLE_ENERGY", too big and only interested in hydropower anyways
  "WB_527_HYDROPOWER",
  "WB_988_LEVEES",
  "WB_1000_WATER_MANAGEMENT_STRUCTURES",
  "WB_1002_IRRIGATION_WATER_QUALITY",
  "WB_1064_WATER_DEMAND_MANAGEMENT",
  "WB_1021_WATER_LAW",
  "WB_1063_WATER_ALLOCATION_AND_WATER_SUPPLY",
  "WB_1199_WATER_SUPPLY_AND_SANITATION",
  "WB_1220_SURFACE_WATER_MANAGEMENT",
  "WB_1729_URBAN_WATER_FINANCIAL_SUSTAINABILITY",
  "WB_1731_NON_REVENUE_WATER",
  "WB_1778_FRESHWATER_ECOSYSTEMS",
  "WB_1790_INTERNATIONAL_WATERWAYS",
  "WB_1805_WATERWAYS",
  "WB_1941_WATERBORNE_TRANSPORT",
  "WB_1998_WATER_ECONOMICS",
  "WB_2005_COMMUNITY_WATER_SUPPLY_MANAGEMENT",
  "WB_2007_WATER_SAFETY_PLANS", # 
  "WB_2971_WATER_PRICING",
  "WB_2972_GROUNDWATER_CONJUNCTIVE_USE"
  #"WB_2992_FRESHWATER_FISHERIES" # really does just catch fishing related
  #"UNGP_FORESTS_RIVERS_OCEANS", too broad
)