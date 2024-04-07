# _______________________________#
# Environment
# Clean 02: Log GDELT GKG: Relevant Themes and Countries List
# 
# Stallman
# Started 2023-05-29
# Last edited: 
#________________________________#

if (!require("tmap")) install.packages("tmap")
if (!require("sf")) install.packages("sf")
if (!require("dplyr")) install.packages("dplyr")
if (!require("countrycode")) install.packages("countrycode")
if (!require("devtools")) install.packages("devtools")
if (!require("gdeltr2")) install_github("abresler/gdeltr2")


library(tidyverse)
library(gdeltr2)
library(stringi) # for efficient string ops
library(countrycode)

countries <- codelist %>%
  filter(continent == "Africa") %>%
  select(fips)

countries_full <- codelist %>%
  filter(continent == "Africa")

# get the countries list

dput(countries)

countries <- c("AG", "AO", "BN", "BC", "UV", "BY", "CM", 
               "CV", "CT", "CD", "CN", "CF", "CG", "IV", "DJ", "EG", "EK", "ER", 
               "WZ", "ET", "GB", "GA", "GH", "GV", "PU", "KE", "LT", "LI", "LY", 
               "MA", "MI", "ML", "MR", "MP", "MF", "MO", "MZ", "WA", "NG", "NI", 
               "RW", "RE", "SG", "SE", "SL", "SO", "SF", "SH", "SU", 
               "TP", "TZ", "TO", "TS", "UG", "WI", "ZA", "ZI")

# see here:
# https://rpubs.com/BrendanKnapp/GDELT_Syrian_Conflict



water_natural_disaster_themes <- c(
  "NATURAL_DISASTER_WATER_LEVEL",
  "NATURAL_DISASTER_FLOODWATERS",
  "NATURAL_DISASTER_HIGH_WATER",
  "NATURAL_DISASTER_FLOOD_WATERS",
  "NATURAL_DISASTER_FLOODWATER",
  "NATURAL_DISASTER_FLOOD_WATER",
  "NATURAL_DISASTER_HIGH_WATERS"
)

water_management_themes <- c(
  "WATER_SECURITY",
  "ENV_WATERWAYS",
  "WB_1199_WATER_SUPPLY_AND_SANITATION",
  "WB_140_AGRICULTURAL_WATER_MANAGEMENT",
  "WB_141_WATER_RESOURCES_MANAGEMENT",
  "WB_1000_WATER_MANAGEMENT_STRUCTURES",
  "WB_138_WATER_SUPPLY",
  "WB_1805_WATERWAYS",
  "WB_427_WATER_ALLOCATION_AND_WATER_ECONOMICS",
  "WB_1064_WATER_DEMAND_MANAGEMENT",
  "WB_159_TRANSBOUNDARY_WATER",
  "WB_1021_WATER_LAW",
  "WB_144_URBAN_WATER",
  "WB_1063_WATER_ALLOCATION_AND_WATER_SUPPLY",
  "WB_143_RURAL_WATER",
  "WB_1998_WATER_ECONOMICS",
  "WB_2971_WATER_PRICING",
  "WB_1778_FRESHWATER_ECOSYSTEMS",
  "WB_155_WATERSHED_MANAGEMENT",
  "WB_156_GROUNDWATER_MANAGEMENT",
  "WB_1790_INTERNATIONAL_WATERWAYS",
  "WB_2005_COMMUNITY_WATER_SUPPLY_MANAGEMENT",
  "WB_2992_FRESHWATER_FISHERIES",
  "WB_1729_URBAN_WATER_FINANCIAL_SUSTAINABILITY",
  "WB_1731_NON_REVENUE_WATER",
  "WB_2007_WATER_SAFETY_PLANS",
  "WB_1220_SURFACE_WATER_MANAGEMENT",
  "WB_1941_WATERBORNE_TRANSPORT",
  "WB_1002_IRRIGATION_WATER_QUALITY",
  "WB_423_INTEGRATED_URBAN_WATER_MANAGEMENT",
  "WB_157_ENVIRONMENTAL_WATER_USE_AND_CATCHMENT_PROTECTION",
  "WB_2972_GROUNDWATER_CONJUNCTIVE_USE",
  "WB_142_ENERGY_AND_WATER",
  "WB_525_RENEWABLE_ENERGY",
  "WB_527_HYDROPOWER",
  "UNGP_FORESTS_RIVERS_OCEANS" # generates a lot of hits 
)


water_quality_themes <- c("CRISISLEX_C06_WATER_SANITATION",
                          "UNGP_CLEAN_WATER_SANITATION",
                          "WB_1462_WATER_SANITATION_AND_HYGIENE",
                          "WB_139_SANITATION_AND_WASTEWATER",
                          "WB_1199_WATER_SUPPLY_AND_SANITATION",
                          "WB_2008_WATER_TREATMENT",
                          "TAX_AIDGROUPS_SINGLE_DROP_FOR_SAFE_WATER",
                          "WB_2009_WATER_QUALITY_MONITORING",
                          "WB_1798_WATER_POLLUTION",
                          "WB_1215_WATER_QUALITY_STANDARDS",
                          "WB_2981_DRINKING_WATER_QUALITY_STANDARDS",
                          "WB_149_WASTEWATER_TREATMENT_AND_DISPOSAL",
                          "WB_150_WASTEWATER_REUSE",
                          "WB_3014_WASTEWATER_DISPOSAL_FACILITIES",
                          "WB_2978_WATER_POLLUTION_LOAD",
                          "WB_991_DRAINAGE_WATER_QUALITY"
)

energy_themes  <- c("WB_1995_ENERGY_FOR_WATER_AND_WASTEWATER_TREATMENT",
                    "WB_1996_ENERGY_RECOVERY_FROM_WASTEWATER",
                    "WB_1197_ENERGY_EFFICIENCY_IN_WATER_AND_WASTEWATER_UTILITIES",
                    "WB_142_ENERGY_AND_WATER",
                    "WB_525_RENEWABLE_ENERGY",
                    "WB_527_HYDROPOWER",
                    #"WB_507_ENERGY_AND_EXTRACTIVES", # accounts for about 300 in 2017 01 01 
                    "ECON_ELECTRICALGENERATION",
                    "ECON_ELECTRICALDEMAND",
                    "ECON_ELECTRICALLOADSHEDDING",
                    "ECON_ELECTRICALPRICE"
                    #"WB_1751_LIQUEFIED_NATURAL_GAS",
                    #"WB_549_OIL_AND_GAS_SYSTEMS"
)

other_water_themes <- c("WB_137_WATER",
                        "TAX_AIDGROUPS_WATERAID",
                        "TAX_AIDGROUPS_MILLENNIUM_WATER_ALLIANCE")




# 200 themes contain "water" in them; most of them are just related to words with "water" 

exclude_themes_list <- c("TAX_WEAPONS_WATER_CANNONS",
                         "TAX_WEAPONS_WATER_CANNON",
                         "TAX_WORLDBIRDS_WATERFOWL",
                         "HUMAN_RIGHTS_ABUSES_WATERBOARDING",
                         "TAX_FNCACT_WATERMAN",
                         "HUMAN_RIGHTS_ABUSES_WATERBOARDED",
                         "TAX_WORLDMAMMALS_WATER_BUFFALO",
                         "HUMAN_RIGHTS_ABUSES_WATERBOARD",
                         "TAX_FNCACT_WATER_DOG",
                         "TAX_FNCACT_WATERMEN",
                         "TAX_FNCACT_WATERBOY",
                         "TAX_FNCACT_WATER_DOGS",
                         "TAX_WORLDMAMMALS_WATER_BUFFALOS",
                         "TAX_FNCACT_WATER_BOY",
                         "TAX_FNCACT_WATERBOYS",
                         "TAX_FNCACT_WATER_BOYS",
                         "TAX_WORLDFISH_FRESHWATER_EEL",
                         "TAX_WORLDBIRDS_WATER_RAIL",
                         "TAX_WORLDMAMMALS_WATER_BUFFALOS",
                         "TAX_WORLDMAMMALS_WATERBUCK",
                         "TAX_FNCACT_WATERMANS",
                         "TAX_DISEASE_REDWATER",
                         "TAX_WORLDBIRDS_MANX_SHEARWATERS",
                         "TAX_WORLDBIRDS_MANX_SHEARWATER",
                         "HUMAN_RIGHTS_ABUSES_WATERBOARDS",
                         "TAX_WORLDBIRDS_SHORTTAILED_SHEARWATERS",
                         "TAX_WORLDBIRDS_WATERFOWLS",
                         "TAX_WORLDBIRDS_WEDGETAILED_SHEARWATERS",
                         "TAX_WORLDBIRDS_SHORTTAILED_SHEARWATER",
                         "TAX_WORLDMAMMALS_CHINESE_WATER_DEER",
                         "TAX_WORLDMAMMALS_WATERBUCKS",
                         "TAX_WORLDBIRDS_LOUISIANA_WATERTHRUSH",
                         "TAX_WORLDREPTILES_NORTHERN_WATER_SNAKE",
                         "TAX_WORLDBIRDS_SOOTY_SHEARWATER",
                         "TAX_WORLDBIRDS_NORTHERN_WATERTHRUSH",
                         "TAX_DISEASE_WATERMELON_STOMACH",
                         "TAX_WORLDFISH_FRESHWATER_EELS",
                         "TAX_WORLDBIRDS_WATER_RAILS",
                         "TAX_WORLDREPTILES_NORTHERN_WATER_SNAKES",
                         "TAX_WORLDBIRDS_HUTTON_SHEARWATER",
                         "TAX_WORLDBIRDS_GREAT_SHEARWATERS",
                         "TAX_WORLDBIRDS_WATER_PIPITS",
                         "TAX_WORLDMAMMALS_WATER_CHEVROTAIN",
                         "TAX_WORLDBIRDS_WEDGETAILED_SHEARWATER",
                         "TAX_WORLDREPTILES_DIAMONDBACK_WATER_SNAKE",
                         "TAX_WORLDBIRDS_FLESHFOOTED_SHEARWATER",
                         "TAX_WORLDBIRDS_WATER_PIPIT",
                         "TAX_WORLDBIRDS_CORY_SHEARWATERS",
                         "TAX_WORLDFISH_FRESHWATER_SHARKS",
                         "TAX_WORLDBIRDS_GREAT_SHEARWATER",
                         "TAX_WORLDFISH_TIDEWATER_GOBY",
                         "TAX_WORLDFISH_FRESHWATER_SHARK",
                         "TAX_WORLDBIRDS_FLESHFOOTED_SHEARWATERS",
                         "TAX_WORLDREPTILES_BANDED_WATER_SNAKE",
                         "TAX_DISEASE_BLACKWATER_FEVER",
                         "TAX_WORLDBIRDS_CORY_SHEARWATER",
                         "TAX_DISEASE_WATERING_EYE",
                         "TAX_WORLDBIRDS_STREAKED_SHEARWATER",
                         "TAX_WORLDBIRDS_WHITEBREASTED_WATERHEN",
                         "TAX_DISEASE_HEARTWATER",
                         "TAX_WORLDMAMMALS_WATER_OPOSSUM",
                         "TAX_WORLDREPTILES_BANDED_WATER_SNAKES",
                         "TAX_WORLDBIRDS_PINKFOOTED_SHEARWATER",
                         "TAX_WORLDBIRDS_HUTTON_SHEARWATERS",
                         "TAX_WORLDFISH_FRESHWATER_HERRING",
                         "TAX_WORLDBIRDS_FLUTTERING_SHEARWATERS",
                         "TAX_WORLDBIRDS_STREAKED_SHEARWATERS",
                         "TAX_WORLDBIRDS_PLUMBEOUS_WATER_REDSTART",
                         "TAX_WORLDBIRDS_FLUTTERING_SHEARWATER",
                         "TAX_WORLDBIRDS_WHITEBREASTED_WATERHENS",
                         "TAX_WORLDREPTILES_DIAMONDBACK_WATER_SNAKES",
                         "TAX_WORLDMAMMALS_EURASIAN_WATER_SHREW",
                         "TAX_WORLDBIRDS_BULLER_SHEARWATER",
                         "TAX_WORLDBIRDS_AUDUBON_SHEARWATERS",
                         "TAX_WORLDMAMMALS_EURASIAN_WATER_SHREWS",
                         "TAX_WORLDFISH_DEEPWATER_FLATHEAD",
                         "TAX_WORLDBIRDS_SHORT_TAILED_SHEARWATERS",
                         "TAX_WORLDBIRDS_WATERCOCK",
                         "TAX_WORLDBIRDS_AUDUBON_SHEARWATER",
                         "TAX_WORLDBIRDS_BULLER_SHEARWATERS",
                         "TAX_WORLDBIRDS_PINKFOOTED_SHEARWATERS",
                         "TAX_DISEASE_BLACK_WATER_FEVER",
                         "TAX_WORLDBIRDS_LITTLE_SHEARWATER",
                         "TAX_WORLDBIRDS_WHITE_BREASTED_WATERHEN",
                         "TAX_WORLDBIRDS_BLACKVENTED_SHEARWATER",
                         "TAX_WORLDBIRDS_TOWNSEND_SHEARWATER",
                         "TAX_DISEASE_HEARTWATER_DISEASE",
                         "TAX_WORLDBIRDS_MEDITERRANEAN_SHEARWATER",
                         "TAX_WORLDBIRDS_PLUMBEOUS_WATERREDSTART",
                         "TAX_WORLDBIRDS_WEDGE_TAILED_SHEARWATER",
                         "TAX_WORLDBIRDS_FLESH_FOOTED_SHEARWATER",
                         "TAX_WORLDBIRDS_WATERCOCKS",
                         "TAX_WORLDBIRDS_BLACKVENTED_SHEARWATERS",
                         "TAX_DISEASE_WATERELECTROLYTE_IMBALANCE",
                         "TAX_WORLDMAMMALS_JAPANESE_WATER_SHREW",
                         "TAX_WORLDBIRDS_WATERFALL_SWIFT",
                         "TAX_WORLDMAMMALS_AMERICAN_WATER_SHREW",
                         "TAX_WORLDBIRDS_FLESH_FOOTED_SHEARWATERS",
                         "TAX_WORLDBIRDS_LITTLE_SHEARWATERS",
                         "TAX_WORLDMAMMALS_ELEGANT_WATER_SHREW",
                         "TAX_WORLDFISH_NORTH_AMERICAN_FRESHWATER_CATFISH",
                         "TAX_WORLDMAMMALS_FALSE_WATER_RAT",
                         "TAX_WORLDMAMMALS_ELEGANT_WATER_SHREWS",
                         "TAX_WORLDBIRDS_MEDITERRANEAN_SHEARWATERS",
                         "TAX_WORLDFISH_DEEPWATER_STINGRAY",
                         "TAX_WORLDBIRDS_WHITE_CAPPED_WATER_REDSTART",
                         "TAX_WORLDBIRDS_WATER_THICKKNEE",
                         "TAX_WORLDBIRDS_LUZON_WATER_REDSTART",
                         "TAX_WORLDMAMMALS_WATER_OPOSSUMS",
                         "TAX_WORLDBIRDS_WATERFALL_SWIFTS",
                         "TAX_WORLDBIRDS_PIED_WATER_TYRANT",
                         "TAX_WORLDREPTILES_FLORIDA_BANDED_WATER_SNAKE",
                         "TAX_WORLDFISH_FRESHWATER_HATCHETFISH",
                         "TAX_WORLDBIRDS_PIED_WATER_TYRANTS",
                         "TAX_WORLDREPTILES_REDBELLY_WATER_SNAKE",
                         "TAX_WORLDREPTILES_REDBELLY_WATER_SNAKES",
                         "TAX_WORLDMAMMALS_WATERHOUSE_LEAFNOSED_BAT",
                         "TAX_WORLDBIRDS_WEDGE_TAILED_SHEARWATERS",
                         "TAX_WORLDMAMMALS_WATER_CHEVROTAINS",
                         "TAX_WORLDMAMMALS_COMMON_WATER_RAT",
                         "TAX_WORLDBIRDS_PLUMBEOUS_WATER_REDSTARTS",
                         "TAX_WORLDBIRDS_PERSIAN_SHEARWATER",
                         "TAX_WORLDMAMMALS_FALSE_WATER_RATS",
                         "TAX_WORLDBIRDS_SHORT_TAILED_SHEARWATER",
                         "TAX_DISEASE_WATERCLEAR_CELL_ADENOMA",
                         "TAX_WORLDBIRDS_WATER_THICKKNEES",
                         "TAX_WORLDBIRDS_WHITE_BREASTED_WATERHENS",
                         "TAX_WORLDBIRDS_NORTHERN_WATERTHRUSHS",
                         "TAX_WORLDBIRDS_WATER_THICK_KNEES",
                         "TAX_WORLDFISH_DEEPWATER_STINGRAYS",
                         "TAX_WORLDBIRDS_DRAB_WATER_TYRANT",
                         "TAX_WORLDBIRDS_PIED_WATERTYRANT",
                         "TAX_WORLDBIRDS_PERSIAN_SHEARWATERS",
                         "TAX_DISEASE_WATERBORNE_DISEASE",
                         "TAX_DISEASE_WATERBORNE_ILLNESS",
                         "TAX_DISEASE_WATER_INTOXICATION",
                         "TAX_AIDGROUPS_WATERAID_AMERICA",
                         "TAX_DISEASE_WATERBORNE_DISEASES"
                         
) 

themes_to_include <- c(#"UNGP_FORESTS_RIVERS_OCEANS",
  #"WB_1386_DAM_SAFETY" ,
  #"WB_2973_MAIN_CANALS"
)