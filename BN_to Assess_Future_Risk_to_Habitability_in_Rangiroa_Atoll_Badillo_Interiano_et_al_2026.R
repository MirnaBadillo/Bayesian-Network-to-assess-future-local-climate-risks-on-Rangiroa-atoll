#---------------------------------------------------------------------------------------
# Bayesian network to assess future risk to habitability on Rangiroa Atoll
#
# Analyses reported in the manuscript. Each section corresponds to one of the research
# questions. All queries were performed using the Likelihood Weighting method (bnlearn).
#
# --------------------------------------------------------------------------------------

library(bnlearn)
library(bnviewer)
library(ggplot2)
library(dplyr)

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("1_Path_config.R")
source(paste0(base_path, "Queries_BN_LW_method_Rangiroa.R"))

#----INPUT DATA----------------------------------

Islands <- as.character(c(1:10, 12))
Risk_levels <- c(0,1,2,3)
Extreme_events_levels <- c("No_extreme_event", "Cyclone", "Distant_swell")
SSP_scenarios <- c("SSP1-2.6","SSP2-4.5", "SSP3-7.0","SSP5-8.5")
SSP_Baseline_scenarios <- c("Baseline-Sce","SSP1-2.6","SSP2-4.5", "SSP3-7.0","SSP5-8.5")
Local_development_scenarios <- c("Local_Baseline","Bussiness_as_usual","Resilient_dev")
Global_warming_levels <- c("1-1.5°C","1.5-2.0°C","2.0-3.0°C","3.0-4.0°C",">4.0°C")
Regional_sea_level_rise_levels <- c("<0.20m","0.20-0.50m","0.50-1.0m","1.0-1.5m")
Island_Robustness_levels <- c("Very low","Low","Moderate","High")
Risk_to_Habitability_levels <- c(0:15) 

#----CONDITIONAL PROBABILITY TABLES -------------

Matrix_GWL <- read.csv(paste0(base_path, "CPT_BN_Rangiroa/CPT_GWL_Rangiroa.csv"))
Matrix_RSLR <- read.csv(paste0(base_path, "CPT_BN_Rangiroa/CPT_RSLR_Rangiroa.csv"))
Matrix_Reefs <- read.csv(paste0(base_path, "CPT_BN_Rangiroa/CPT_Reefs_Rangiroa.csv"))
Matrix_Island_Robustness <- read.csv(paste0(base_path, "CPT_BN_Rangiroa/CPT_Island_Robustness.csv"))
Matrix_Imports_30 <- read.csv(paste0(base_path, "CPT_BN_Rangiroa/CPT_Imports_Rangiroa.csv"))
Matrix_Habitable_land <- read.csv(paste0(base_path, "CPT_BN_Rangiroa/CPT_Habitable_land_Rangiroa.csv"))
Matrix_Food_supply <- read.csv(paste0(base_path, "CPT_BN_Rangiroa/CPT_Food_supply_Rangiroa.csv"))
Matrix_Freshwater_supply <- read.csv(paste0(base_path, "CPT_BN_Rangiroa/CPT_Freshwater_supply_Rangiroa.csv"))
Matrix_Settlements_and_infrastructure <- read.csv(paste0(base_path, "CPT_BN_Rangiroa/CPT_Settlements_and_infrastructure_Rangiroa.csv"))
Matrix_Economic_activities <- read.csv(paste0(base_path, "CPT_BN_Rangiroa/CPT_Economic_activities_Rangiroa.csv"))
Matrix_Risk_to_Habitability <- Risk_to_Habitability_array(Risk_to_Habitability_levels, Risk_levels)

#----BAYESIAN NETWORK DEVELOPMENT-----------------------------------------------------------------

# Bayesian Network structure
net = model2network("[Island][Island_Robustness|Island][SSP_Y][Time][Local_context][Extreme_event][GWL|SSP_Y:Time][RSLR|SSP_Y:Time][Imports|GWL:SSP_Y:Local_context][Reefs|GWL][Food_supply|Reefs:Imports:Extreme_event][Habitable_land|RSLR:Island_Robustness:Extreme_event][Freshwater_supply|Habitable_land:Imports:Extreme_event][Settlements_and_infrastructure|Habitable_land:Extreme_event:Island][Economic_activities|Habitable_land:Reefs:Extreme_event][Risk_to_Habitability|Habitable_land:Freshwater_supply:Food_supply:Settlements_and_infrastructure:Economic_activities]")
print(net)
# Interactive network
viewer(net,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_with_sugiyama",
       bayesianNetwork.title = "<br> Discrete Bayesian Network to assess risks for future atoll habitability",
       bayesianNetwork.subtitle = "Rangiroa - French Polynesia",
       node.colors = list(background = "white",
                          border = "black"),
       node.font = list(color = "black", face="Arial"),
       clusters.legend.title = list(text = "<b>Legend</b> <br> Variable Categories",
                                    style = "font-size:18px;
                                             font-family:Arial;
                                             color:black;
                                             text-align:center;"))

# Definition of the Conditional Probability Tables

# Node Island
cptIsland <- matrix(rep(1/11, 11),
                    ncol = 1,
                    dimnames = list(Island = c("1","2","3","4","5","6","7","8","9","10","12")))

# Node Island Robustness
cptIsland_Robustness <- array(as.numeric(Matrix_Island_Robustness[[1]]),
                              dim = c(4,11),
                              dimnames = list(Island_Robustness = c("Very low","Low","Moderate","High"),
                                              Island = c("1","2","3","4","5","6","7","8","9","10","12")))

# Node SSP scenarios
cptSSP_Y = matrix(c(0.2,0.2,0.2,0.2,0.2), ncol = 1, dimnames = list(SSP_Y= c("Baseline-Sce","SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5")))

# Node Time
cptTime <- matrix(c(1/3,1/3,1/3), ncol = 1, byrow = TRUE, dimnames = list(Time = c(2025,2050,2100)))

# Node Extreme_event
cptExtreme_event <- matrix(c(0.333,0.333,0.333), ncol = 1, byrow = TRUE, dimnames = list(Extreme_event = c("No_extreme_event","Cyclone", "Distant_swell")))

# Node Global Warming Level conditional on SSP-Y and Time
cptGWL <- array(as.numeric(Matrix_GWL[[1]]),
                dim = c(5,5,3),
                dimnames = list(GWL = c("1-1.5°C","1.5-2.0°C","2.0-3.0°C","3.0-4.0°C",">4.0°C"),
                                SSP_Y = c("Baseline-Sce","SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5"),
                                Time = c(2025,2050,2100)))

# Node Regional Sea Level Rise conditional on SSP-Y and Time
cptRSLR <- array(as.numeric(Matrix_RSLR[[1]]),
                 dim = c(4,5,3),
                 dimnames = list(RSLR = c("<0.20m","0.20-0.50m","0.50-1.0m","1.0-1.5m"),
                                 SSP_Y = c("Baseline-Sce","SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5"),
                                 Time = c(2025,2050,2100)))

# Node Local context

cptLocal_context <- matrix(c(0.333,0.333,0.333), ncol = 1, byrow = TRUE, dimnames = list(Local_context = c("Local_Baseline","Bussiness_as_usual","Resilient_dev")))


# Node Imports conditional on SSP-Y 
cptImports <- array(as.numeric(Matrix_Imports_30[[1]]),
                    dim = c(4,5,3,5),
                    dimnames = list(Imports = c(0,1,2,3),
                                    GWL = c("1-1.5°C","1.5-2.0°C","2.0-3.0°C","3.0-4.0°C",">4.0°C"),
                                    Local_context = c("Local_Baseline","Bussiness_as_usual","Resilient_dev"),
                                    SSP_Y = c("Baseline-Sce","SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5")))

# Node Reefs conditional on Global Warming Level 
cptReefs <- array(as.numeric(Matrix_Reefs[[1]]),
                  dim = c(4,5),
                  dimnames = list(Reefs = c(0,1,2,3),
                                  GWL = c("1-1.5°C","1.5-2.0°C","2.0-3.0°C","3.0-4.0°C",">4.0°C")))

# Node Risk to habitable land conditional regional sea level rise, island robustness and extreme events
cptHabitable_land <- array(as.numeric(Matrix_Habitable_land[[1]]),
                           dim = c(4,4,4,3),
                           dimnames = list(Habitable_land = c(0,1,2,3),
                                           RSLR = c("<0.20m","0.20-0.50m","0.50-1.0m","1.0-1.5m"),
                                           Island_Robustness = c("Very low","Low","Moderate","High"),
                                           Extreme_event = c("No_extreme_event","Cyclone","Distant_swell")))

# Node Risk to settlements and infrastructure conditional on land, settlements exposure and extreme events
cptSettlements_and_infrastructure <- array(as.numeric(Matrix_Settlements_and_infrastructure[[1]]),
                                           dim = c(4,4,11,3),
                                           dimnames = list(Settlements_and_infrastructure = c(0,1,2,3),
                                                           Habitable_land = c(0,1,2,3),
                                                           Island = c(1,2,3,4,5,6,7,8,9,10,12),
                                                           Extreme_event = c("No_extreme_event","Cyclone","Distant_swell")))

# Node Risk to food supply conditional on reefs, imports and extreme events
cptFood_supply <- array(as.numeric(Matrix_Food_supply[[1]]),
                        dim = c(4,4,4,3),
                        dimnames = list(Food_supply = c(0,1,2,3),
                                        Reefs = c(0,1,2,3),
                                        Imports = c(0,1,2,3),
                                        Extreme_event = c("No_extreme_event","Cyclone","Distant_swell")))


# Node Risk to freshwater supply conditional on land, imports and extreme events
cptFreshwater_supply <- array(as.numeric(Matrix_Freshwater_supply[[1]]),
                              dim = c(4,4,4,3),
                              dimnames = list(Freshwater_supply = c(0,1,2,3),
                                              Habitable_land = c(0,1,2,3),
                                              Imports = c(0,1,2,3),
                                              Extreme_event = c("No_extreme_event","Cyclone","Distant_swell")))

# Node Risk to economic activities conditional on habitable land, reefs, and extreme events
cptEconomic_activities <- array(as.numeric(Matrix_Economic_activities[[1]]),
                                dim = c(4,4,4,3),
                                dimnames = list(Economic_activities = c(0,1,2,3),
                                                Habitable_land = c(0,1,2,3),
                                                Reefs = c(0,1,2,3),
                                                Extreme_event = c("No_extreme_event","Cyclone","Distant_swell")))

# Node Risk to habitability conditional on the five habitability pillars
cptRisk_to_Habitability <- array(c(Matrix_Risk_to_Habitability),
                                       dim = c(16,4,4,4,4,4),
                                       dimnames = list(Risk_to_Habitability = c(0:15),
                                                       Habitable_land = c(0,1,2,3),
                                                       Freshwater_supply = c(0,1,2,3),
                                                       Food_supply = c(0,1,2,3),
                                                       Settlements_and_infrastructure = c(0,1,2,3),
                                                       Economic_activities = c(0,1,2,3)))

# BN CUSTOM
dfit = custom.fit(net, dist = list(Island = cptIsland,
                                   SSP_Y = cptSSP_Y,
                                   Island_Robustness = cptIsland_Robustness,
                                   Time = cptTime,
                                   GWL = cptGWL,
                                   RSLR = cptRSLR,
                                   Reefs = cptReefs,
                                   Local_context = cptLocal_context, 
                                   Imports = cptImports,
                                   Extreme_event = cptExtreme_event,
                                   Food_supply = cptFood_supply,
                                   Freshwater_supply = cptFreshwater_supply,
                                   Habitable_land = cptHabitable_land,
                                   Settlements_and_infrastructure = cptSettlements_and_infrastructure,
                                   Economic_activities = cptEconomic_activities,
                                   Risk_to_Habitability = cptRisk_to_Habitability))



#----BN INFERENCE---------------

#----------------------------------------------------------------------------------
# 1. Risk and uncertainty across islands and scenarios
#    Manuscript: Figure 3
#----------------------------------------------------------------------------------

# Query the five pillars for every island, under each scenario

results_2100 <- lapply(SSP_scenarios, function(ssp){
  
  Query_all_pillars_all_islands(
    list(Time = "2100", SSP_Y = ssp, Extreme_event = "No_extreme_event"), dfit)
})

names(results_2100) <- SSP_scenarios


# Expected mean risk and entropy per pillar, then aggregated per island
global_2100 <- bind_rows(lapply(SSP_scenarios, function(ssp) {
  
  results_2100[[ssp]] %>%
    Compute_posterior_stats() %>%
    Aggregate_global_indices () %>%
    mutate(Scenario = ssp)
}))


global_2100$Year <- "2100"

Fig_global_index_scenarios_2100 <- Plot_Global_index_all_scenarios(global_2100, "2100")

#----------------------------------------------------------------------------------
# 2. Which habitability pillars are most threatened
#    Manuscript: Figure 4.
#----------------------------------------------------------------------------------

evidence_pillars <- list(Time = "2100", SSP_Y = "SSP5-8.5", Extreme_event = "No_extreme_event", Local_context = "Bussiness_as_usual")

results_pillars <- Query_all_pillars_all_islands(evidence_pillars, dfit)

global_pillars <- results_pillars %>%
  Compute_posterior_stats() %>%
  Aggregate_global_indices()

Fig_radar_SSP5_2100 <- Plot_query_function_all_Pillars_given_SSP_Time_Extreme_event_all_islands_Index_LC(results_pillars, 2100, "SSP5-8.5", "No_extreme_event", "Bussiness_as_usual", dfit, global_pillars)

#----------------------------------------------------------------------------------
# 3. Conditions under which Rangiroa may become uninhabitable 
#   Inverse queries: P(Driver | at least one pillar at very high risk)
#   Manuscript: Figure 5
#----------------------------------------------------------------------------------

drivers <- list(
  Reefs = Risk_levels,
  Imports = Risk_levels,
  RSLR = Regional_sea_level_rise_levels,
  GWL = Global_warming_levels,
  SSP_Y = SSP_Baseline_scenarios,
  Island_Robustness = Island_Robustness_levels
)

results_drivers <- lapply(names(drivers), function(node) {
  
  Query_function_DRIVERS_SEVERE_RISK_OR_LW_CUSTOM(
    node, drivers[[node]], 3, 3, 3, 3, 3, dfit
  )
})

names(results_drivers) <- names(drivers)

Fig_drivers_severe_risks <- plot_drivers(results_drivers)

