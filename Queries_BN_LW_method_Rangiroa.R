#-----------------------------------------------------------------------------------------------------
# All the queries were performed using the BNLEARN package. We used Likelihood weighting (LW) method.
#-----------------------------------------------------------------------------------------------------

#   QUERY 1) Risk to each habitability pillar given an evidence set

Query_all_pillars_LW <- function(Risk_to_pillar_levels,evidence_list,dfit,n=1e6) {
  
  pillars <- c("Habitable_land", "Freshwater_supply", "Food_supply",
               "Settlements_and_infrastructure", "Economic_activities")
  
  evidences <- paste0(
    "list(",
    paste0(names(evidence_list), ' = "', unlist(evidence_list), '"', collapse = ", "),
    ")"
  )
  
  all_results <- list()
  
  for (Pillar in pillars) {
    result_LW <- data.frame(Risk_to_pillar = character(), Probability = numeric(), Island = numeric())
    
    for (j in seq_along(Risk_to_pillar_levels)) {
      
      event <- paste0("(", Pillar, "=='", Risk_to_pillar_levels[j], "')")
      
      query_val <- eval(parse(text = paste(
        "cpquery(fitted = dfit, event =", event,
        ", evidence =", evidences,
        ", method = 'lw', n =", n, ")"
      )))
      
      result_LW <- rbind(result_LW, data.frame(
        Risk_to_pillar = Risk_to_pillar_levels[j],
        Probability = query_val,
        Island = evidence_list$Island
      ))
    }
    
    result_LW$Pillar <- Pillar
    all_results[[Pillar]] <- result_LW
  }
  
  df_all <- do.call(rbind, all_results)
  
  return(df_all)
}


Query_all_pillars_all_islands <- function(evidence_list, dfit, n = 1e6) {
  
  islands <- c(1:10, 12)
  
  all_islands <- list()
  
  for (i in islands) {
    cat("Running Island:", i, "\n")
    
    ev <- c(list(Island = as.character(i)), evidence_list)
    
    all_islands[[as.character(i)]] <- Query_all_pillars_LW(
      Risk_to_pillar_levels = Risk_levels,
      evidence_list = ev,
      dfit = dfit,
      n = n
    )
  }
  
  do.call(rbind, all_islands)
}


# Compute_posterior_stats function estimate the mean risk and the entropy for each habitability pillar.
# Aggregate_global_indices computes the mean risk and uncertainty for each island, providing a global index.

Compute_posterior_stats <- function(df,
                                    risk_col = "Risk_to_pillar",
                                    prob_col = "Probability") {
  
  df$risk_numeric <- as.numeric(as.character(df[[risk_col]]))
  
  stats <- df %>%
    dplyr::group_by(Island, Pillar) %>%
    dplyr::summarise(
      Risk_mean = sum(risk_numeric * .data[[prob_col]]),
      Entropy = -sum(.data[[prob_col]] * log(.data[[prob_col]]+ 1e-12)),
      
      .groups = "drop"
    )
  
  stats$Risk_mean   <- round(stats$Risk_mean, 3)
  stats$Entropy     <- round(stats$Entropy, 3)
  
  return(stats)
}

Aggregate_global_indices <- function(stats_df) {
  
  global_stats <- stats_df %>%
    dplyr::group_by(Island) %>%
    dplyr::summarise(
      Global_Risk_Index = mean(Risk_mean),
      Global_Uncertainty_Index = mean(Entropy),
      .groups = "drop"
    )
  
  global_stats$Global_Risk_Index <- round(global_stats$Global_Risk_Index, 2)
  global_stats$Global_Uncertainty_Index <- round(global_stats$Global_Uncertainty_Index, 2)
  
  return(global_stats)
}


Plot_Global_index_all_scenarios <- function(global_index_df,year){
  Global_index_all_scenarios <- ggplot(
    global_index_df,
    aes(x = Global_Risk_Index,
        y = Global_Uncertainty_Index,
        fill = Scenario,    
        shape = Scenario,
        label = Island)
  ) +
    geom_point(size = 4,
               color = "black",   
               stroke = 1.2) +
  labs(title = paste0("Global Risk vs Uncertainty by Island"," - ",year),
       x = "Global Risk Index",
       y = "Global Uncertainty Index") +
    theme_minimal(base_size = 12) +
    geom_hline(yintercept = mean(global_index_df$Global_Uncertainty_Index, na.rm = TRUE),
               linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = mean(global_index_df$Global_Risk_Index, na.rm = TRUE),
               linetype = "dashed", color = "grey50") +
    scale_shape_manual(values = c(21, 22, 23, 24)) +
    scale_fill_manual(
      values = c("SSP1-2.6" = "#003466",
                 "SSP2-4.5" = "#f69320",
                 "SSP3-7.0" = "#df0000",
                 "SSP5-8.5" = "#980002")
    )+
    coord_cartesian(xlim = c(0, 2.0),
                    ylim = c(0, 1.38))
  plot(Global_index_all_scenarios)
}


#----------------------------------------------------------------------------------------
# Query 2) Risk to habitability pillars - Radar plot  

Plot_query_function_all_Pillars_given_SSP_Time_Extreme_event_all_islands_Index_LC <- 
  function(query_df, Time, SSP_Y, Extreme_event,LocalContext, dfit, global_index_df){
    
    set.seed(123)
    
    islands <- c(1:10, 12)
    
    pillars = c("Habitable_land", "Freshwater_supply", "Food_supply",
                "Settlements_and_infrastructure", "Economic_activities")
    pillars_labels = c("Land", "Freshwater", "Food", "Settl.", "Economic activities")
    
    circle_levels <- seq(0, 1, by = 0.2)
    circle_labels <- data.frame(
      y = seq(0, 1, by = 0.2),
      label = paste0(seq(0, 100, by = 20), "%")
    )
    
    divider_data <- data.frame(x = seq(0.5, length(pillars), by = 1))
    
    risk_colors <- c("#F7F7F7","#ffde21", "#d45c21ff", "#85215eff")
    
    radar_data <- query_df %>%
      select(Island, Pillar, Risk_to_pillar, Probability) %>%
      distinct()
    
    radar_data <- radar_data %>%
      arrange(Island, Pillar, Risk_to_pillar) %>%
      mutate(
        angle = as.numeric(factor(Pillar, levels = pillars)),
        xmin = angle - 0.10,
        xmax = angle + 0.10,
        ymin = 0,
        ymax = Probability
      )
    
    radar_data$Probability <- as.numeric(radar_data$Probability)
    radar_data$Risk_to_pillar <- factor(as.character(radar_data$Risk_to_pillar))
    
    radar_data <- radar_data %>%
      mutate(
        label_x = angle,
        label_y = ymax + 0.05,  
        prob_label = ifelse(Probability >= 0.01, sprintf("%.2f", Probability), "")
      )
    
    
    central_labels <- global_index_df %>%
      mutate(
        label = paste0(
          "RM: ", round(Global_Risk_Index, 2),
          "\nUnc: ", round(Global_Uncertainty_Index, 2)
        ),
        x = 3,    
        y = -1.00 
      )
    
    p <- ggplot() +
      
      geom_hline(data = data.frame(y = circle_levels),
                 aes(yintercept = y),
                 color = "gray") +
      
      geom_vline(data = divider_data,
                 aes(xintercept = x),
                 color = "grey60", linewidth = 0.4,
                 linetype = "dotted") +
      
      geom_rect(data = radar_data,
                aes(xmin = xmin, xmax = xmax,
                    ymin = ymin, ymax = ymax,
                    fill = Risk_to_pillar),
                color = "black",
                position = position_dodge2(preserve = "single")) +

      geom_text(data = circle_labels,
                aes(x = 0.1, y = y+0.01, label = label),
                color = "grey30", size = 3.0,
                hjust = -0.3, vjust=0.5) +
      
      geom_text(data = central_labels,
                aes(x = x, y = y, label = label),
                inherit.aes = FALSE,
                size = 3.0,
                fontface = "bold") +
      
      coord_polar() +
      
      scale_fill_manual(values = risk_colors, name = "Risk level") +
      
      scale_y_continuous(limits = c(-1, 1.1),
                         breaks = circle_labels$y,
                         labels = circle_labels$label,
                         expand = c(0, 0)) +
      
      scale_x_continuous(
        breaks = 1:length(pillars),
        labels = pillars_labels,
        position = "top"
      ) +
      
      facet_wrap(~Island, ncol = 4) +
      
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "gray12",
                                   face = "bold", size = 7),
        legend.position = "right",
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank()
      ) +
      
      labs(
        title = paste0("Risk to pillars given ", SSP_Y,
                       " and ", Time,
                       " and ", Extreme_event,
                       " and ", LocalContext
        ),
        subtitle = paste0("P[Pillar | ",
                          SSP_Y, ", ",
                          Time, ", ",
                          Extreme_event, "]")
      )
    
    print(p)
  }


# QUERY 3) Conditions under which Rangiroa may become uninhabitable 
#   Inverse queries: P(Driver | at least one pillar at very high risk)

Query_function_DRIVERS_SEVERE_RISK_OR_LW_CUSTOM <- function(node, node_levels,
                                                        Risk_to_land, Risk_to_food, Risk_to_freshwater,
                                                        Risk_to_settlements, Risk_to_econom, dfit){
  
  
  mutbn <- mutilated(dfit, list(Extreme_event = "No_extreme_event"))
  
  particles <- rbn(mutbn, 10^6)
  
  w <- logLik(dfit, particles, nodes = "Extreme_event", by.sample = TRUE)
  
  OR_condition <- particles$Habitable_land == as.character(Risk_to_land) |
    particles$Food_supply == as.character(Risk_to_food) |
    particles$Freshwater_supply == as.character(Risk_to_freshwater) |
    particles$Settlements_and_infrastructure == as.character(Risk_to_settlements) |
    particles$Economic_activities == as.character(Risk_to_econom)
  
  wE <- sum(exp(w[OR_condition]))
  
  results_LW <- data.frame(Risk_to_node = character(), Probability = numeric())
  
  for (j in seq_along(node_levels)) {
    
    Event_condition <- particles[[node]] == node_levels[j]
    
    wEQ <- sum(exp(w[OR_condition & Event_condition]))
    
    P_Q_given_E_LW <- wEQ/wE
    
    results_LW <- rbind(results_LW,
                        data.frame(Risk_to_node = node_levels[j],
                                   Probability  = P_Q_given_E_LW))
    
  }
  
  return(results_LW)
}



plot_drivers <- function(results_drivers){
  
  node_order <- c("SSP scenario", "Global warming level", "Regional sea-level rise",
                  "Reef condition", "Import system", "Island robustness")
  
  All_drivers <- bind_rows(
    results_drivers$Reefs %>%
      mutate(Risk_to_node = as.character(Risk_to_node),
             Node = "Reef condition"),
    
    results_drivers$Imports %>%
      mutate(Risk_to_node = as.character(Risk_to_node),
             Node = "Import system"),
    
    results_drivers$RSLR %>%
      mutate(Risk_to_node = as.character(Risk_to_node),
             Node = "Regional sea-level rise"),
    
    results_drivers$GWL %>%
      mutate(Risk_to_node = as.character(Risk_to_node),
             Node = "Global warming level"),
    
    results_drivers$SSP_Y %>%
      mutate(Risk_to_node = as.character(Risk_to_node),
             Node = "SSP scenario"),
    
    results_drivers$Island_Robustness %>%
      mutate(Risk_to_node = as.character(Risk_to_node),
             Node = "Island robustness"))
  
  
  All_drivers <- All_drivers %>%
    mutate(
      Node = factor(Node, levels = node_order)
    ) %>%
    group_by(Node) %>%
    mutate(
      Risk_to_node = factor(
        Risk_to_node,
        levels = unique(Risk_to_node)
      )
    ) %>%
    ungroup()
  
  
  Figure_5 <- ggplot(All_drivers,aes(x = Risk_to_node, y = Probability)) +
    
    geom_col(fill = "#00798c",color = "black",width = 0.7) +
    facet_wrap(~ Node, ncol = 3,scales = "free_x") +
    scale_y_continuous(limits = c(0, 1),breaks = seq(0, 1, 0.25),expand = expansion(mult = c(0, 0.03))) +
    labs(x = NULL,y = "Probability") +
    theme_bw(base_size = 12) +
    theme(strip.text = element_text(face = "bold", size = 12),
          axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
          panel.grid.minor = element_blank(),
          legend.position = "none")
  
  plot(Figure_5)
}


#------------------------------------------------------------------------------------------------------
# The function allow to create the condtional probability table for the global risk to habitability
#------------------------------------------------------------------------------------------------------

Risk_to_Habitability_array <- function(Risk_to_Habitability_levels,Risk_levels){
  
  Risk_to_Land_levels <- Risk_levels
  Risk_to_freshwater_levels <- Risk_levels
  Risk_to_Foodsupply_levels <- Risk_levels
  Risk_to_Settlement_levels <- Risk_levels
  Risk_to_Economic_opportunities_levels <- Risk_levels
  
  dim_array <- c(length(Risk_to_Habitability_levels), length(Risk_to_Land_levels), length(Risk_to_freshwater_levels),length(Risk_to_Foodsupply_levels),length(Risk_to_Settlement_levels), length(Risk_to_Economic_opportunities_levels))
  
  Risk_to_Habitability_array <- array(0,dim = dim_array)
  
  for (i in 1:length(Risk_to_Habitability_levels)) {
    for (j in 1:length(Risk_to_Land_levels)) {
      for (k in 1:length(Risk_to_freshwater_levels)) {
        for (l in 1:length(Risk_to_Foodsupply_levels)) {
          for (m in 1:length(Risk_to_Settlement_levels)) {
            for (n in 1:length(Risk_to_Economic_opportunities_levels)) {
              
              Risk_to_Habitability_value <- i - 1
              Risk_to_land_value <- j - 1
              Risk_to_freshwater_value <- k - 1
              Risk_to_Foodsupply_value <- l - 1 
              Risk_to_Settlement_value <- m  - 1
              Risk_to_Economic_opportunities_value <- n  - 1
              
              Sum_Normalized_and_weighted_risk_levels <- (Risk_to_land_value + 
                                                            Risk_to_freshwater_value + 
                                                            Risk_to_Foodsupply_value + 
                                                            Risk_to_Settlement_value + 
                                                            Risk_to_Economic_opportunities_value)
              
              Rescaled_Sum_pillars_risks <- Sum_Normalized_and_weighted_risk_levels
              
              if (Rescaled_Sum_pillars_risks == Risk_to_Habitability_value) {
                Risk_to_Habitability_array[i, j, k,l,m, n] <- 1.00
              }
            }
          }
        }
      }
    }
  }
  
  return(Risk_to_Habitability_array)
}



