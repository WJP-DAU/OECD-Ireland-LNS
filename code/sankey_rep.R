plot_sankey_advice <- function(data) {
  
  # Wrangling base
  sankey_wrangling <- data %>%
    dplyr::mutate(
      sankey_advice_stage_1 = dplyr::case_when(
        is.na(had_dispute)    ~ NA_character_,
        had_dispute == 0      ~ NA_character_,
        contact_adviser == 1  ~ "Contacted an advisor",
        contact_adviser == 0  ~ "Did not contact an advisor"
      ),
      sankey_advice_stage_2 = dplyr::case_when(
        is.na(had_dispute)           ~ NA_character_,
        had_dispute == 0             ~ NA_character_,
        appropriate_adviser == 1     ~ "Appropriate Advisor",
        no_appropriate_adviser == 1  ~ "Non-Appropriate Advisor",
        no_need_assistance == 1      ~ "Did not need an advisor",
        needed_assistance == 1       ~ "Needed but did not have access to an advisor",
        unknown == 1                 ~ "Unknown Reason"
      )
    ) %>% 
    dplyr::select(
      dplyr::starts_with("sankey_advice_stage_")
    ) %>%
    dplyr::filter(
      !is.na(sankey_advice_stage_1) & !is.na(sankey_advice_stage_2)
    )
  
  # Stage 1 Data
  sankey_data_stage_1 <- sankey_wrangling %>% 
    dplyr::group_by(sankey_advice_stage_1) %>% 
    dplyr::summarise(
      count = dplyr::n(),
      .groups = "keep"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      total = sum(count),
      national_avg = count / total
    )
  
  # Stage 2 Data
  sankey_data_stage_2 <- sankey_wrangling %>% 
    dplyr::group_by(sankey_advice_stage_2) %>% 
    dplyr::summarise(
      count = dplyr::n(),
      .groups = "keep"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      total = sum(count),
      national_avg = count / total
    )
  
  # Stage 3 Data
  sankey_data_stage_3 <- sankey_data_stage_2 %>% 
    dplyr::mutate(
      sankey_advice_stage_3 = dplyr::case_when(
        sankey_advice_stage_2 %in% c(
          "Appropriate Advisor", 
          "Did not need an advisor"
        ) ~ "Had access",
        sankey_advice_stage_2 %in% c(
          "Non-Appropriate Advisor", 
          "Needed but did not have access to an advisor",
          "Unknown Reason"
        ) ~ "Had no access"
      )
    ) %>% 
    dplyr::group_by(sankey_advice_stage_3) %>% 
    dplyr::summarise(
      national_avg = sum(national_avg),
      .groups = "keep"
    )
  
  # Proportions
  proportions <- dplyr::bind_rows(
    sankey_data_stage_1 %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(x = "sankey_advice_stage_1") %>% 
      dplyr::select(x, node = sankey_advice_stage_1, perc = national_avg),
    
    sankey_data_stage_2 %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(x = "sankey_advice_stage_2") %>% 
      dplyr::select(x, node = sankey_advice_stage_2, perc = national_avg),
    
    sankey_data_stage_3 %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(x = "sankey_advice_stage_3") %>% 
      dplyr::select(x, node = sankey_advice_stage_3, perc = national_avg)
  ) %>% 
    dplyr::mutate(
      perc = scales::percent(perc, accuracy = 0.1)
    )
  
  # Data final para Sankey
  sankey_final_data <- sankey_data_stage_2 %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      sankey_advice_stage_1 = dplyr::case_when(
        sankey_advice_stage_2 %in% c(
          "Appropriate Advisor", 
          "Non-Appropriate Advisor"
        ) ~ "Contacted an advisor",
        sankey_advice_stage_2 %in% c(
          "Did not need an advisor", 
          "Needed but did not have access to an advisor",
          "Unknown Reason"
        ) ~ "Did not contact an advisor"
      ),
      sankey_advice_stage_3 = dplyr::case_when(
        sankey_advice_stage_2 %in% c(
          "Appropriate Advisor", 
          "Did not need an advisor"
        ) ~ "Had access",
        sankey_advice_stage_2 %in% c(
          "Non-Appropriate Advisor", 
          "Needed but did not have access to an advisor",
          "Unknown Reason"
        ) ~ "Had no access"
      )
    ) %>% 
    ggsankey::make_long(
      sankey_advice_stage_1, sankey_advice_stage_2, sankey_advice_stage_3,
      value = "national_avg"
    ) %>% 
    dplyr::left_join(
      proportions,
      by = c("x", "node")
    )
  
  # Plot
  p <- ggplot2::ggplot(
    sankey_final_data, 
    ggplot2::aes(
      x         = x, 
      next_x    = next_x, 
      node      = node, 
      next_node = next_node,
      label     = paste(node, perc, sep = ":\n"),
      fill      = node,
      value     = value
    )
  ) +
    ggsankey::geom_sankey(
      flow.alpha = .6
      # node.color = "gray30"
    ) +
    ggsankey::geom_sankey_label(
      size   = 3, 
      color  = "gray15", 
      fill   = "gray90",
      family = "inter"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Appropriate Advisor"                        = "#A0BAC7",
        "Contacted an advisor"                       = "#386641",
        "Did not contact an advisor"                 = "#A63C06",
        "Did not need an advisor"                    = "#A0BAC7",
        "Had access"                                 = "#0A69A5",
        "Had no access"                              = "#710000",
        "Needed but did not have access to an advisor" = "#A37B67",
        "Non-Appropriate Advisor"                    = "#A37B67",
        "Unknown Reason"                             = "#707173"
      )
    ) +
    ggplot2::scale_x_discrete(
      position = "top",
      labels = c(
        "Contacted an Advisor", 
        "Type of Advisor /\nReason for No Contact", 
        "Access to Adequate\nAssistance and Representation"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position       = "none",
      panel.grid.major.x    = ggplot2::element_blank(),
      panel.grid.major.y    = ggplot2::element_blank(),
      panel.grid.minor.y    = ggplot2::element_blank(),
      axis.text.x           = ggplot2::element_text(
        size   = 12,
        family = "lato",
        face   = "bold.italic"
      ),
      axis.text.y           = ggplot2::element_blank(),
      axis.title.x          = ggplot2::element_blank(),
      plot.margin           = ggplot2::margin(0, -40, -15, -45)
    )
  
  return(p)
}
