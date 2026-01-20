# ============================================================
#
# CO-OCCURRENCE NETWORK OF JUSTICE PROBLEMS IN IRELAND
#
# ============================================================

network_graph <- function(data = data2plot) {
  
  # ============================================================
  # 1. DATA PREPARATION
  # ============================================================
  
  consolidated_df <- data
  
  # Convert all AJP columns to numeric
  ajp_cols <- grep("^AJP_", names(df), value = TRUE)
  for (col in ajp_cols) {
    df[[col]] <- as.numeric(as.character(df[[col]]))
    df[[col]][is.na(df[[col]])] <- 0
  }
  
  cats <- setdiff(names(consolidated_df), "respondentid")
  n_cats <- length(cats)
  n_total <- nrow(consolidated_df)
  
  # ============================================================
  # 2. CALCULATE LIFT MATRIX
  # ============================================================
  
  lift_matrix <- matrix(0, n_cats, n_cats, dimnames = list(cats, cats))
  cooccurrence_counts <- matrix(0, n_cats, n_cats, dimnames = list(cats, cats))
  
  for (i in 1:n_cats) {
    for (j in 1:n_cats) {
      cat1 <- cats[i]
      cat2 <- cats[j]
      both <- sum(consolidated_df[[cat1]] == 1 & consolidated_df[[cat2]] == 1, na.rm = TRUE)
      cooccurrence_counts[i, j] <- both
      
      if (i == j) {
        lift_matrix[i, j] <- 1
      } else {
        p_both <- both / n_total
        p_cat1 <- mean(consolidated_df[[cat1]], na.rm = TRUE)
        p_cat2 <- mean(consolidated_df[[cat2]], na.rm = TRUE)
        if (p_cat1 > 0 & p_cat2 > 0) {
          lift_matrix[i, j] <- p_both / (p_cat1 * p_cat2)
        }
      }
    }
  }
  
  # Calculate prevalence
  prevalence <- sapply(cats, function(cat) mean(consolidated_df[[cat]], na.rm = TRUE) * 100)
  
  cat("Prevalence:\n")
  print(sort(prevalence, decreasing = TRUE))
  
  # ============================================================
  # 3. CREATE NETWORK AND CALCULATE CENTRALITY
  # ============================================================
  
  # Create edge list (all edges for layout calculation)
  edges_all <- data.frame()
  for (i in 1:(n_cats-1)) {
    for (j in (i+1):n_cats) {
      cat1 <- cats[i]
      cat2 <- cats[j]
      edges_all <- rbind(edges_all, data.frame(
        from = cat1,
        to = cat2,
        weight = lift_matrix[cat1, cat2],
        cooccurrence = cooccurrence_counts[cat1, cat2],
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Create edge list for visualization (only significant edges)
  edges_sig <- edges_all %>%
    filter(weight > 1.2, cooccurrence >= 10)
  
  # Create nodes dataframe
  nodes <- data.frame(
    name = cats,
    prevalence = prevalence[cats],
    stringsAsFactors = FALSE
  )
  
  # Create igraph object with ALL edges for layout
  g_full <- graph_from_data_frame(edges_all, directed = FALSE, vertices = nodes)
  E(g_full)$weight <- edges_all$weight
  
  # Calculate weighted degree centrality
  weighted_degree <- strength(g_full, weights = E(g_full)$weight)
  names(weighted_degree) <- V(g_full)$name
  
  # Add centrality to nodes
  nodes$centrality <- weighted_degree[nodes$name]
  
  # Normalize centrality for color gradient (0 to 1)
  nodes$centrality_norm <- (nodes$centrality - min(nodes$centrality)) / 
    (max(nodes$centrality) - min(nodes$centrality))
  
  cat("\nCentrality Ranking:\n")
  print(sort(weighted_degree, decreasing = TRUE))
  
  # ============================================================
  # 4. CREATE COLOR GRADIENT BASED ON #575796
  # ============================================================
  
  # Base color: #575796 (RGB: 87, 87, 150)
  # Create gradient from light (low centrality) to dark (high centrality)
  
  # Function to create color gradient
  create_purple_gradient <- function(centrality_norm) {
    # Light version: #A8A8C8 (more white mixed in)
    # Dark version: #575796 (original)
    # Very dark: #3D3D6B (darker)
    
    # Interpolate between light and dark based on centrality
    light_rgb <- c(168, 168, 200)  # Light purple
    dark_rgb <- c(57, 57, 107)     # Dark purple (darker than #575796)
    
    r <- round(light_rgb[1] + (dark_rgb[1] - light_rgb[1]) * centrality_norm)
    g <- round(light_rgb[2] + (dark_rgb[2] - light_rgb[2]) * centrality_norm)
    b <- round(light_rgb[3] + (dark_rgb[3] - light_rgb[3]) * centrality_norm)
    
    rgb(r, g, b, maxColorValue = 255)
  }
  
  # Apply color to nodes
  nodes$color <- sapply(nodes$centrality_norm, create_purple_gradient)
  
  # ============================================================
  # 5. FORCE-DIRECTED LAYOUT
  # ============================================================
  
  cat("\nGenerating force-directed layout...\n")
  
  # Use Fruchterman-Reingold with weights
  set.seed(42)
  layout_fr <- layout_with_fr(g_full, weights = E(g_full)$weight^2, niter = 1000)
  
  # Create layout dataframe
  layout_df <- data.frame(
    name = V(g_full)$name,
    x = layout_fr[, 1],
    y = layout_fr[, 2]
  )
  
  # Merge with nodes
  nodes <- merge(nodes, layout_df, by = "name")
  
  # ============================================================
  # 6. CREATE VISUALIZATION WITH GGPLOT2
  # ============================================================
  
  cat("Creating visualization...\n")
  
  # Prepare edge data for plotting
  edges_plot <- edges_sig %>%
    left_join(nodes %>% select(name, x, y), by = c("from" = "name")) %>%
    rename(x_from = x, y_from = y) %>%
    left_join(nodes %>% select(name, x, y), by = c("to" = "name")) %>%
    rename(x_to = x, y_to = y) %>%
    mutate(
      edge_width = (weight - 0.8) * 2.5,
      edge_alpha = pmin(0.85, (weight - 1) * 0.35 + 0.2)
    )
  
  # Calculate node sizes (much larger)
  min_size <- 45
  max_size <- 45
  nodes$node_size <- min_size + (nodes$prevalence - min(nodes$prevalence)) / 
    (max(nodes$prevalence) - min(nodes$prevalence)) * (max_size - min_size)
  
  # Create the plot
  p <- ggplot() +
    # Draw edges
    geom_segment(
      data = edges_plot,
      aes(x = x_from, y = y_from, xend = x_to, yend = y_to,
          linewidth = edge_width, alpha = edge_alpha),
      color = "#5D6D7E",
      lineend = "round"
    ) +
    scale_linewidth_identity() +
    scale_alpha_identity() +
    
    # Draw nodes
    geom_point(
      data = nodes,
      aes(x = x, y = y, size = node_size, fill = color),
      shape = 21,
      color = "#2C3E50",
      stroke = 1.5
    ) +
    scale_size_identity() +
    scale_fill_identity() +
    
    # Add labels
    geom_text(
      data = nodes %>% mutate(
        # Wrap long names
        label_name = case_when(
          name == "Government Benefits" ~ "Government\nBenefits",
          name == "Public Services" ~ "Public\nServices",
          name == "Money & Debt" ~ "Money &\nDebt",
          TRUE ~ name
        ),
        label_full = paste0(label_name, "\n(", round(prevalence, 0), "%)")
      ),
      aes(x = x, y = y, label = label_full),
      size = 5.5,
      fontface = "bold",
      color = "white",
      lineheight = 0.85
    ) +
    # Theme and labels
    labs(
      caption = "Closer nodes = Stronger co-occurrence (higher Lift)\nNode (%) = Prevalence (%)\nNode color = Centrality (darker = more central)"
    ) +
    theme_void(base_size = 14) +
    theme(
      #plot.title = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(b = 5)),
      #plot.subtitle = element_text(hjust = 0.5, size = 13, color = "gray40", margin = margin(b = 15)),
      plot.caption = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(t = 15)),
      plot.margin = margin(20, 20, 20, 20),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    coord_fixed();p
}
