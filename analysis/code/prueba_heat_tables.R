## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Co-occurrence Heat Tables (Prueba)
##
## Purpose:           Generate two clearer alternatives to network visualization:
##                    1) Option 2 + 3: conditional co-occurrence + qualitative label
##                    2) Option 1 + 3: symmetric co-occurrence + qualitative label
##
## Output folder:     analysis/prueba/
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("code/settings.R")
source("code/data_wrangling.R")

out_dir <- file.path(getwd(), "prueba")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

master_data <- haven::read_dta(file.path(path2SP, "data/ireland_lns_2025_final.dta"))
data_subset <- wrangle_ireland_lns(master_data)

problem_cols <- c(
    "problem_cat_land", "problem_cat_neighbors", "problem_cat_housing", "problem_cat_family",
    "problem_cat_injury", "problem_cat_citizen", "problem_cat_gov", "problem_cat_public",
    "problem_cat_products", "problem_cat_services", "problem_cat_money", "problem_cat_employment"
)

problem_labels <- c(
    problem_cat_land = "Land",
    problem_cat_neighbors = "Neighbors",
    problem_cat_housing = "Housing",
    problem_cat_family = "Family/relationship",
    problem_cat_injury = "Injury",
    problem_cat_citizen = "Citizenship",
    problem_cat_gov = "Government\nbenefits",
    problem_cat_public = "Public\nservices",
    problem_cat_products = "Products",
    problem_cat_services = "Services",
    problem_cat_money = "Money/debt",
    problem_cat_employment = "Employment"
)

problems_df <- data_subset %>%
    dplyr::select(all_of(problem_cols)) %>%
    dplyr::mutate(dplyr::across(everything(), ~ dplyr::if_else(.x == 1, 1, 0, missing = 0)))

total_n <- nrow(problems_df)
base_n <- colSums(problems_df == 1, na.rm = TRUE)

joint_counts <- matrix(0,
    nrow = length(problem_cols), ncol = length(problem_cols),
    dimnames = list(problem_cols, problem_cols)
)

for (i in seq_along(problem_cols)) {
    for (j in seq_along(problem_cols)) {
        joint_counts[i, j] <- sum(problems_df[[problem_cols[i]]] == 1 & problems_df[[problem_cols[j]]] == 1, na.rm = TRUE)
    }
}

build_heat_plot <- function(df_long,
                            file_name,
                            footer_how_to_read,
                            highlight_diagonal = FALSE,
                            width_mm = 320,
                            height_mm = 285) {
    min_val <- min(df_long$value, na.rm = TRUE)
    max_val <- max(df_long$value, na.rm = TRUE)

    if (!is.finite(min_val) || !is.finite(max_val) || min_val == max_val) {
        min_val <- 0
        max_val <- 1
    }

    tile_base <- if (highlight_diagonal) {
        dplyr::filter(df_long, !is_diagonal)
    } else {
        df_long
    }

    p <- ggplot2::ggplot(df_long, ggplot2::aes(x = col_problem, y = row_problem)) +
        ggplot2::geom_tile(
            data = tile_base,
            ggplot2::aes(fill = value),
            color = "white",
            linewidth = 0.8
        ) +
        {
            if (highlight_diagonal) {
                ggplot2::geom_tile(
                    data = dplyr::filter(df_long, is_diagonal),
                    fill = "white",
                    color = "white",
                    linewidth = 1
                )
            }
        } +
        ggplot2::geom_text(
            ggplot2::aes(label = label),
            family = "inter",
            size = 3,
            fontface = "bold",
            lineheight = 0.9,
            color = "#1a1a1a",
            na.rm = TRUE
        ) +
        ggplot2::scale_fill_gradient(
            low = "#E3E4F5",
            high = "#575796",
            limits = c(min_val, max_val),
            labels = scales::percent_format(accuracy = 1)
        ) +
        ggplot2::scale_x_discrete(position = "top") +
        ggplot2::labs(
            x = "Also had this problem (column)",
            y = "Given this problem (row)",
            caption = paste0("How to read: ", footer_how_to_read)
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            legend.position = "none",
            axis.text.x.top = ggplot2::element_text(family = "inter", face = "bold", size = 9.5, color = "#1a1a1a"),
            axis.text.y = ggplot2::element_text(family = "inter", face = "bold", size = 10, color = "#1a1a1a"),
            plot.caption = ggplot2::element_text(family = "inter", size = 12, color = "#1a1a1a", lineheight = 1.2, hjust = 0),
            plot.caption.position = "plot",
            axis.title.x = ggplot2::element_text(family = "inter", face = "bold", size = 10, color = "#1a1a1a", margin = ggplot2::margin(t = 18)),
            axis.title.x.top = ggplot2::element_text(family = "inter", face = "bold", size = 10, color = "#1a1a1a", margin = ggplot2::margin(b = 14)),
            axis.title.y = ggplot2::element_text(family = "inter", face = "bold", size = 10, color = "#1a1a1a", margin = ggplot2::margin(r = 18)),
            panel.grid = ggplot2::element_blank(),
            plot.margin = ggplot2::margin(8, 18, 12, 8)
        )

    ggplot2::ggsave(
        filename = file.path(out_dir, file_name),
        plot = p,
        width = width_mm,
        height = height_mm,
        units = "mm"
    )

    p
}

# -----------------------------------------------------------------------------
# Option 2 + 3: Conditional matrix P(column | row) + qualitative label
# -----------------------------------------------------------------------------

conditional_mat <- matrix(NA_real_,
    nrow = length(problem_cols), ncol = length(problem_cols),
    dimnames = list(problem_cols, problem_cols)
)

for (i in seq_along(problem_cols)) {
    for (j in seq_along(problem_cols)) {
        denom <- base_n[i]
        conditional_mat[i, j] <- if (denom > 0) joint_counts[i, j] / denom else NA_real_
    }
}

# Suppress rows with low base (same n<30 convention used in project)
low_base_rows <- names(base_n[base_n < 30])
if (length(low_base_rows) > 0) {
    conditional_mat[low_base_rows, ] <- NA_real_
}

cond_long <- as.data.frame(as.table(conditional_mat), stringsAsFactors = FALSE) %>%
    dplyr::rename(row_problem = Var1, col_problem = Var2, value = Freq) %>%
    dplyr::mutate(
        is_diagonal = row_problem == col_problem,
        label = dplyr::if_else(
            is.na(value),
            "N/A",
            scales::percent(value, accuracy = 1)
        ),
        label = dplyr::if_else(is_diagonal, "", label),
        row_problem = dplyr::recode(row_problem, !!!problem_labels),
        col_problem = dplyr::recode(col_problem, !!!problem_labels)
    )

row_order <- base_n %>%
    sort(decreasing = TRUE) %>%
    names() %>%
    dplyr::recode(!!!problem_labels)

cond_long <- cond_long %>%
    dplyr::mutate(
        row_problem = factor(row_problem, levels = rev(row_order)),
        col_problem = factor(col_problem, levels = row_order)
    )

build_heat_plot(
    df_long = cond_long,
    file_name = "cooccurrence_option2_plus_3.svg",
    footer_how_to_read = "Choose a row problem, then move across columns. Each cell shows the % who also had the column problem.",
    highlight_diagonal = TRUE
)

readr::write_csv(cond_long, file.path(out_dir, "cooccurrence_option2_plus_3.csv"))

# -----------------------------------------------------------------------------
# Option 1 + 3: Symmetric matrix (joint % of total sample) + qualitative label
# -----------------------------------------------------------------------------

joint_pct_total <- joint_counts / total_n

sym_long <- as.data.frame(as.table(joint_pct_total), stringsAsFactors = FALSE) %>%
    dplyr::rename(row_problem = Var1, col_problem = Var2, value = Freq) %>%
    dplyr::mutate(
        is_diagonal = row_problem == col_problem,
        label = dplyr::if_else(
            is.na(value),
            "N/A",
            scales::percent(value, accuracy = 1)
        ),
        row_problem = dplyr::recode(row_problem, !!!problem_labels),
        col_problem = dplyr::recode(col_problem, !!!problem_labels)
    ) %>%
    dplyr::mutate(
        row_problem = factor(row_problem, levels = rev(row_order)),
        col_problem = factor(col_problem, levels = row_order)
    )

build_heat_plot(
    df_long = sym_long,
    file_name = "cooccurrence_option1_plus_3.svg",
    footer_how_to_read = "Each cell shows the % of the full sample that reported both the row and the column problems.",
    highlight_diagonal = FALSE
)

readr::write_csv(sym_long, file.path(out_dir, "cooccurrence_option1_plus_3.csv"))

message("Saved files in: ", out_dir)
