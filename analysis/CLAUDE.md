# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an R-based data analysis pipeline for the **OECD Ireland Legal Needs Survey (LNS) 2025**, produced by the World Justice Project. It processes survey microdata (`.dta` format) and generates SVG visualizations for a country report on access to justice, dispute resolution, and legal capability.

## Running the Pipeline

All scripts are sourced from `main.R`, run from the project root:

```r
# Full sample
Rscript main.R

# High-impact problems only (AJE_impact in 3,4,5)
Rscript main.R --high_impact=TRUE
```

Output SVGs are written to `output/`. A summary data table is written to `tables_outline.xlsx`.

### Non-standard package installs

Two packages must be installed from GitHub before running:

```r
devtools::install_github("davidsjoberg/ggsankey")
devtools::install_github("ctoruno/WJPr")
```

All other packages are managed via `pacman::p_load()` in `code/settings.R`.

## Project Structure

The git repository lives inside `analysis/`. Everything else — data, output, fonts — sits in sibling directories accessed via `path2SP` and `path2fonts`.

```
OECD-Ireland-LNS/                   ← path2SP
├── data/
│   ├── ireland_lns_2025_final.dta  ← main input (cleaned)
│   ├── ireland_lns_2025_raw.dta
│   └── counts_a2j_path.dta         ← pre-computed Sankey counts
├── metadata/
├── output/                         ← all SVG outputs written here
├── tables_outline.xlsx             ← summary table written by main.R
└── analysis/                       ← git repo root (working directory)
    ├── main.R
    ├── CLAUDE.md
    ├── code/
    │   ├── settings.R
    │   ├── data_wrangling.R
    │   ├── params.R
    │   ├── functions.R
    │   ├── bars_group.R
    │   ├── sankey_rep.R
    │   ├── sankey_drm.R
    │   └── network_graph.R
    └── cleaning/                   ← Stata cleaning scripts (not run by main.R)
        ├── cleaning_ireland.do
        └── routines/
            ├── renaming.do
            ├── routing.do
            └── variable_labels.do
```

## Architecture

The pipeline follows a strict **params → tables → plots** separation:

```
main.R
├── code/settings.R       # packages, path2SP, font loading
├── code/data_wrangling.R # wrangle_ireland_lns() — all variable construction
├── code/params.R         # groupbars_params(), build_bars_params() — config only, no data
├── code/functions.R      # compute_*/render_* — data-agnostic computation + save logic
├── code/bars_group.R     # plot_by_group(), plot_coocurrence_bars()
├── code/sankey_rep.R     # plot_sankey_advice()
├── code/sankey_drm.R     # plot_sankey_drm()
└── code/network_graph.R  # network_graph()
```

### Key design patterns

**`path2SP`** is set per-user in `settings.R` via `Sys.info()["user"]`. It points to the **parent** of the project root (the `OECD-Ireland-LNS/` directory), because `data/`, `metadata/`, and `output/` live there rather than inside the repo. All file I/O uses `file.path(path2SP, ...)`. Adding a new user requires adding a new `if` block here.

**`wrangle_ireland_lns(master_data)`** (`data_wrangling.R`) is the single transformation step. It takes raw `.dta` data and returns `data_subset.df` with all derived variables (binary indicators, recoded sociodemographics, DRM quality metrics). No wrangling happens downstream of this function.

**Grouped bar params flow** (`params.R` → `functions.R` → `bars_group.R`):
- `groupbars_params()` returns a list with `full_group_cfg`, `levels_map`, `groups_presets`, and `measures`. Each measure has an `id`, a quoted `value` column, and a `groups_preset` name (`"basic"`, `"extended"`, or `"problem"`). The `"problem"` preset disaggregates only by `category` (selected problem type).
- `compute_groupbars_tables()` runs `summarize_by_vars()` for each measure across all disaggregation groups, suppressing cells with `n < 30`.
- `render_groupbars_plots()` calls `plot_by_group()` and saves each plot as SVG.

**Multi-response bar params flow** (`params.R` → `functions.R`):
- `build_bars_params()` defines `blocks` — each block maps a set of binary columns (e.g., `AJD_adviser_1:17`) to human-readable labels via either a positional vector (`labels_vec_id`) or a named map (`labels_map_id`).
- `compute_bars_tables()` + `render_bars_plots()` handle aggregation and saving.

**DRM heatmaps**: `tables_drm()` / `tables_drm2()` pivot and label `drm_*` columns into actor × quality-dimension grids. Cells with `n_obs < 30` are set to `NA` in `main.R` before plotting.

### Survey variable naming conventions

- `AJP_*` — problem/prevalence questions
- `AJD_*` — dispute resolution / adviser questions  
- `AJR_*` — resolution process questions
- `AJE_*` — experience/impact questions
- `drm_*` — derived DRM quality metrics (efficiency, fairness, affordable, duration, helpful)
- `drm_res_*` — derived DRM outcome metrics (outcome, appeal, rep, oth)

### Data files

Data lives **outside the project root** at `../` (i.e., `OECD-Ireland-LNS/`), accessed via `path2SP`:

| Path relative to `path2SP` | Description |
|------|-------------|
| `data/ireland_lns_2025_final.dta` | Cleaned analysis dataset (input to `main.R`) |
| `data/ireland_lns_2025_raw.dta` | Raw survey data |
| `data/counts_a2j_path.dta` | Pre-computed counts for Sankey paths |
| `cleaning/cleaning_ireland.do` | Stata cleaning script (raw → final) |

### Adding a new plot

1. If the measure is a single column already in `data_subset.df`, add it via `add_measure()` in `groupbars_params()` (`params.R`).
2. If it's a new multi-response block, add an `add_mr_block()` entry in `build_bars_params()` (`params.R`).
3. If neither fits, create a new function in `functions.R` following the `tables_*/plot_*` naming pattern and call it from `main.R`.
4. New derived variables belong in `wrangle_ireland_lns()` in `data_wrangling.R`.

### Fonts

The Inter font family must be available at `path2fonts` (a sibling OneDrive directory `0. Fonts/`). `showtext_auto()` is called in `settings.R` so all ggplot2 output uses it automatically.
