## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            OECD Ireland LNS Report - Settings
##
## Author(s):         Natalia Rodriguez   (nrodriguez@worldjusticeproject.org)
##                    Santiago Pardo      (spardo@worldjusticeproject.org)
##
## Dependencies:      World Justice Project
##
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 1.  Required packages                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required packages
library(pacman)

# Notes: ggsankey and ggwaffle need to be installed from Github's developer version. Run the following lines 
# of code in order to install: 
# devtools::install_github("davidsjoberg/ggsankey")
# devtools::install_github("liamgilbey/ggwaffle")
#devtools::install_github("ctoruno/WJPr")

p_load(char = c(
  
  # Data Loading and Saving
  "haven", "readxl", "writexl", "openxlsx", "lubridate", "rlang", "janitor",
  # Viz
  "showtext", "forcats", "stringr", "ggtext", "WJPr",
  # Good 'ol Tidyverse
  "tidyverse"
  
))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 2.  SharePoint Path                                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SharePoint path
if (Sys.info()["user"] == "nrodriguez") {
  path2SP <- "/Users/nrodriguez/OneDrive - World Justice Project/Programmatic/Data Analytics/6. Country Reports/OECD-Ireland-LNS/"
  path2fonts <- "/Users/nrodriguez/OneDrive - World Justice Project/Programmatic/Data Analytics/6. Country Reports/0. Fonts/"
  
}
if (Sys.info()["user"] == "santiagopardo") {
  path2SP <- "/Users/santiagopardo/OneDrive - World Justice Project/Data Analytics/6. Country Reports/OECD-Ireland-LNS/"
  path2fonts <- "/Users/santiagopardo/OneDrive - World Justice Project/Data Analytics/6. Country Reports/0. Fonts/"
  
  
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## 3.  Fonts                                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#Loading fonts
font_add(family     = "inter",
         regular    = paste0(path2fonts, "Inter_24pt-Regular.ttf"),
         italic     = paste0(path2fonts, "Inter_24pt-Italic.ttf"),
         bold       = paste0(path2fonts, "Inter_24pt-Bold.ttf"),
         bolditalic = paste0(path2fonts, "Inter_24pt-BoldItalic.ttf"))
showtext_auto()

