# 0_smallsets.R
# using smallsets: https://cran.r-project.org/web/packages/smallsets/vignettes/smallsets.html
# August 2023

library(smallsets)

Smallset_Timeline(data = s_data, 
                  code = system.file("0_read_focus_groups.R", package = "smallsets"), 
                  rowCount = 7, 
                  rowSelect = NULL)
