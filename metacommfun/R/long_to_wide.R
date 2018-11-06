
# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


library(tidyverse)

long_to_wide <- function(dat.in.long = d.in.long,
                         location_name = 'SITE_ID',
                         time_step_name = 'DATE',
                         taxon_name = 'VARIABLE_NAME',
                         taxon_count_name = 'VALUE',
                         ...){

    dat.in.wide.spp <- dat.in.long %>% 
        group_by_(.dots = c(location_name,
                            time_step_name,
                            taxon_name)) %>%
        select_(.dots = list(location_name,
                             time_step_name,
                             taxon_name,
                             taxon_count_name)) %>% 
        summarise_all(funs( mean(as.numeric(.), na.rm = TRUE))) %>%
        tidyr::spread_(key_col = taxon_name, 
                       value_col = taxon_count_name,
                       fill = 0) %>%
        na.omit() %>%
        ungroup()

    return(dat.in.wide.spp)

}

