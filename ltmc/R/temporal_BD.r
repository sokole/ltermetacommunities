#' @title Temporal beta-diversity
#'
#'
#' @author Eric R. Sokol \email{sokole@gmail.com}
#' @author Thomas Lamy \email{thomas.lamy27@gmail.com}
#'
#'
#' @description Calculates temporal beta-diversity for a single site
#'
#'
#' @param data_in Data input for analysis (can be either long or wide format). When \code{data_in} is wide format and rows are times and columns are taxa, no other parameters are requred. See examples for different \code{data_in} format types that are accepted.
#' @param time_step_col_name Character string indicating time step column name. For use with both \code{data_long} and \code{data_wide} inputs.
#' @param taxon_list Vector (character) of taxon IDs, used to indicate which columns in \code{data_wide} are species counts (or biomass measurements)
#' @param taxon_id_col_name Character string indicating column name in \code{data_long} that contains taxon IDs. Required for long format data.
#' @param biomass_col_name Character string indicating column name in \code{data_long} that contains biomass measurements (or species counts). Required for long format data.
#'
#'
#' @return Returns a numeric value.
#'
#'
#' @examples
#' # data example
#' library(dplyr)
#' library(tidyr)
#'
#' dat_1site_5times <- data.frame(
#'   a = c(1, 2, 3, 4, 5),
#'   b = c(23, 5, 76, 4, 56),
#'   c = c(0, 0, 12, 41, 1))
#'
#' dat_1site_5times_with_time_col <- data.frame(
#'   my_time_step = c(1, 2, 3, 4, 5),
#'   a = c(1, 2, 3, 4, 5),
#'   b = c(23, 5, 76, 4, 56),
#'   c = c(0, 0, 12, 41, 1))
#'
#' dat_long <- dat_1site_5times_with_time_col %>%
#'   gather(my_species, my_biomass, -my_time_step)
#'
#' # examples
#' # wide data, only species cols, rows ordered by time
#' temporal_BD(data_in = dat_1site_5times)
#'
#' # wide data, indicate species and time_step columns
#' temporal_BD(data_in = dat_1site_5times_with_time_col,
#'             taxon_list = c('a', 'b', 'c'), time_step_col_name = 'my_time_step')
#'
#' # long data
#' temporal_BD(data_in = dat_long,
#'             time_step_col_name = 'my_time_step',
#'             taxon_id_col_name = 'my_species',
#'             biomass_col_name = 'my_biomass')
#'
#' @export
temporal_BD <- function(
  data_in = NULL,
  time_step_col_name = NULL, #defaults to 'time_step' if not provided
  taxon_list = NULL,
  taxon_id_col_name = NULL,
  biomass_col_name = NULL){



  #force to be data frame
  data_in <- as.data.frame(data_in)

  if(!is.null(taxon_list)){taxon_id_col_name <- NULL}

  if(is.null(time_step_col_name) & is.null(taxon_id_col_name) & is.null(biomass_col_name)){

    n_numeric_cols <- sum(data_in %>% dplyr::summarize_all(is.numeric) %>% as.numeric())
    if(n_numeric_cols == ncol(data_in)){
      if(is.null(taxon_list)) taxon_list <- names(data_in)
      time_step_col_name <- 'time_step'
      data_in$time_step <- 1:nrow(data_in)
    }else{
      stop('input not formatted correctly, please indicate which cols are taxon names in "taxon_list" and indicate time col using "time_step_col_name"')
    }
  }


  if(!is.null(taxon_list) & length(taxon_list) > 1){
    taxon_id_col_name <- 'taxon_id'
    biomass_col_name <- 'biomass'

    comm_long <- data_in %>%
      # tidyr::gather('taxon_id','biomass', one_of(taxon_list)) %>%
      tidyr::pivot_longer(
        cols = dplyr::any_of(taxon_list),
        names_to = "taxon_id",
        values_to = "biomass") %>%
      dplyr::select(
        dplyr::all_of(c(time_step_col_name, taxon_id_col_name, biomass_col_name)))



  }else if(is.null(taxon_list) & !is.null(taxon_id_col_name)){
    comm_long <- data_in %>%
      dplyr::select(
        dplyr::all_of(c(time_step_col_name, taxon_id_col_name, biomass_col_name)))

  }else{
    stop('input not formatted correctly')
  }

  # rename columns to a known col_name
  comm_long <- as.data.frame(comm_long)
  comm_long$taxon_id <- comm_long[,taxon_id_col_name]
  comm_long$biomass <- comm_long[,biomass_col_name]
  comm_long <- comm_long %>%
    dplyr::select(
      dplyr::all_of(time_step_col_name), .data$taxon_id, .data$biomass)

  comm_temporal_var <- comm_long %>%
    dplyr::group_by(.data$taxon_id) %>%
    dplyr::summarize(
      var_biomass = stats::var(.data$biomass)) %>%
    dplyr::ungroup()

  return(sum(comm_temporal_var$var_biomass))
}
