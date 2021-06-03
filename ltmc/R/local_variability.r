#' @title local variability
#'
#'
#' @author Eric R. Sokol \email{sokole@gmail.com}
#' @author Thomas Lamy \email{thomas.lamy27@gmail.com}
#'
#'
#' @description Calculates local (alpha) variability for each site in a metacommunity
#'
#'
#' @param data_long Data input for analysis (should be \code{NULL} if input is wide format data). Long format data frame (or tibble) with species counts (or biomass measurements) for multiple sites observed over multiple time periods
#' @param data_wide Data input for analysis (should be \code{NULL} if input is long format data). Wide format data frame (or tibble) where rows are observations and columns include \code{time_step_col_name}, \code{site_id_col_name}, taxa listed in \code{taxon_list}.
#' @param time_step_col_name Character string indicating time step column name. For use with both \code{data_long} and \code{data_wide} inputs.
#' @param site_id_col_name Character string indicating site id column name. For use with both \code{data_long} and \code{data_wide} inputs.
#' @param taxon_list Vector (character) of taxon IDs, used to indicate which columns in \code{data_wide} are species counts (or biomass measurements)
#' @param taxon_id_col_name Character string indicating column name in \code{data_long} that contains taxon IDs.
#' @param biomass_col_name Character string indicating column name in \code{data_long} that contains biomass measurements (or species counts).
#' @param standardization_method Only used when \code{variability_type} is "comp". Character string ("h" or "hT") denoting standardization method, either Hellinger ("h") or modified Hellinger ("hT")
#' @param variability_type Character string indicating type of analysis. Either "comp" or "agg".
#'
#'
#' @return Returns a data frame with the selected variability metric calculated for eash site in a metacommunity.
#'
#'
#' @examples
#' set.seed(123)
#' library(dplyr)
#' dat_example_long <- data.frame(
#'   my_sites = c(rep('x', 8), rep('y',8)),
#'   my_times = rep(1:4, each = 2),
#'   my_spp = rep(c('a','b'), 4),
#'   my_biomass = (exp(rnorm(16))*10) %>% round(0))
#'
#' # Variability in community composition using the
#' # modified Hellinger metric
#' local_variability(
#'   data_long = dat_example_long,
#'   time_step_col_name = 'my_times',
#'   site_id_col_name = 'my_sites',
#'   taxon_id_col_name = 'my_spp',
#'   biomass_col_name = 'my_biomass',
#'   standardization_method = 'hT', #or 'h'
#'   variability_type = 'comm')
#'
#' # Variability in aggregate biomass folloing
#' # Wang and Loreau (2014)
#' metacommunity_variability(
#'   data_long = dat_example_long,
#'   time_step_col_name = 'my_times',
#'   site_id_col_name = 'my_sites',
#'   taxon_id_col_name = 'my_spp',
#'   biomass_col_name = 'my_biomass',
#'   variability_type = 'agg')
#'
#'
#' @references
#' Wang, S., and M. Loreau. 2014. Ecosystem stability in space: alpha, beta and gamma variability. Ecology Letters 17:891-901. https://doi.org/10.1111/ele.12292.
#'
#'
#' @export
#'
local_variability <- function(
  data_long = NULL,
  data_wide = NULL,
  time_step_col_name = NULL,
  site_id_col_name = NULL,
  taxon_list = NULL,
  taxon_id_col_name = NULL,
  biomass_col_name = NULL,
  standardization_method = NULL, #'hT' or 'h'
  variability_type = NULL #'comp' or 'agg'
){


  # make data long if provide in wide format
  if(is.null(data_long)){
    data_long <- data_wide %>%
      dplyr::select(
        dplyr::one_of(site_id_col_name, time_step_col_name, taxon_list)) %>%
      tidyr::gather(taxon_id, biomass, dplyr::one_of(taxon_list))

  }else if(!is.null(data_long)){ #otherwise, if already long, identify which col is the taxon names and biomass values
    if(is.null(taxon_id_col_name)) stop('which col name is the taxon names?')
    data_long <- as.data.frame(data_long)
    data_long$taxon_id = data_long[,taxon_id_col_name]

    if(is.null(biomass_col_name)) stop('indicate the name of the biomass column?')
    data_long$biomass = data_long[,biomass_col_name]
  }

  # calc total metacommunity biomass, needed for BD_hT
  data_long$total_metacommunity_biomass <- data_long$biomass %>% sum

  # compositinoal variability calculations
  if(grepl('(?i)com', variability_type) |  grepl('(?i)BD', variability_type) | grepl('(?i)beta', variability_type)){

    # use long format data as input -- standardize biomass values using Hellinger or modified Hellinger transforms
    data_long <- data_long %>%
      dplyr::group_by_at(
        dplyr::vars(site_id_col_name, time_step_col_name)) %>%
      dplyr::mutate(
        total_site_biomass_per_time = sum(biomass)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        biomass_standardized = dplyr::case_when(
          tolower(standardization_method) == 'h' ~ sqrt(biomass/total_site_biomass_per_time),
          grepl('(?i)hT',standardization_method) ~ sqrt(biomass/total_metacommunity_biomass)),
        standardization_method = standardization_method)

    # calculate temporal BD by site using temporal_BD() function
    temporal_BD_by_site <- data_long %>%
      tidyr::nest(-one_of(site_id_col_name)) %>%
      mutate(
        BD = purrr::map(
          .x = .$data,
          .f = temporal_BD,
          time_step_col_name = time_step_col_name,
          taxon_id_col_name = 'taxon_id',
          biomass_col_name = 'biomass_standardized')
      ) %>%
      dplyr::select(-data) %>%
      tidyr::unnest()

    # make data_out table, add info on method used
    if(tolower(standardization_method) == 'h'){

      data_out <- data.frame(
        temporal_BD_by_site,
        variability_type,
        metric_type = 'BD_h')

    }else if(grepl('(?i)hT',standardization_method)){

      data_out <- data.frame(
        temporal_BD_by_site,
        variability_type,
        metric_type = 'BD_hT')}

    # calc temporal BD using aggregate method
  }else if(grepl('(i?)agg',variability_type)){

    sd_T_by_site <- data_long %>%
      dplyr::select(
        dplyr::one_of(site_id_col_name, time_step_col_name), biomass) %>%
      dplyr::group_by_at(
        dplyr::vars(site_id_col_name, time_step_col_name)) %>%
      dplyr::summarize(agg_biomass = sum(biomass)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by_at(
        dplyr::vars(site_id_col_name)) %>%
      dplyr::summarize(CV = ltmc::cv(agg_biomass))

    data_out <- data.frame(
      sd_T_by_site,
      variability_type,
      metric_type = 'CV_biomass')
  }


  return(data_out)
} #END FUNCTION

########################################
