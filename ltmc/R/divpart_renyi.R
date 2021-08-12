#' @title Numbers equivalents for a metacommunity time series.
#'
#'
#' @author Eric R. Sokol \email{sokole@gmail.com}
#' @author Thomas Lamy \email{thomas.lamy27@gmail.com}
#'
#'
#' @description A wrapper function for \code{vegan::renyi()} to calculate "numbers equivalents" alpha, beta, and gamma diversity indicies for each time step in a metacommunity data set. See Jost (2007) and documentation for \code{vegan::renyi()} for more information about the metrics.
#'
#'
#' @param data_long Data input for analysis (should be \code{NULL} if input is wide format data). Long format data frame (or tibble) with species counts (or biomass measurements) for multiple sites observed over multiple time periods
#' @param data_wide Data input for analysis (should be \code{NULL} if input is long format data). Wide format data frame (or tibble) where rows are observations and columns include \code{time_step_col_name}, \code{site_id_col_name}, taxa listed in \code{taxon_list}.
#' @param time_step_col_name Character string indicating time step column name. For use with both \code{data_long} and \code{data_wide} inputs.
#' @param site_id_col_name Character string indicating site id column name. For use with both \code{data_long} and \code{data_wide} inputs.
#' @param taxon_list Vector (character) of taxon IDs, used to indicate which columns in \code{data_wide} are species counts (or biomass measurements)
#' @param taxon_id_col_name Character string indicating column name in \code{data_long} that contains taxon IDs.
#' @param biomass_col_name Character string indicating column name in \code{data_long} that contains biomass measurements (or species counts).
#' @param q_value Order of the diversity measure. Default is q = 0.
#'
#'
#' @return Returns a data frame.
#'
#'
#' @examples
#' # examples
#' set.seed(123)
#' library(dplyr)
#' library(tidyr)
#'
#' dat_example_long <- data.frame(
#'   my_sites = c(rep('x', 8), rep('y',8)),
#'   my_times = rep(1:4, each = 2),
#'   my_spp = rep(c('a','b'), 4),
#'   my_biomass = (exp(rnorm(16))*10) %>% round(0))
#'
#' divpart_renyi(
#'   data_long = dat_example_long,
#'   site_id_col_name = 'my_sites',
#'   time_step_col_name = 'my_times',
#'   taxon_id_col_name = 'my_spp',
#'   biomass_col_name = 'my_biomass',
#'   q_value = 0)
#'
#' divpart_renyi(
#'   data_long = dat_example_long,
#'   site_id_col_name = 'my_sites',
#'   time_step_col_name = 'my_times',
#'   taxon_id_col_name = 'my_spp',
#'   biomass_col_name = 'my_biomass',
#'   q_value = 1)
#'
#' divpart_renyi(
#'   data_long = dat_example_long,
#'   site_id_col_name = 'my_sites',
#'   time_step_col_name = 'my_times',
#'   taxon_id_col_name = 'my_spp',
#'   biomass_col_name = 'my_biomass',
#'   q_value = 2)
#'
#' # examples with wide data
#' dat_example_wide <- dat_example_long %>% spread(my_spp, my_biomass)
#'
#' divpart_renyi(
#'   data_wide = dat_example_wide,
#'   site_id_col_name = 'my_sites',
#'   time_step_col_name = 'my_times',
#'   taxon_list = c('a','b'),
#'   biomass_col_name = 'my_biomass',
#'   q_value = 0)
#'
#' @references
#' Jost, L. 2007. Partitioning diversity into independent alpha and beta components. Ecology 88:2427–2439. https://doi.org/10.1890/06-1736.1.
#'
#'
#' @export
#'
divpart_renyi <- function(
                    data_long = NULL,
                    data_wide = NULL,
                    time_step_col_name = NULL,
                    site_id_col_name = NULL,
                    taxon_list = NULL, # col name with taxa identifiers
                    taxon_id_col_name = NULL,
                    biomass_col_name = NULL,
                    q_value = 0){ #default is 0 (presence absence), but can be any integer >= 0


  # if data are long, make wide. If data are wide, check to make sure taxon_list is provided.
  if(is.null(data_wide)){
    data_wide <- data_long %>%
      tidyr::spread(taxon_id_col_name, biomass_col_name, fill = 0)
    taxon_list <- data_long %>%
      dplyr::select_at(dplyr::vars(taxon_id_col_name)) %>%
      unlist() %>%
      as.character() %>%
      unique()
  }else if(!is.null(data_wide) & is.null(taxon_list)){
    stop('please provide a taxon_list')
  }


  # nest by time period
  data_nested <- data_wide %>%
    dplyr::group_by_at(dplyr::vars(time_step_col_name)) %>%
    dplyr::select_at(dplyr::vars(taxon_list)) %>%
    tidyr::nest(data = dplyr::any_of(taxon_list))


  # browser()



  # calculate Jost (2007) diversity metrics for all sites in each time period.
  data_results <- data_nested %>%
    dplyr::mutate(
      n_obs = purrr::map_int(.data$data, ~ nrow(.)),
      alpha_div = purrr::map_dbl(
        .x = .data$data,
        .f = ~ vegan::renyi(x = .,
                            hill = TRUE,
                            scales = q_value) %>% mean()),
      gamma_div = purrr::map_dbl(
        .x = .data$data,
        .f = ~ vegan::renyi(x = colMeans(.),
                            hill = TRUE,
                            scales = q_value)),
      beta_div = .data$gamma_div / .data$alpha_div)

  # remove nested data
  data_results <- data_results %>%
    dplyr::select(-.data$data) %>%
    as.data.frame()

  return(data_results)

}
