#' @title Metacommunity variability
#'
#'
#' @author Eric R. Sokol \email{sokole@gmail.com}
#' @author Thomas Lamy \email{thomas.lamy27@gmail.com}
#'
#'
#' @description Calculates local (alpha) and regional (gamma) metacommunity variability and the scaling factor (phi)
#'
#'
#'
#' @param data_long Data input for analysis (should be \code{NULL} if input is wide format data). Long format data frame (or tibble) with species counts (or biomass measurements) for multiple sites observed over multiple time periods
#' @param data_wide Data input for analysis (should be \code{NULL} if input is long format data). Wide format data frame (or tibble) where rows are observations and columns include \code{time_step_col_name}, \code{site_id_col_name}, taxa listed in \code{taxon_list}.
#' @param time_step_col_name Character string indicating time step column name. For use with both \code{data_long} and \code{data_wide} inputs.
#' @param site_id_col_name Character string indicating site id column name. For use with both \code{data_long} and \code{data_wide} inputs.
#' @param taxon_list Vector (character) of taxon IDs, used to indicate which columns in \code{data_wide} are species counts (or biomass measurements)
#' @param taxon_id_col_name Character string indicating column name in \code{data_long} that contains taxon IDs.
#' @param biomass_col_name Character string indicating column name in \code{data_long} that contains biomass measurements (or species counts).
#' @param standardization_method Only used when \code{variability_type} is "comp". Character string denoting standardization method. Currently, only Hellinger ("h") is implemented.
#' @param variability_type Character string indicating type of analysis. Either "comp" or "agg".
#'
#'
#' @return Returns a one-row data frame where each element names indicate the variability metric that is being returned.
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
#' metacommunity_variability(
#'   data_long = dat_example_long,
#'   time_step_col_name = 'my_times',
#'   site_id_col_name = 'my_sites',
#'   taxon_id_col_name = 'my_spp',
#'   biomass_col_name = 'my_biomass',
#'   standardization_method = 'h',
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
metacommunity_variability <- function(
  data_long = NULL,
  data_wide = NULL,
  time_step_col_name = NULL,
  site_id_col_name = NULL,
  taxon_list = NULL,
  taxon_id_col_name = NULL,
  biomass_col_name = NULL,
  standardization_method = "h", # or 'h'
  variability_type = NULL #'comp' or 'agg'
  ){


  # make data long if provide in wide format
  if(is.null(data_long)){

    # infer taxon_list if it is NULL
    if(is.null(taxon_list)){
      taxon_list <- dplyr::setdiff(names(data_wide), c(time_step_col_name, site_id_col_name))
    }

    # make long
    data_long <- data_wide %>%
      dplyr::select(
        dplyr::any_of(c(site_id_col_name, time_step_col_name, taxon_list))) %>%
      # tidyr::gather(taxon_id, biomass, dplyr::one_of(taxon_list))
      tidyr::pivot_longer(
        cols = dplyr::any_of(taxon_list),
        names_to = "taxon_id",
        values_to = "biomass")

  }else if(!is.null(data_long)){ #otherwise, if already long, identify which col is the taxon names and biomass values
    if(is.null(taxon_id_col_name)) stop('which col name is the taxon names?')
    data_long <- as.data.frame(data_long)
    data_long$taxon_id = data_long[,taxon_id_col_name]

    if(is.null(biomass_col_name)) stop('indicate the name of the biomass column?')
    data_long$biomass = data_long[,biomass_col_name]
  }

  # # calc total metacommunity biomass, needed for BD_hT
  # data_long$total_metacommunity_biomass <- data_long$biomass %>% sum

  # compositinoal variability calculations
  if(grepl('(?i)com', variability_type) |  grepl('(?i)BD', variability_type) | grepl('(?i)beta', variability_type)){

    # use long format data as input -- standardize biomass values using Hellinger or modified Hellinger transforms
    data_long <- data_long %>%
      dplyr::group_by_at(
        dplyr::vars(site_id_col_name, time_step_col_name)) %>%
      dplyr::mutate(
        total_site_biomass_per_time = sum(.data$biomass)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        biomass_standardized = dplyr::case_when(
          tolower(standardization_method) == 'h' ~
            sqrt(.data$biomass/.data$total_site_biomass_per_time) %>%
            ifelse(is.na(.), 0, .)),
        standardization_method = standardization_method)



    # calculate temporal BD by site using temporal_BD() function
    temporal_BD_by_site <- data_long %>%
      tidyr::nest(data = -dplyr::any_of(site_id_col_name)) %>%
      dplyr::mutate(
        BD = purrr::map(
          .x = .$data,
          .f = ltmc::temporal_BD,
          time_step_col_name = time_step_col_name,
          taxon_id_col_name = 'taxon_id',
          biomass_col_name = 'biomass_standardized')
      ) %>%
      dplyr::select(-.data$data) %>%
      tidyr::unnest(cols = c(.data$BD))



    if(tolower(standardization_method) == 'h'){

      # calculate each site's mean total biomass over time
      site_total_biomass_averaged_over_time <- data_long %>%
        dplyr::group_by_at(
          dplyr::vars(site_id_col_name, time_step_col_name)) %>%
        dplyr::summarize(
          total_site_biomass_per_time = sum(.data$biomass)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by_at(
          dplyr::vars(site_id_col_name)) %>%
        dplyr::summarize(
          mean_total_biomass = mean(.data$total_site_biomass_per_time))

      # use site total biomass (averaged over time) to calculate weights
      # for how much each site contributes to alpha variation (alpha var
      # averaged across sites)
      site_total_biomass_averaged_over_time$weights <- site_total_biomass_averaged_over_time$mean_total_biomass / sum(site_total_biomass_averaged_over_time$mean_total_biomass)

      # use weights to average local (alpha) temporal beta diversity across sites
      # weighted by site total biomass (averaged over time)
      joined_site_data <- temporal_BD_by_site %>%
        dplyr::left_join(site_total_biomass_averaged_over_time) %>%
        dplyr::mutate(BD_x_wi = .data$BD * .data$weights)

      # calculate alpha var for the entire metacommunity
      alpha_var <- joined_site_data$BD_x_wi %>% sum

    }else{
      alpha_var <- NA
      stop("please provide a valid 'standardization_method'")
    }


    # regional scale
    data_regional_long <- data_long %>%
      dplyr::group_by_at(
        dplyr::vars(time_step_col_name, .data$taxon_id)) %>%
      dplyr::summarize(
        taxon_agg_biomass = sum(.data$biomass)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::group_by_at(
        dplyr::vars(time_step_col_name)) %>%
      dplyr::mutate(
        total_metacommunity_biomass_by_time = sum(.data$taxon_agg_biomass)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        taxon_regional_biomass_standardize = dplyr::case_when(
          tolower(standardization_method) == 'h' ~
            sqrt(.data$taxon_agg_biomass/.data$total_metacommunity_biomass_by_time)),
        standardization_method = standardization_method)

    gamma_var <- data_regional_long %>%
      ltmc::temporal_BD(
        time_step_col_name = time_step_col_name,
        taxon_id_col_name = 'taxon_id',
        biomass_col_name = 'taxon_regional_biomass_standardize')

    data_out <- data.frame(
      variability_type,
      standardization_method,
      gamma_var,
      alpha_var,
      phi_var = gamma_var / alpha_var)

  }else if(grepl('(i?)agg',variability_type)){
    mu_TT <- data_long %>%
      dplyr::select(
        dplyr::any_of(time_step_col_name), .data$biomass) %>%
      dplyr::group_by_at(
        dplyr::vars(time_step_col_name)) %>%
      dplyr::summarize(agg_metacommunity_biomass_by_time = sum(.data$biomass)) %>%
      dplyr::ungroup() %>%
      dplyr::summarize(mu_TT = mean(.data$agg_metacommunity_biomass_by_time)) %>%
      unlist

    sigma_TT <- data_long %>%
      dplyr::select(
        dplyr::one_of(time_step_col_name), .data$biomass) %>%
      dplyr::group_by_at(
        dplyr::vars(time_step_col_name)) %>%
      dplyr::summarize(agg_metacommunity_biomass_by_time = sum(.data$biomass)) %>%
      dplyr::ungroup() %>%
      dplyr::summarize(mu_TT = stats::sd(.data$agg_metacommunity_biomass_by_time)) %>%
      unlist

    CV2_gamma <- (sigma_TT / mu_TT)^2 %>% unlist

    sd_T_by_site <- data_long %>%
      dplyr::select(
        dplyr::any_of(c(site_id_col_name, time_step_col_name)), .data$biomass) %>%
      dplyr::group_by_at(
        dplyr::vars(site_id_col_name, time_step_col_name)) %>%
      dplyr::summarize(agg_biomass = sum(.data$biomass)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by_at(
        dplyr::vars(site_id_col_name)) %>%
      dplyr::summarize(sd_agg_biomass = stats::sd(.data$agg_biomass))

    CV2_alpha <- (sum(sd_T_by_site$sd_agg_biomass) / mu_TT)^2 %>% unlist
    phi_agg = CV2_gamma / CV2_alpha

    data_out <- data.frame(
      variability_type,
      standardization_method = NA,
      gamma_var = CV2_gamma,
      alpha_var = CV2_alpha,
      phi_var = CV2_gamma / CV2_alpha)
  }


  return(data_out)
} #END FUNCTION

########################################


