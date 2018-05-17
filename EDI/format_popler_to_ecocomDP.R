#' Reformat a popler dataset to the ecocomDP
#'
#' @description  
#'     This function converts a popler dataset 
#'     (https://github.com/AldoCompagnoni/popler) into the ecocomDP format
#'     (https://github.com/EDIorg/ecocomDP/tree/master). This function 
#'     addresses the current needs (2018-04-05) of the LTER metacommunities 
#'     working group and does not populate all the tables of the ecocomDP for 
#'     the sake of expediency. See 
#'
#' @usage format_popler_to_ecocomDP(path.out = "", proj.metadata.key = )
#' 
#' @param path.out
#'     A character string specifying a path to the directory to which the 
#'     reformatted popler datasets will be exported.
#' @param proj.metadata.key
#'     An integer value that is the popler project metadata key 
#'     (i.e. proj_metadata_key) of the dataset to be converted to the ecocomDP.
#' 
#' @return 
#'     The observation, location, and taxon, ecocomDP tables in comma separated
#'     format (.csv) to the specified "path.out". File naming follow the 
#'     format: proj.metadata.key_tableName (e.g. 27_observation, 27_location,
#'     27_taxon for the popler dataset represented by the project metadata key
#'     twenty-seven, and for the observation, location, and taxon tables.
#'     
#' @note 
#'     Some notes about what information is included and missing from the 
#'     ecocomDP converted datasets:
#'     \itemize{
#'         \item{event_id}{The event_id is unique combinations of location_id
#'         and observation_datetime}
#'         \item{package_id}{All package_ids have been assigned a value of NA.
#'         Many of the datasets within popler are not easily traced back to 
#'         data package ids}
#'         \item{taxon_id}{When the popler field 'sppcode' is present, then 
#'         these values are used as the taxon_id. Otherwise, unique IDs are 
#'         created.}
#'         \item{location_name}{These values are the spatial locations pasted
#'         together for each unique nested level.}
#'         \item{latitude}{No attempt was made to extract latitude, and was 
#'         assigned the value NA.}
#'         \item{longitude}{No attempt was made to extract longitude, and was 
#'         assigned the value NA.}
#'         \item{elevation}{No attempt was made to extract elevation, and was 
#'         assigned the value NA.}
#'         \item{authority_system}{No attempt was made to extract authority 
#'         system information and was assigned the value NA.}
#'         \item{authority_taxon_id}{No attempt was made to extract authority 
#'         taxon ID information and was assigned the value NA.}
#'     }
#'
#' @export
#'


# Load required libraries -----------------------------------------------------

library(lubridate)
library(ecocomDP)
library(popler)
library(readr)

format_popler_to_ecocomDP <- function(path.out = '.', 
                                      proj.metadata.key,
                                      write.tables.to.csv = FALSE,
                                      return.tables.as.list = TRUE){
  
  # Validate arguments --------------------------------------------------------
  
  message('Validating arguments')
  
  if (write.tables.to.csv & missing(path.out) & path.out!='.'){
    stop('Input argument "path.out" is missing! Specify the path to the directory that will be filled with ecocomDP versions of popler datasets.')
  }
  if (missing(proj.metadata.key)){
    stop('Input argument "proj.metadata.key" is missing! Specify the project metadata key for your dataset of interest. This is the "proj_metadata_key listed in the popler_catalog."')
  }
  
  validate_path(
    path.out
  )
  
  popler_catalog <- browse(
    full_tbl = T,
    trim = F
  )
  
  use_i <- match(
    proj.metadata.key,
    popler_catalog$proj_metadata_key
  )
  
  if (sum(!is.na(use_i)) == 0){
    stop('The specified project metadata key does not exist in the popler database.')
  }
  
  
  
  # Parameterize --------------------------------------------------------------
  
  message(
    'Loading parameters'
    )
  
  ranks <- c(
    'kingdom',
    'subkingdom',
    'infrakingdom',
    'superdivision',
    'division',
    'subdivision',
    'superphylum',
    'phylum',
    'subphylum',
    'class',
    'subclass',
    'order',
    'family',
    'genus',
    'species'
  )
  
  spatial_level_labels <- c(
    'spatial_replication_level_1_label',
    'spatial_replication_level_2_label',
    'spatial_replication_level_3_label',
    'spatial_replication_level_4_label',
    'spatial_replication_level_5_label'
    )
  
  spatial_level_values <- c(
    'spatial_replication_level_1',
    'spatial_replication_level_2',
    'spatial_replication_level_3',
    'spatial_replication_level_4',
    'spatial_replication_level_5'
  )
  
  
    
  # Load data ---------------------------------------------------------------
  
  message(
    paste0(
      'Loading popler dataset (proj_metadata_key = ',
      as.character(
        proj.metadata.key
        ),
      ')'
      )
    )
  
  data <- suppressMessages(
    eval(
      parse(
        text = paste0(
          'get_data(proj_metadata_key == ',
          as.character(
            proj.metadata.key
            ),
          ')'
          )
        )
      )
    )
  
  if ('covariates' %in% colnames(data)){
    data <- suppressMessages(
      eval(
        parse(
          text = paste0(
            'get_data(proj_metadata_key == ',
            as.character(
              proj.metadata.key
              ),
            ', cov_unpack = T)'
          )
        )
      )
    )
  }
  
  
  
  # Create observation table (1 of 2) ---------------------------------------

  message(
    'Building observation table (1 of 2)'
    )
  
  # Create column: observation_datetime
  
  if (('year' %in% colnames(data)) & 
      ('month' %in% colnames(data)) &
      ('day' %in% colnames(data))){
    
    observation_datetime <- paste(
      data$year,
      data$month,
      data$day,
      sep = '-'
    )
    
    datetime_format_str <- 'YYYY-MM-DD'
    
    observation_datetime <- ymd(
      observation_datetime
      )
    
  } else if (('year' %in% colnames(data)) & 
             ('month' %in% colnames(data))){
    
    observation_datetime <- paste(
      data$year,
      data$month,
      sep = '-'
    ) # !convert to date object!
    
    datetime_format_str <- 'YYYY-MM'

  } else if (('year' %in% colnames(data))){
    
    observation_datetime <- data$year # !convert to date object!
    
    datetime_format_str <- 'YYYY'
    
  } else if (('month' %in% colnames(data)) &
             ('day' %in% colnames(data))){
    
    yr <- rep(
      'YYYY',
      nrow(data)
    )
    
    observation_datetime <- paste(
      yr,
      data$month,
      data$day,
      sep = '-'
    )
    
  }
  
  # Create column: variable_name
  
  variable_name <- data$datatype
  
  # Create column: value
  
  value <- data$abundance_observation
  
  # Create column: unit
  
  if ('samplingunits' %in% colnames(data)){
    
    unit <- data$samplingunits
    
  } else {
    
    unit <- rep(
      NA,
      length(
        observation_datetime
        )
      )
    
  }
  
  # Create column: taxon_id
  
  if (is.null(data$sppcode)){
    taxon_id <- rep(
      NA,
      nrow(
        data
      )
    )
  } else {
    taxon_id <- data$sppcode
  }

  
  
  # Create location table ---------------------------------------------------
  
  message(
    'Building location table'
    )
  
  # Index spatial columns
  
  col_i <- match(
    c(
      rbind(
        spatial_level_labels, 
        spatial_level_values
        )
      ),
    colnames(
      data
      )
    )
  
  col_i <- col_i[!is.na(col_i)]
  
  # Initialize storage space for intermediary dataframes
  
  df <- rep(
    list(
      c()
      ), 
    (length(
      col_i)/2)
    )
  
  # Continue ... if spatial data is present
  
  if (length(col_i) > 0){
    
    # Make keys in observation table for linking to the location table.
    
    data$keys <- apply( 
      data[ , col_i],
      1,
      paste,
      collapse = "_"
    )
    
    # Form location working table. This table is an intermediary between 
    # the observation and location tables. Copy in data$keys.
    
    k <- length(col_i)/2
    
    df[[k]] <- data[ , col_i]
    
    df[[k]]$key <- data$keys
    
    df[[k]] <- unique.data.frame(
      df[[k]]
      )
    
    # Make location_id for most nested location
    
    df[[k]][ , paste0('srl_key_', k)] <- paste0(
      k,
      '_',
      seq(
        nrow(
          df[[k]]
          )
        )
      )
    
    # Make similar tables for coarser spatial scales
    
    if (length(col_i) > 2){
      
      for (j in ((length(col_i)/2) - 1):1){
        
        # Create table of next coarser spatial scale
        
        df[[j]] <- data[ , col_i[1:(j*2)]]
        
        # Get unique elements of this table
        
        df[[j]] <- unique.data.frame(
          df[[j]]
        )
        
        # Create location keys for this level
        
        df[[j]][ , paste0('srl_key_', j)] <- paste0(
          j,
          '_',
          seq(
            nrow(
              df[[j]]
            )
          )
        )
        
        # Create keys to finer nested tables (i.e. j + 1)
        # Create key at this level and create at next finer level.
        # This links info here to the parent of the next finer level.
        
        df[[j]]['key'] <- apply( # Create key at this level
          df[[j]][ , 1:(ncol(df[[j]])-1)],
          1,
          paste,
          collapse = '_'
        )
        
        df[[j+1]][paste0('key_to_L', j)] <- apply( # Create same key in next finer level 
          df[[j+1]][ , 1:length(col_i[1:(j*2)])],
          1,
          paste,
          collapse = '_'
        )
        
        # Add srl_j_key to level = j+1 (essentially we are adding the parent_location_id)
        
        index <- match(
          df[[j+1]][[paste0('key_to_L', j)]],
          df[[j]][['key']]
        )
        
        df[[j+1]]['parent'] <- df[[j]][[paste0('srl_key_', j)]][index]
        
      }
      
    } else {
      
      j <- 1
      
    }
    
    # Coarsest level does not have a parent.
    # Add NA as parent value.
    
    df[[j]]['parent'] <- rep(
      NA_character_,
      nrow(
        df[[j]]
      )
    )
    
    # Compile the location table.
    
    location_id <- c() # initialize vectors
    location_name <- c()
    latitude <- c()
    longitude <- c()
    elevation <- c()
    parent_location_id <- c()
    
    for (j in (length(col_i)/2):1){ # Populate the vectors
      
      location_id <- c(
        location_id,
        df[[j]][[paste0('srl_key_', j)]]
      )
      
      location_name <- c(
        location_name,
        df[[j]][['key']]
      )
      
      latitude <- c(
        latitude,
        rep(
          NA_character_,
          nrow(
            df[[j]]
          )
        )
      )
      
      longitude <- c(
        longitude,
        rep(
          NA_character_,
          nrow(
            df[[j]]
          )
        )
      )
      
      elevation <- c(
        elevation,
        rep(
          NA_character_,
          nrow(
            df[[j]]
          )
        )
      )
      
      parent_location_id <- c(
        parent_location_id,
        df[[j]][['parent']]
      )

    }
    
    # Assemble into data.frame and arrange
    
    location_table <- data.frame(
      location_id,
      location_name,
      latitude,
      longitude,
      elevation,
      parent_location_id,
      stringsAsFactors = F
    )
    
    location_table <- arrange(
      location_table,
      location_id
    )
    

  }
  
  
  
  # Create observation table (2 of 2) ---------------------------------------
  
  message(
    'Building observation table (2 of 2)'
    )
  
  # Create location_id in observation table (i.e. table named 'data')
  # Use 'keys' field to link to location table.
  
  index <- match(
    data[['keys']],
    df[[(length(col_i)/2)]][['key']]
  )
  
  data[['location_id']] <- df[[(length(col_i)/2)]][[paste0('srl_key_', (length(col_i)/2))]][index]
  
  # Create event_id
  
  df_event <- data.frame( # make data.frame to detect unique datetimes at location_ids
    observation_datetime,
    data$location_id
  )
  
  df_event_id <- df_event
  
  df_event <- unique.data.frame(df_event)
  
  df_event$key <- apply( # create key to observation table
    df_event[ , 1:2],
    1,
    paste,
    collapse = '_'
  )
  
  df_event_id$key <- apply( # create key from observation table (or table that will feed into observation table)
    df_event_id[ , 1:2],
    1,
    paste,
    collapse = '_'
  )
  
  df_event$event_id <- paste0( # make event_id
    'ev_',
    seq(
      nrow(df_event)
    )
  )
  
  index <- match( # Add event_id to observation table (or table that will feed into observation table)
    df_event_id$key,
    df_event$key
  )
  
  df_event_id$event_id <- df_event$event_id[index]
  
  # Create package_id (!this should be updated to reflect data package these data were obtained from!)
  
  data$package_id <- rep(
    NA_character_,
    nrow(data)
  )
  
  # Create observation_id
  
  data$observation_id <- paste0( # make observation_id
    'ob_',
    seq(
      nrow(data)
    )
  )
  
  # Compile observation table
  
  observation <- data.frame(
    observation_id = data$observation_id,
    event_id = df_event_id$event_id,
    package_id = data$package_id,
    location_id = data$location_id,
    observation_datetime = observation_datetime,
    taxon_id = taxon_id,
    variable_name = variable_name,
    value = value,
    unit = unit,
    stringsAsFactors = F
  )
  
  
  
  # Create taxon table ------------------------------------------------------
  #
  # More detailed taxonomic information needs to be incorporated here.
  # For the time being taxa codes are being used.
  
  # Create rudimentary taxon table for manipulations
  
  message('Building taxon table')
  
  # Create species code (sppcode) if not present
  
  col_i <- match(
    'sppcode',
    colnames(
      data
    )
  )
  
  if (is.na(col_i)){
    
    col_i <- match(
      ranks,
      colnames(
        data
      )
    )
    
    col_i <- col_i[!is.na(col_i)]
    
    if (length(col_i) == 1){
      data$tid <- data[ , col_i]
    } else {
      data$tid <- apply(
        data[ , col_i],
        1,
        paste,
        collapse = ' '
      )
    }
    
    key <- unique(
      data$tid
      )
    
    tid <- paste0(
      'tx_',
      seq(
        length(
          key
          )
        )
      )
    
    index <- match(
      data$tid,
      key
    )
    
    data$sppcode <- tid[index]
    
    col_i <- match( # reorder sppcode
      'sppcode',
      colnames(
        data
        )
      )
    
    col_i <- col_i[!is.na(col_i)]
    
    data <- data[ , c(col_i, 1:(ncol(data)-1))]
    
    observation$taxon_id <- data$sppcode # update observation table with taxon ID

  }
  
  
  # Index taxonomic columns including sppcode
  
  col_i <- match(
    c(
      'sppcode',
      ranks
      ),
    colnames(
      data
    )
  )
  
  col_i <- col_i[!is.na(col_i)]
  
  if (length(col_i) == 1){ # If only species codes are present ...
    
    df_taxon <- data.frame(
      sppcodes = unique(
        data[ , col_i]
      )
    )
    
    # Create taxon_rank
    
    df_taxon$taxon_rank <- rep(
      NA_character_,
      nrow(df_taxon)
    )
    
    # Create taxon_name
    
    df_taxon$taxon_name <- rep(
      NA_character_,
      nrow(df_taxon)
    )
    
  } else { # If more rank specific info is present in addition to species codes ...
    
    df_taxon <- unique.data.frame(
      data[ , col_i]
    )
    
    # Create vectors of taxon table
    
    taxon_id <- df_taxon$sppcode # taxon_id
    
    mat_i <- df_taxon != 'NA' # taxon_rank
    c_i <- seq(length(col_i)-1)
    mat_n <- as.data.frame(
      matrix(
        rep(
          c_i,
          each = nrow(
            df_taxon
          )
        ),
        nrow = nrow(
          df_taxon
        )
      )
    )
    holder <- mat_n*NA
    
    if (ncol(holder) == 1){
      index <- as.logical(mat_i[ ,2])
      holder[index, ] <- colnames(df_taxon[2])[1]
      df_taxon$taxon_rank <- holder[ ,1]
    } else {
      holder[mat_i[ ,2:ncol(mat_i)]] <- mat_n[mat_i[ , 2:ncol(mat_i)]]
      index <- suppressWarnings(apply(holder, 1, function(x) max(x, na.rm = T)))
      df_taxon$taxon_rank <- colnames(df_taxon[2:ncol(df_taxon)])[index]
    }
    
    
    if (sum(c('genus', 'species') %in% colnames(df_taxon)) == 2){ # Create taxon_name
      
      c_i <- match(c('genus', 'species'), colnames(df_taxon))
      c_i <- c_i[!is.na(c_i)]
      binom <- df_taxon[c_i]
      mat_i <- binom == 'NA'
      index <- suppressWarnings(apply(mat_i, 1, function(x) sum(x, na.rm = T)))
      index <- as.logical(index == 0)
      
      df_taxon$taxon_name[index] <- apply(
        binom[seq(nrow(df_taxon))[index], ],
        1,
        paste,
        collapse = ' '
      )

      # Work through other taxonomic levels
      
      taxa_names <- lapply(
        seq(nrow(mat_i)),
        function(x) df_taxon[x, match(
          df_taxon$taxon_rank[x],
          colnames(df_taxon)
        )]
        )
      taxa_names <- as.character(taxa_names)
      
      index <- (df_taxon$taxon_rank != 'species') & (!is.na(df_taxon$taxon_rank))
      df_taxon$taxon_name[index] <- taxa_names[index]
      
    } else if (ncol(holder) == 1){
      
      df_taxon$taxon_name <- rep(
        NA,
        nrow(df_taxon)
      )
      df_taxon$taxon_name[index] <- df_taxon[index ,2]
      
    } else {
    
      # Work through other taxonomic levels
      
      taxa_names <- lapply(
        seq(nrow(mat_i)),
        function(x) df_taxon[x, match(
          df_taxon$taxon_rank[x],
          colnames(df_taxon)
        )]
      )
      taxa_names <- as.character(taxa_names)
      
      index <- (df_taxon$taxon_rank != 'species') & (!is.na(df_taxon$taxon_rank))
      df_taxon$taxon_name[index] <- taxa_names[index]

    }
    
  }
  
  
  # Create authority_system
  
  df_taxon$authority_system <- rep(
    NA_character_,
    nrow(df_taxon)
  )
  
  # Create authority_taxon_id
  
  df_taxon$authority_taxon_id <- rep(
    NA_character_,
    nrow(df_taxon)
  )
  
  # Compile taxon table
  
  taxon_table <- data.frame(
    taxon_id = df_taxon$sppcode,
    taxon_rank = df_taxon$taxon_rank,
    taxon_name = df_taxon$taxon_name,
    authority_system = df_taxon$authority_system,
    authority_taxon_id = df_taxon$authority_taxon_id,
    stringsAsFactors = F
  )
  
  
  # Clean up tables and write to file ---------------------------------------
  
  if(write.tables.to.csv){
    message('Writting ecocomDP tables')
    
    # Write observation table
    
    write_csv(observation,
              path = paste0(
                path.out,
                '\\',
                proj.metadata.key,
                '_observation.csv'
              )
    )
    
    # Write location table
    
    write_csv(location_table,
              path = paste0(
                path.out,
                '\\',
                proj.metadata.key,
                '_location.csv'
              )
    )
    
    # Write taxon table
    
    write_csv(taxon_table,
              path = paste0(
                path.out,
                '\\',
                proj.metadata.key,
                '_taxon.csv'
              )
    )
  }
 
  
  # return tables
  return(
    list('observation_table' = observation,
         'location_table' = location_table,
         'taxon_table' = taxon_table)
  )
  
}

