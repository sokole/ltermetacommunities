# ----------------------------------
# Tools -- CODE WRITTEN BY LAURENT HALLETT, ADAPTED FROM ERIC SOKOL
# ----------------------------------
# 

# 
#' Read file from Google Drive using the google id hash
#' adapted from https://github.com/sokole/ltermetacommunities/blob/master/examples/SOKOL-RScript-reading-from-google-drive.R
#'
#' @param file_id_gdrive Google Drive csv file id A character
#' @param gdrive_url Google Drive URL A character
#'
#' @return data frame with the csv content A data frame
#' @export
#'
#' @examples 
#' my_data <- read_csv_gdrive("0B7AABlvKD6WjSTY3YUVKZ1AwLWs")
#' 
read_csv_gdrive <- function(file_id_gdrive, skipper=0, gdrive_url="https://drive.google.com/uc?export=download&id="){
  # Create the full URL for the files
  download.link <- paste0(gdrive_url,file_id_gdrive)
  # Import the csv as Data frame
  data.table <- read.csv(file = download.link, header = T, skip = skipper)
  return(data.table)
}



#' A function to apply the fill_zeros_uniqueID function across a whole site 
#' This will fill in zeros for years when a species was absent
#' @param df A dataframe with the following columns
#' @param year A column for year in the data frame
#' @param site The LTER site
#' @param habitat The habitat type
#' @param project The project name
#' @param plot The plot name@name 
#' @param subplot The subplot name
#' @param uniqueID The uniqueID, ie the lowest level of replication
#' @param unitAbund The unit abundance is measured in
#' @param scaleAbund The scale abundance is measured at
#' @param species The species column
#' @param abundance The abundance column
#'
#' @return data frame with the same columns as df, with the zeros filled in when a species was present in the plot but not the year
#' @import tidyr, dplyr
#' @export
#'
#' @examples 
fill_zeros <- function(df, year = "year", site = "site", habitat = "habitat",
                       project = "project", plot = "plot", subplot = "subplot",
                       uniqueID = "uniqueID", unitAbund = "unitAbund", 
                       scaleAbund = "scaleAbund", species = "species", abundance = "abundance") {
  X <- split(df, df$uniqueID)
  out <- lapply(X, FUN=fill_zeros_uniqueID)
  ID <- unique(names(out))
  out <- mapply(function(x, y) "[<-"(x, "uniqueID", value = y) ,
                out, ID, SIMPLIFY = FALSE)
  zerodat <- do.call("rbind", out) %>%
    tbl_df()
  tomerge <- df %>%
    select(year, site, habitat, project, plot, subplot, uniqueID, unitAbund, scaleAbund, species) %>%
    unique()
  zerodat2 <- merge(tomerge, zerodat, all.y = T)
  return(zerodat2)
}


### Internal function for fill_zeros()
#' A function to fill in 0s for species present in the plot but not that year
#' @param df a dataframe with the following columns
#' @param year A column for year in the data frame
#' @param species The species column
#' @param abundance The abundance column
#'
#' @return data frame with a column for year, species, and abundance, with the zeros filled in when a species was present in the uniqueID but not the year
#' @import tidyr, dplyr
#' @examples 
fill_zeros_uniqueID <- function (df, year = "year", species = "species", abundance = "abundance") {
  nosp <- length(unique(df[,species]))
  df2 <- df[c(year, species, abundance)] %>%
    spread(species, abundance, fill=0) 
  df3 <- df2 %>%
    gather(species, abundance, 2:ncol(df2))
  return(df3)
}

