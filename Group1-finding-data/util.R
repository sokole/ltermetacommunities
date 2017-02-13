# Utility functins to format data

# Function to order columns according to data format
order_col <- function(x) {
  out <- select(x,
                OBSERVATION_TYPE,SITE_ID,DATE,
                VARIABLE_NAME,VARIABLE_UNITS,
                VALUE)
  return(out)
}

# Function to format data
format_data <- function(x, OBSERVATION_TYPE, SITE_ID, DATE, 
                       VARIABLE_NAME, VARIABLE_UNITS, VALUE){
  
  # Select relevant information
  tmp <- x[,c(SITE_ID,VALUE)]
  # Give "correct" names to columns
  names(tmp) <- c("SITE_ID","VALUE")
  # Generate all remaining columns
  tmp <- mutate(tmp, 
                OBSERVATION_TYPE = OBSERVATION_TYPE,
                DATE = DATE,
                VARIABLE_NAME = VARIABLE_NAME,
                VARIABLE_UNITS = VARIABLE_UNITS)
  
  out <- order_col(tmp)
  return(out)
   
}
                       