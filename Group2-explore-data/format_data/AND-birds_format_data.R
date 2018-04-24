library(tidyverse)
setwd("LTER-Data/AND-birds/")
data <- read.csv(file = "SA02402_v1.csv")
out <- data.frame(OBSERVATION_TYPE = "", 
                  SITE_ID = "",
                  DATE = "",
                  VARIABLE_NAME = "",
                  VARIABLE_UNITS = "",
                  VALUE = "")
unique(cbind(data$PLOT, data$REPLICATE))
head(data)
dat1 <- data %>% group_by(YEAR, PLOT, SPECIES) %>% 
  summarize(VALUE = n())
out <- cbind.data.frame(OBSERVATION_TYPE = "TAXON_COUNT",
                        SITE_ID = dat1$PLOT,
                        DATE = dat1$YEAR,
                        VARIABLE_NAME = dat1$SPECIES,
                        VARIABLE_UNITS = "count",
                        VALUE = dat1$VALUE
)

write.csv()
