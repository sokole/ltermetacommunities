# Aldo Compagnoni: script to retrieve time series data using
# the ecoretriever package. 
# Found 9 time series data sets.
library(devtools)
install_github("ropensci/ecoretriever")

# Attach the package and the database
library(ecoretriever)
library("RSQLite")

# List the datasets available via the Retriever
ecoRetDataList=ecoretriever::datasets()

# Found a few temporal data sets.
temporalData=c("Adler2007","Zachmann2010","McGlinn2010","DelMoral2010",
            "PortalMammals","Woods2009","Palmer2007","Steppe_plants_2013",
            "TreeWesternGhats")

# Install data sets as databases
for(i in 1:length(temporalData)){
  
  ecoretriever::install(dataset = paste0(temporalData[i]), 
                        connection = 'sqlite', 
                        db_file = paste0(temporalData[i],
                                         ".sqlite1"))
}

# Access one of the data sets using SQLite
db <- dbConnect(SQLite(), paste0(temporalData[1],".sqlite1"))

# List tables within the database
dbListTables(db)

# List fields within a particular table
dbListFields(db, dbListTables(db)[1])

# Check out a table
head(dbGetQuery(db, paste0("SELECT * FROM ", 
                           dbListTables(db)[6])))

# Now, consult the metadata, query and format the data set! 
