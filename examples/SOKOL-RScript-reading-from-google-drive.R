# ----------------------------------
# Eric's working script with examples
# ----------------------------------

# ----------------------------------
# -- download data off google drive using id
# ----------------------------------
data_id_googledrive <- "0B2P104M94skvQVprSnBsYjRzVms" #NWT data id
download.link <- paste0("https://drive.google.com/uc?export=download&id=",data_id_googledrive)
data.table <- read.csv(file = download.link, header = T)
# ----------------------------------





