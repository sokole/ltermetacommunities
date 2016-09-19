## Thomas LAMY
## set the working directory 
setwd("~/Data/ltermetacomm")

## set the working directory 
source("./Group1-diversity-metrics/betadivLTER.R")
source("./Group1-diversity-metrics/lcbdLTER.R")

## -- Generate random dataset for test -- ##
## assumes row blocks corresponding to times
## Number of site = s
s = 20
## Number of years = t
t = 10
## total number of observation
n = s*t
## generate the random data for 10 species
Y = matrix(0,(n),10)
for (i in 1:n) Y[i,1:10] = floor(exp(rnorm(10, 0, 1.8)))

## -- Load real dataset for the test -- ##
download.link <- "https://drive.google.com/uc?export=download&id=0BxUZSA1Gn1HZYTVfd2FZTWhWbm8"
sbc.data <- read.csv(file = download.link, header = T)
sbc.data <- droplevels(subset(sbc.data, OBSERVATION_TYPE=="TAXON_COUNT"))
## GOL has only NA values
sbc.data[is.na(sbc.data$VALUE),]
## remove GOL
sbc.data <- droplevels(subset(sbc.data, SITE_ID!="GOL"))
## formating a wide community data table
library(reshape2)
sbc.wide <- dcast(sbc.data, SITE_ID + DATE ~ VARIABLE_NAME, value.var="VALUE")
plot(as.numeric(sbc.wide$SITE_ID), sbc.wide$DATE)
## focus on years 2004 -> 2016
sbc.wide <- subset(sbc.wide, DATE %in% c(2004:2016))
## Formating data, create blocks corresponding to times
sbc.wide <- sbc.wide[order(sbc.wide$SITE_ID),]
sbc.wide <- sbc.wide[order(sbc.wide$DATE),]

## running function on SBC dataset
s = length(unique(sbc.wide$SITE_ID))
t = length(unique(sbc.wide$DATE))
Y = sbc.wide[,3:dim(sbc.wide)[2]]
library(vegan)
Y.h = decostand(Y, "hellinger")

## need package gplots !
test1 <- lcbdLTER(Y, s, t, method="%difference", plot=TRUE)
test2 <- betadivLTER(Y, s=s, t=t, plot=TRUE)



