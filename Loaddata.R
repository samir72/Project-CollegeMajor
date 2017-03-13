Loaddata <- function(file)
{
#  browser()
    ## Read the csv file
  Dataload <- read.csv(file, header = TRUE,stringsAsFactors = FALSE)
  ## Coerce some character columns to numeric
  #numcols <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
  #auto.price[, numcols] <- lapply(auto.price[, numcols], as.numeric)
  
  ## Remove cases or rows with missing values. In this case we keep the 
  ## rows which do not have nas. 
  Dataload[complete.cases(Dataload), ]
  return(Dataload)
}


