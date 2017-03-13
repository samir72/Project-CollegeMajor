ZScoreNormalise <- function(Column)
{
  #browser()
  #logColumn <- as.numeric(log(na.omit(Column)))
  mean_logColumn <- mean(Column, na.rm = TRUE)
  sd_logColumn <- sd(Column, na.rm = TRUE)
  normalized_logColumn <- (Column - mean_logColumn) / sd_logColumn
  mean_normalized_logColumn <- mean(normalized_logColumn,na.rm = TRUE)
  Normalized_df <- data.frame(normalized_logColumn,mean_normalized_logColumn)
  return(Normalized_df)
}


