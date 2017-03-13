MutateColumn <- function(df, column1, column2)
{
  browser()
  mut_df = mutate(df, column1 = column1, column2 = column2)
  return(mut_df)
}


