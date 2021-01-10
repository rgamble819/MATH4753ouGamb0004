mathMode <- function(x)
{
  uniqueVals=unique(x)
  uniqueVals[which.max(tabulate(match(x,uniqueVals)))]
}
