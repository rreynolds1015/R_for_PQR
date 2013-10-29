parse.PMDB.data <- function (df,new.lots=NULL,analysis.level="Tier 3",where="screen"){
  # Get basic information about how many parameters there are
  parameters <- length(df)
  
  num.parameters <- c(1)
  j <- 1
  for (i in 1:parameters) {
    test <- as.numeric(as.vector(df[[i]]))
    test.2 <- is.na(test[4]) && is.na(test[5])
    if (!is.na(test[6]) && test.2==FALSE) {
      num.parameters[j] <- i
      j <- j + 1
    }
  }
  for (i in 1:length(num.parameters)) {
    release.analysis(data.set=df[[num.parameters[i]]],label.set=df[[1]],
                     analysis.level=analysis.level, new.lots=new.lots,where=where)
  }
}
