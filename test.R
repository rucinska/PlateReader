library(outliers)

# Function to detect outliers with Grubbs test in a vector
grubbs.flag <- function(vector) {
  outliers <- NULL
  test <- vector
  grubbs.result <- grubbs.test(test)
  pv <- grubbs.result$p.value
  # throw an error if there are too few values for the Grubb's test
  if (length(test) < 3 ) stop("Grubb's test requires > 2 input values")
  na.vect <- test
  while(pv < 0.05) {
    outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    test <- vector[!vector %in% outliers]
    # stop if all but two values are flagged as outliers
    if (length(test) < 3 ) {
      warning("All but two values flagged as outliers")
      break
    }
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
    idx.outlier <- which(vector %in% outliers)
    na.vect <- replace(vector, idx.outlier, NA)
    
  }
  return(na.vect)
}




test <- data.frame(data_diff[,3:6])
apply(test,2,grubbs.flag)

