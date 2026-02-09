estat_names <- c("Min","Q1","Med","Media","Q3",
                 "Max","DP","CV","Skn","Krt")
estat_desc <- function(x){
  x<-na.omit(x)
  m <- mean(x,na.rm = TRUE)
  md <- median(x)
  mini <- min(x,na.rm = TRUE)
  q1 <- quantile(x,.25)
  q3 <- quantile(x,.75)
  maxi <- max(x,na.rm = TRUE)
  dp <- sd(x,na.rm = TRUE)
  cv <- 100*dp/m
  ass <- agricolae::skewness(x)
  curt <- agricolae::kurtosis(x)
  c(mini,q1,md,m,q3,maxi,dp,cv,ass,curt)
}
