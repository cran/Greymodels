# Multivariate grey models

#' A function
#'
#' @param fp1
#' @param actual
#' @param x
#' @param model ci
#'
#' @return
#' @importFrom stats qt
#' @importFrom utils tail
#' @importFrom Metrics rmse
#' @importFrom dplyr slice_tail
#' @export

# Confidence interval of predicted values

# GM (1, 3) and IGM (1, 3) models

CIvalue <- function(fp1,actual,x,ci){

  # fp1 is the sequence of fitted and predicted values
  # actual is the original data
  # x is the number of values chosen to forecast
  # ci is the confidence level

  actual2 <- t(actual)

  n <- length(actual2)

  fp2 <- t(fp1)

  w <- length(fp2)

  n <- length(actual2)

  fitted1 <- fp2[1:n]

  fitted2 <- tail(fp1,4)

  predicted <- t(fitted2[1:x])

  t <- length(predicted)

  # Root mean square error (rmse)
  s <- rmse(actual2, fitted1)

  # Sum of square error
  sse <- sum((actual2 - fitted1)^2)

  # Mean square error
  mse <- sse / (n - 2)

  # Calculate confidence interval
  cc <- (ci + 100)/200

  t.val <- qt(cc, n - 2)

  u <- numeric(t)
  l <- numeric(t)
  for (i in 1:t) {
    u[i] = predicted[i] + (t.val * (sqrt(mse) * sqrt(i)))
    l[i] = predicted[i] - (t.val * (sqrt(mse) * sqrt(i)))
  }
  UpperBound <- c(u[1:t])
  LowerBound <- c(l[1:t])

  CIset <- data.frame(LowerBound,UpperBound)
  return(CIset)

}



CI_nhmgmp <- function(fp1,x01,x02,x,ci){

  n <- length(x01)
  p1 <- fp1[,1]
  p2 <- fp1[,2]
  fitted_1 <- p1[1:n]
  fitted_2 <- p2[1:n]
  forecast_1a <- tail(p1,4)
  forecast_2a <- tail(p2,4)
  forecast_1 <- forecast_1a[1:x]
  forecast_2 <- forecast_2a[1:x]
  x0cap2_x1 <- c(fitted_1,forecast_1 )
  x0cap2_x2 <- c(fitted_2,forecast_2 )
  w <- length(x0cap2_x1)
  t <- length(forecast_1)
  s_x1 <- rmse(x01, fitted_1)
  s_x2 <- rmse(x02, fitted_2)
  sse_x1 <- sum((x01 - fitted_1)^2)
  mse_x1 <- sse_x1 / (n - 2)
  sse_x2 <- sum((x02 - fitted_2)^2)
  mse_x2 <- sse_x2 / (n - 2)
  cc <- (ci + 100)/200
  t.val <- qt(cc, n - 2)
  u_x1 <- numeric(t)
  l_x1 <- numeric(t)
  for (i in 1:t) {
    u_x1[i] = forecast_1[i] + (t.val * (sqrt(mse_x1) * sqrt(i)))
    l_x1[i] = forecast_1[i] - (t.val * (sqrt(mse_x1) * sqrt(i)))
  }
  UB_x1 <- c(u_x1[1:t])
  LB_x1 <- c(l_x1[1:t])
  LB1_x1 <- c(fitted_1[n],LB_x1)
  UB2_x1 <- c(fitted_1[n],UB_x1)
  l1_x1 <- length(LB1_x1)
  d3_x1 <- seq(1, l1_x1, 1)
  u1_x1 <- length(UB2_x1)
  d4_x1 <- seq(1, u1_x1, 1)
  set3_x1 <- data.frame(x=d3_x1, y=LB1_x1)
  set4_x1 <- data.frame(x=d4_x1, y=UB2_x1)
  u_x2 <- numeric(t)
  l_x2 <- numeric(t)
  for (i in 1:t) {
    u_x2[i] = forecast_2[i] + (t.val * (sqrt(mse_x2) * sqrt(i)))
    l_x2[i] = forecast_2[i] - (t.val * (sqrt(mse_x2) * sqrt(i)))
  }
  UB_x2 <- c(u_x2[1:t])
  LB_x2 <- c(l_x2[1:t])
  LB1_x2 <- c(fitted_2[n],LB_x2)
  UB2_x2 <- c(fitted_2[n],UB_x2)
  l1_x2 <- length(LB1_x2)
  d3_x2 <- seq(1, l1_x2, 1)
  u1_x2 <- length(UB2_x2)
  d4_x2 <- seq(1, u1_x2, 1)
  set3_x2 <- data.frame(x=d3_x2, y=LB1_x2)
  set4_x2 <- data.frame(x=d4_x2, y=UB2_x2)
  CIset <- matrix(c(LB_x1,UB_x1,LB_x2,UB_x2),ncol=4)
  colnames(CIset, do.NULL = FALSE)
  colnames(CIset) <- c("LB Data 1","UB Data 1","LB Data 2","UB Data 2")
  return(CIset)

}


