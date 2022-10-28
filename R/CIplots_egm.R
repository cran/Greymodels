# Extended grey model

#' A function
#'
#' @param actual2 is the original sequence
#' @param fp1 is the fitted and predicted sequence
#' @param ci is the confidence level (90, 95, 99)
#'
#' @return
#' @importFrom stats qt
#' @importFrom utils tail
#' @importFrom Metrics rmse
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @export


# EGM (1, 1) model

# Confidence interval (CI) of predicted values

CIvalue <- function(fp1,actual2,x,ci){

  fp2 <- t(fp1)

  w <- length(fp2)

  n <- length( actual2)

  fitted1 <- fp2[1:n]
  fitted2 <- tail(fp1,4)

  predicted <- t(fitted2[1:x])

  t <- length(predicted)
  s <- rmse(actual2, fitted1)
  sse <- sum((actual2 - fitted1)^2)
  mse <- sse / (n - 2)

  cc <- (ci + 100)/200
  t.val <- qt(cc, n - 2)

  u <- numeric(t)
  l <- numeric(t)
  for (i in 1:t) {
    u[i] = predicted[i] + (t.val * (sqrt(mse) * sqrt(i)))
    l[i] = predicted[i] - (t.val * (sqrt(mse) * sqrt(i)))
  }
  LowerBound <- c(l[1:t])
  UpperBound <- c(u[1:t])
  CIset <- data.frame(LowerBound,UpperBound)
  return(CIset)

}

#' A function
#'
#' @param x0 is the original sequence
#' @param x0cap2 is the fitted and predicted sequence
#'
#' @return
#' @importFrom stats qt
#' @importFrom utils tail
#' @importFrom Metrics rmse
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @export




# Plots

plotegm <- function(x0,x0cap2,ci){

  n <- length(x0)
  fitted2 <- t(x0cap2)
  x0cap <- x0cap2[1:n]
  fitted3 <- tail(x0cap2,4)
  x0cap5 <- fitted3

  w <- length(x0cap2)
  t <- length(x0cap5)

  s <- rmse(x0, x0cap)
  sse <- sum((x0 - x0cap)^2)
  mse <- sse / (n - 2)
  cc <- (ci + 100)/200
  t.val <- qt(cc, n - 2)

  u <- numeric(t)
  l <- numeric(t)
  for (i in 1:t) {
    u[i] = x0cap5[i] + (t.val * (sqrt(mse) * sqrt(i)))
    l[i] = x0cap5[i] - (t.val * (sqrt(mse) * sqrt(i)))
  }
  UB <- c(u[1:t])
  LB <- c(l[1:t])

  LB1 <- c(x0cap[n],LB)
  UB2 <- c(x0cap[n],UB)

  l1 <- length(LB1)
  d3 <- seq(1, l1, 1)

  u1 <- length(UB2)
  d4 <- seq(1, u1, 1)

  set3 <- data.frame(x=d3, y=LB1)
  set4 <- data.frame(x=d4, y=UB2)

  d0 <- seq(1, n, 1)
  xy1 <- data.frame(x=d0, y=x0)

  d1 <- seq(1, w, 1)
  xy2 <- data.frame(x=d1, y=x0cap2)

  df <- rbind(xy1, xy2, set3, set4)
  colors <- c("Original Data" = "red", "Fitted and Predicted Data" = "blue", "Lower Bound CI" = "green", "Upper Bound CI" = "yellow")
  CI <- c(n:w)

  x=y=NULL

  p <- ggplot(df) +
    theme_bw() +
    labs(title = 'EGM (1, 1) model',x = 'Number of observation',y = 'Data Forecast & Prediction') +
    scale_x_continuous(breaks=1:w) +
    scale_y_continuous(labels = scales::comma) +
    geom_point(data = xy1, aes(x = x, y = y), shape = 24, color = "black") +
    geom_point(data = xy2, aes(x = x, y = y), shape = 21, color = "black") +
    geom_point(data = set3, aes(x = CI, y = y), shape = 23, color = "black") +
    geom_point(data = set4, aes(x = CI, y = y), shape = 23, color = "black") +
    geom_line(data = xy1, aes(x = x, y = y,color = "Original Data")) +
    geom_line(data = xy2, aes(x = x, y = y,color = "Fitted and Predicted Data")) +
    geom_line(data = set3, aes(x = CI, y = y,color = "Lower Bound CI"), linetype=2) +
    geom_line(data = set4, aes(x = CI, y = y,color = "Upper Bound CI"), linetype=2) +
    scale_color_manual(name = "Label",values = colors)
  r <- ggplotly(p)
  return(r)

}


