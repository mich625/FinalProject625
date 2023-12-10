#'FL
#'
#'This function will take all the local server summaries and combine them in preparation for
#'the federated learning analysis.  It requires all the local server summaries as an input, these
#'will be exactly what the R Shiny app produces.
#'
#'
#'@param summary this is the combined summary statistics output from the FL_combine function.
#'
#'@return a table is returned that specifies the beta estimate, standard error, t statistic, and p value for each covariate.
#'
#'@examples
#'data(mtcars)
#'mk_lm(Y = mtcars$mpg, X = cbind(mtcars$wt, mtcars$cyl), X_names = c("weight", "cylinders"))
#'mk_lm(Y = mtcars$mpg, X = matrix(mtcars$wt, ncol = 1), X_names = c("weight"))
#'
#'@export
#'

FL = function(summary) {
  # Get the estimates and standard errors of the coefficients
  beta_hat = solve(summary$xtx) %*% (summary$xty)
  SSE =  summary$yty - 2*(t(beta_hat) %*% summary$xty) + (t(beta_hat) %*% summary$xtx %*% beta_hat)
  sigmasq_hat = SSE / (summary$n - length(beta_hat))

  # Compute covariance matrix
  cov_mat =  as.numeric(sigmasq_hat) * solve(summary$xtx)

  # Compute standard error, t values and p values
  std_error = sqrt(diag(cov_mat))
  t_values = beta_hat / std_error
  p_values = 2 * pt(-abs(t_values), df = summary$n - length(beta_hat))

  # Create a dataframe for output
  result = data.frame(Estimate = beta_hat,
                      `Std. Error` = std_error,
                      `t value` = t_values,
                      `p value` = p_values)

  rownames(result) = colnames(summary$xtx)

  return(result)
}
