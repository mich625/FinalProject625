#'FL_Local_Summary
#'
#'This function will produce the local server federated learning summary statistics.  It requires an input of the
#'linear regression formula and a csv data set.  It will then run a linear regression and output the necessary
#'summary statistics (XtX matrix, XtY matrix, YtY matrix, & n)
#'
#'
#'@param formula linear regression formula being used for the study
#'@param data csv file containing all the specified data fro the study.
#'
#'@return a list containing the summary statistics (XtX matrix, XtY matrix, YtY matrix, & n) is returned
#'
#'@examples
#'data(Aetna)
#'FL_local_summary(Billing.Amount ~ Age + Gender + Medical.Condition + Admission.Type + Medication, Aetna)
#'
#'
#'@export
#'

FL_local_summary = function(formula, data) {
  # Adjust the contrast settings to use one-hot encoding for factors
  options(contrasts = c("contr.sum", "contr.poly"))

  # model.matrix automatically adds an intercept
  model_matrix = model.matrix(formula, data)

  # Automatically get the outcome variable name from the formula
  outcome = as.character(formula(formula)[[2]])

  xtx = crossprod(model_matrix)
  xty = crossprod(model_matrix, data[[outcome]])
  yty = crossprod(data[[outcome]])
  n = nrow(data)

  return(list(xtx = xtx, xty = xty, yty = yty, n = n))
}
