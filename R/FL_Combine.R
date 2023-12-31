#'FL_Combine
#'
#'This function will take all the local server summaries and combine them in preparation for
#'the federated learning analysis.  It requires all the local server summaries as an input, these
#'will be exactly what the R Shiny app produces.
#'
#'
#'@param summaries the outputs from the FL_local_summary which contains all local level summary statistics that will be used in this analysis
#'
#'@return the combined list of summary statistics is returned
#'
#'@examples
#'data(Aetna)
#'data(United.Sum)
#'Aetna.Sum = FL_local_summary(Billing.Amount ~ Age + Gender + Medical.Condition + Admission.Type + Medication, Aetna)
#`United.Sum = FL_local_summary(Billing.Amount ~ Age + Gender + Medical.Condition + Admission.Type + Medication, UnitedHealthcare)
#'FL_combine(list(Aetna.Sum, United.Sum))
#'
#'@export
#'

FL_combine = function(summaries) {
  # combine each summary from the local servers  by adding
  combined_xtx = Reduce("+", lapply(summaries, function(summary) summary$xtx))
  combined_xty = Reduce("+", lapply(summaries, function(summary) summary$xty))
  combined_yty = Reduce("+", lapply(summaries, function(summary) summary$yty))
  combined_n = sum(sapply(summaries, function(summary) summary$n))
  p = dim(combined_xtx)[1]
  return(list(xtx = combined_xtx, xty = combined_xty, yty = combined_yty, n = combined_n))
}
