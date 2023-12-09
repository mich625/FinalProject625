FL_combine = function(summaries) {
  # combine each summary from the local servers  by adding
  combined_xtx = Reduce("+", lapply(summaries, function(summary) summary$xtx))
  combined_xty = Reduce("+", lapply(summaries, function(summary) summary$xty))
  combined_yty = Reduce("+", lapply(summaries, function(summary) summary$yty))
  combined_n = sum(sapply(summaries, function(summary) summary$n))
  p = dim(combined_xtx)[1]
  return(list(xtx = combined_xtx, xty = combined_xty, yty = combined_yty, n = combined_n))
}
