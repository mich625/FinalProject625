##pushing to git
##git remote add origin git@github.com:mich625/HW_3.git   ##this probably not needed if connection already set
##git branch -M main
##git push -u origin main

##R Oxygen2 code
## ????
##
## To create readme
## roxygen2::roxygenize()

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

FL_combine = function(summaries) {
  # combine each summary from the local servers  by adding
  combined_xtx = Reduce("+", lapply(summaries, function(summary) summary$xtx))
  combined_xty = Reduce("+", lapply(summaries, function(summary) summary$xty))
  combined_yty = Reduce("+", lapply(summaries, function(summary) summary$yty))
  combined_n = sum(sapply(summaries, function(summary) summary$n))
  p = dim(combined_xtx)[1]
  return(list(xtx = combined_xtx, xty = combined_xty, yty = combined_yty, n = combined_n))
}

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



####------- Example
set.seed(42)  # For reproducibility

# Simulating dataset1
age1 = rnorm(100, mean = 50, sd = 10)  # simulating age
income1 = rnorm(100, mean = 50000, sd = 10000)  # simulating income
noise1 = rnorm(100, 0, 1)  # some noise
Purchase1 = 20000 + 500*age1 + 0.05*income1 + 10*age1*income1/100000 + noise1  # creating a relationship for Purchase

data1 = data.frame(age = age1, income = income1, Purchase = Purchase1)
rm(age1, income1, noise1, Purchase1)

# Simulating dataset2 in a similar way
age2 = rnorm(100, mean = 50, sd = 10)
income2 = rnorm(100, mean = 50000, sd = 10000)
noise2 = rnorm(100, 0, 1)
Purchase2 = 20000 + 500*age2 + 0.05*income2 + 10*age2*income2/100000 + noise2

data2 = data.frame(age = age2, income = income2, Purchase = Purchase2)
rm(age2, income2, noise2, Purchase2)

summary1 = FL_local_summary(Purchase ~ age + income +income:age, data1)
summary2 = FL_local_summary(Purchase ~ age + income + income:age, data2)

combined_summaries = FL_combine(list(summary1, summary2))

soln = FL(combined_summaries)

data= rbind(data1,data2)
#compare with lm function
summary(lm(Purchase ~ age + income +income:age, data=data))
##--------------------------


###--------------- Another complicated example----------------------------------

generate_data = function(n, mean_age, sd_age, mean_income, sd_income, intercept, age_effect, income_effect, noise_effect) {
  age = rnorm(n, mean = mean_age, sd = sd_age)  # simulating age
  income = rnorm(n, mean = mean_income, sd = sd_income)  # simulating income
  group = as.factor(sample(LETTERS[1:3], n, replace = TRUE))  # simulating group
  noise = rnorm(n, 0, noise_effect)  # some noise
  Purchase = intercept + age_effect * age + income_effect * income  +  noise + ifelse(group == 'A', 1000, (ifelse(group =='B', 2000, 3000))) # creating a relationship for Purchase

  data = data.frame(age = age, income = income, group = group, Purchase = Purchase)

  return(data)
}

# Generate four datasets
data1 = generate_data(100, 50, 10, 50000, 10000, 20000, 500, 0.05, 1)
data2 = generate_data(100, 45, 15, 45000, 15000, 25000, 600, 0.04, 1)
data3 = generate_data(100, 55, 8, 55000, 5000, 21000, 400, 0.03, 1)
data4 = generate_data(100, 42, 12, 40000, 8000, 26000, 550, 0.06, 1)

# Summary stats for the datasets
summary1 = FL_local_summary(Purchase ~ age + income + group + age:income + age:group + income:group, data1)
summary2 = FL_local_summary(Purchase ~ age + income + group + age:income + age:group + income:group, data2)
summary3 = FL_local_summary(Purchase ~ age + income + group + age:income + age:group + income:group, data3)
summary4 = FL_local_summary(Purchase ~ age + income + group + age:income + age:group + income:group, data4)

# Combine summaries
combined_summaries = FL_combine(list(summary1, summary2, summary3, summary4))

# Federate learnin
soln= FL(combined_summaries)

# Print out the beta_hat
print(soln)

data = rbind(data1, data2,data3,data4)
# compare with lm function
summary(lm(Purchase ~ age + income + group + age:income + age:group + income:group, data=data))
