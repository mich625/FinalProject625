##test case for FL_Local_Summary


test_that("FL_local_summary", {
  data(mtcars)
  local_test = FL_local_summary(Purchase ~ age + income +income:age, data1)
  #expect_equal(mk_test[1], 39.686261, tolerance = 0.000001)
  #expect_equal(mk_test[2], -3.190972, tolerance = 0.000001)

})

##Not sure if anything really makes sense here, nothing to compare to



##to test run devtools::test() in the console
##at the end run the test for coverage (want 100%) -> usethis::use_coverage() & usethis::use_github_action ("test coverage")
##for continuous integration use usethis::use_github_actions() then copy badge and add it to the readME
