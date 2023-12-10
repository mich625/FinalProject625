##test case for FL_Local_Summary


test_that("FL", {
  data(mtcars)
  FL_test = FL(Purchase ~ age + income +income:age, data1)
  #expect_equal(mk_test[1], 39.686261, tolerance = 0.000001)
  #expect_equal(mk_test[2], -3.190972, tolerance = 0.000001)

})


##We need to load our dataset here, not sure how to do that.
#I think this is where we can show oracle vs our FL function




##to test run devtools::test() in the console
##at the end run the test for coverage (want 100%) -> usethis::use_coverage() & usethis::use_github_action ("test coverage")
##for continuous integration use usethis::use_github_actions() then copy badge and add it to the readME
