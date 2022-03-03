test_that("Goodness of fit test", {
  x=c(12,9,10,7,12); prob=c(1/5,1/5,1/5,1/5,1/5)
  expect_equal(gofchisq(x=x,p=prob),gofchisq(x,prob))
})
