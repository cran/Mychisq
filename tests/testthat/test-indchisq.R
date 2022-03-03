test_that("Independence test", {
  v <- c(80,60,150,50,40,20); X<- matrix(v,ncol=2,byrow = TRUE)
  expect_equal(indchisq(X), indchisq(X,conf.level = 0.95))
})
