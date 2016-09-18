context("Balance at death")

test_that("Plausible value for balance", {
  expect_equal(BalanceAfterRealLevelPayments(nominal_payment = 10e3, n = 10, balance = 100e3 + 1, r_earnings = 0, cpi = 0, inArrears = TRUE),
               1)
})
