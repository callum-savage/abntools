test_that("ABN conversion from non-zero first digit codes works", {
  expect_equal(as_abn(824753556), "51824753556")
  expect_equal(as_abn("824 753 556"), "51824753556")
  expect_equal(as_abn("824 753 556"), as_abn(824753556))
})

test_that("ABN conversion from zero first digit codes works", {
  expect_equal(as_abn(19), "89000000019")
  expect_equal(as_abn("000 000 001"), "80000000001")
  expect_equal(as_abn("042"), as_abn(42))
})

test_that("ABN conversion works with a vector of ABNs", {
  expect_equal(as_abn(c(1234, 5678)), c("18000001234", "29000005678"))
})

test_that("codes longer than 9 characters raise an appropriate error", {
  expect_error(as_abn(123456789123))
  expect_equal(as_abn(123456789123, truncate = TRUE), "91456789123")
})
