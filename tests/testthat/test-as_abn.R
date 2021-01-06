test_that("ABN conversion from non-zero first digit codes works", {
  expect_equal(as_abn(824753556), "51824753556")
  expect_equal(as_abn("824 753 556"), "51824753556")
  expect_equal(as_abn("824 753 556"), as_abn(824753556))
})
