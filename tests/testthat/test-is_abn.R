test_that("valid ABNs are identified", {
  expect_true(is_abn(51824753556))
  expect_false(is_abn(12345678910))
})

test_that("character vectors are treated correctly", {
  expect_true(is_abn("50110219460"))
  expect_false(is_abn("50110219461"))
})

test_that("white space characters don't interrupt evaluation", {
  # single spaces between numbers
  expect_true(is_abn("53 930 548 027"))
  expect_false(is_abn("53 930 548 028"))

  # multiple spaces between numbers
  expect_true(is_abn("7  9 769 4 24 86 1"))
  expect_false(is_abn("7  9 769 4 24 86 2"))

  # spaces at start and end of number
  expect_true(is_abn("  8 7 235 24  8 116 "))
  expect_false(is_abn("  8 7 235 24  8 117 "))

  # combination of spaces and tabs
  expect_true(is_abn("83  783 686 257"))
  expect_false(is_abn("83  783 686 258"))
})

test_that("is_abn is vectorised", {
  expect_identical(
    is_abn(c(40978973457, 99644068914, 36643591119)),
    c(TRUE, FALSE, TRUE)
  )

  expect_identical(
    is_abn(c("40978973457", "99644068914", "36643591119")),
    c(TRUE, FALSE, TRUE)
  )
})
