test_that("valid acns are identified", {
  expect_true(is_acn("000000019"))
  expect_false(is_acn("000000018"))
})

# TODO: test treatment of numeric vectors


test_that("white space characters don't interrupt evaluation", {
  # single spaces between numbers
  expect_true(is_acn("000 250 000"))
  expect_false(is_acn("000 250 001"))

  # multiple spaces between numbers
  expect_true(is_acn("0  01 00 0 004"))
  expect_false(is_acn("0  01 00 0 005"))

  # spaces at start and end of number
  expect_true(is_acn("  001 250 004 "))
  expect_false(is_acn("  001 250 005 "))

  # combination of spaces and tabs
  expect_true(is_acn("	001 5 00 009"))
  expect_false(is_acn("	001 5 00 010"))
})

test_that("is_acn is vectorised", {
  expect_identical(
    is_acn(c("002 999 993", "003 249 993", "003 499 992")),
    c(TRUE, FALSE, TRUE)
  )
})
