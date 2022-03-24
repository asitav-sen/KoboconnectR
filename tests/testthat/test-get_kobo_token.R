test_that("url empty conditions works", {
  expect_error(get_kobo_token(url=""))
})


test_that("username empty condition works", {
  expect_error(get_kobo_token(uname=""))
})

test_that("password empty condition works", {
  expect_error(get_kobo_token(pwd=""))
})

test_that("url non string conditions works", {
  expect_error(get_kobo_token(url=1234))
})

test_that("username non string conditions works", {
  expect_error(get_kobo_token(uname=1234))
})

test_that("password non string conditions works", {
  expect_error(get_kobo_token(pwd=1234))
})


test_that("correct token is returned", {
  expect_equal(unlist(get_kobo_token(url="kobo.humanitarianresponse.info", uname="scary_scarecrow", pwd="sybWE6USkFxDsr4")), c(token="d7a1faf575e047d4ebb1519469454297dcd012ed"))
})
