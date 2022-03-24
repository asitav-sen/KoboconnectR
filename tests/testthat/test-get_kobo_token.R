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
