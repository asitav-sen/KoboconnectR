test_that("url empty conditions works", {
  expect_error(kobotools_api(url=""))
})


test_that("username empty condition works", {
  expect_error(kobotools_api(uname=""))
})

test_that("password empty condition works", {
  expect_error(kobotools_api(pwd=""))
})

test_that("url non string conditions works", {
  expect_error(kobotools_api(url=1234))
})

test_that("username non string conditions works", {
  expect_error(kobotools_api(uname=1234))
})

test_that("password non string conditions works", {
  expect_error(kobotools_api(pwd=1234))
})
