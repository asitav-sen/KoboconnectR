test_that("url empty conditions works", {
  expect_error(kobotools_kpi_data(url=""))
})


test_that("username empty condition works", {
  expect_error(kobotools_kpi_data(uname=""))
})

test_that("password empty condition works", {
  expect_error(kobotools_kpi_data(pwd=""))
})

test_that("assetid empty condition works", {
  expect_error(kobotools_kpi_data(assetid=""))
})

test_that("url non string conditions works", {
  expect_error(kobotools_kpi_data(url=1234))
})

test_that("username non string conditions works", {
  expect_error(kobotools_kpi_data(uname=1234))
})

test_that("password non string conditions works", {
  expect_error(kobotools_kpi_data(pwd=1234))
})

test_that("password non string conditions works", {
  expect_error(kobotools_kpi_data(assetid=1234))
})
