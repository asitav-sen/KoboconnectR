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


test_that("a list of length 4 is downloaded", {
  expect_length(kobotools_kpi_data(assetid="akANpTUN6aB4wXADscfRSy", url="kobo.humanitarianresponse.info", uname="scary_scarecrow", pwd="sybWE6USkFxDsr4"),4)
})

test_that("error if not 200", {
  expect_error(kobotools_kpi_data(assetid="akANpTUN6aB4wXADscfRSy", url="humanitarianresponse.info", uname="scary_scarecrow", pwd="sybWE6USkFxDsr4"))
})






