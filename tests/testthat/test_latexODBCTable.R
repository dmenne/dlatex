context("latex output from ODBC Table")

test_that("Reading Access database gives valid output", {
  skip_on_travis()
  database = system.file("extdata", "testtable.accdb", package = "Dlatex")
  tb = capture.output(latexODBCTable(database, table = "Subject"))
  expect_true(length(tb)==15)
  expect_match(tb[8],"Menne&Dieter&\\$60\\$")
  expect_match(tb[9],"Erdaz&Milhan&\\$30\\$")
  expect_match(tb[13],"\\\\label\\{tab:subject\\}\\}\\}")
})

