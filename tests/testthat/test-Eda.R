good <- list(database = "superchouettedb", host = "localhost", port = 5432,
             user = "moidabord", password = "monpetitsecret")

test_that("Eda is created from a base list", {
  obj <- new("Eda", base = good, root_path = tempdir())
  expect_s4_class(obj, "Eda")
  expect_equal(obj@base$host, "localhost")
})

test_that("Eda is created from credentials.yml", {
  skip_if_not_installed("yaml")
  dir <- withr::local_tempdir()
  yaml::write_yaml(good, file.path(dir, "credentials.yml"))
  obj <- new("Eda", root_path = dir)
  expect_equal(obj@base$database, "d")
})

test_that("missing credentials file gives a clear error", {
  dir <- withr::local_tempdir()
  expect_error(new("Eda", root_path = dir), "No `base` supplied")
})

test_that("validity catches bad base", {
  expect_error(new("Eda", base = good[-1]), "database")
  bad <- good; bad$port <- "x" # A character
  expect_error(new("Eda", base = bad, root_path = tempdir()), "port")
})

test_that("show hides the password", {
  obj <- new("Eda", base = good, root_path = tempdir())
  out <- capture.output(show(obj))
  # there shouldn't be anything other than stars in the show method
  expect_false(any(grepl("^\\s*password:\\s*[^*]+$", out[grepl("password", out)])))
  expect_true(any(grepl("^\\s*password: \\*{4}$", out)))
})
