test_that("defaultDbPath uses ~ expansion outside Windows", {
  local_mocked_bindings(.isWindows = function() FALSE)

  expect_identical(
    defaultDbPath(),
    path.expand("~/IMR_biotic_BES_database")
  )
  expect_identical(
    defaultDbPath(folder = "custom_folder"),
    path.expand("~/custom_folder")
  )
})

test_that("defaultDbPath uses USERPROFILE on Windows", {
  local_mocked_bindings(.isWindows = function() TRUE)
  withr::local_envvar(USERPROFILE = "C:/Users/tester")

  expect_identical(
    defaultDbPath(),
    "C:/Users/tester/IMR_biotic_BES_database"
  )
})

test_that("defaultDbPath falls back to ~ when USERPROFILE is unset", {
  local_mocked_bindings(.isWindows = function() TRUE)
  withr::local_envvar(USERPROFILE = "")

  expect_identical(
    defaultDbPath(),
    path.expand("~/IMR_biotic_BES_database")
  )
})

test_that("dbPathCandidates lists both historical Windows locations", {
  local_mocked_bindings(.isWindows = function() TRUE)
  withr::local_envvar(USERPROFILE = "C:/Users/tester")

  candidates <- dbPathCandidates()

  expect_length(candidates, 2)
  expect_identical(candidates[[1]], "C:/Users/tester/IMR_biotic_BES_database")
  expect_identical(candidates[[2]], path.expand("~/IMR_biotic_BES_database"))
})

test_that("dbPathCandidates returns a single path outside Windows", {
  local_mocked_bindings(.isWindows = function() FALSE)

  expect_identical(dbPathCandidates(), path.expand("~/IMR_biotic_BES_database"))
})

test_that("findDatabase returns NULL when no database exists", {
  local_mocked_bindings(.isWindows = function() FALSE)
  withr::local_envvar(HOME = withr::local_tempdir())

  expect_null(findDatabase())
})

test_that("findDatabase prefers the current default over the legacy location", {
  home <- withr::local_tempdir()
  profile <- withr::local_tempdir()

  dir.create(file.path(home, "IMR_biotic_BES_database"))
  dir.create(file.path(profile, "IMR_biotic_BES_database"))
  file.create(file.path(home, "IMR_biotic_BES_database", "bioticexplorer.duckdb"))
  file.create(file.path(profile, "IMR_biotic_BES_database", "bioticexplorer.duckdb"))

  local_mocked_bindings(.isWindows = function() TRUE)
  withr::local_envvar(HOME = home, USERPROFILE = profile)

  expect_identical(
    findDatabase(),
    normalizePath(
      file.path(profile, "IMR_biotic_BES_database", "bioticexplorer.duckdb"),
      winslash = "/", mustWork = FALSE
    )
  )
})

test_that("findDatabase falls back to the legacy location and honours dbName", {
  home <- withr::local_tempdir()
  profile <- withr::local_tempdir()

  dir.create(file.path(home, "IMR_biotic_BES_database"))
  file.create(file.path(home, "IMR_biotic_BES_database", "bioticexplorer.duckdb"))

  local_mocked_bindings(.isWindows = function() TRUE)
  withr::local_envvar(HOME = home, USERPROFILE = profile)

  expect_identical(
    findDatabase(),
    normalizePath(
      file.path(home, "IMR_biotic_BES_database", "bioticexplorer.duckdb"),
      winslash = "/", mustWork = FALSE
    )
  )
  expect_null(findDatabase(dbName = "does_not_exist"))
})
