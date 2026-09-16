fixture_zip <- test_path("fixtures", "rti-06075.zip")

lfs_pointer_text <- function(file, sha256 = NULL) {
  if (is.null(sha256)) {
    sha256 <- if (requireNamespace("digest", quietly = TRUE)) {
      digest::digest(file = file, algo = "sha256")
    } else {
      strrep("0", 64)
    }
  }
  c(
    "version https://git-lfs.github.com/spec/v1",
    paste0("oid sha256:", sha256),
    paste0("size ", file.size(file))
  )
}

# Serves the fixture in place of GitHub and counts the zip downloads.
# `zip` overrides what the zip download does, e.g. to simulate an interruption.
local_rti_mock <- function(pointer = lfs_pointer_text(fixture_zip), zip = NULL,
                           env = parent.frame()) {
  calls <- new.env()
  calls$zip <- 0
  calls$urls <- character()
  local_mocked_bindings(
    rti_download_file = function(url, destfile, quiet = FALSE) {
      calls$urls <- c(calls$urls, url)
      if (startsWith(url, "https://raw.githubusercontent.com/")) {
        writeLines(pointer, destfile)
      } else {
        calls$zip <- calls$zip + 1
        if (is.null(zip)) {
          file.copy(fixture_zip, destfile, overwrite = TRUE)
        } else {
          zip(url, destfile)
        }
      }
      invisible(destfile)
    },
    .env = env
  )
  calls
}

local_cache_dir <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  withr::local_options(helios.cache_dir = dir, .local_envir = env)
  dir
}

cache_contents <- function(dir) {
  list.files(dir, recursive = TRUE, all.files = TRUE, include.dirs = TRUE)
}

test_that("rti_population() rejects numeric FIPS with a hint", {
  expect_error(rti_population(6075), "leading zeros")
  expect_error(rti_population(6), "leading zeros")
})

test_that("rti_population() rejects malformed FIPS", {
  expect_error(rti_population("6075"), "2-digit state or 5-digit county")
  expect_error(rti_population("060750"), "2-digit state or 5-digit county")
  expect_error(rti_population("0607a"), "2-digit state or 5-digit county")
  expect_error(rti_population(c("06", "07")), "2-digit state or 5-digit county")
  expect_error(rti_population(NA_character_), "2-digit state or 5-digit county")
})

test_that("rti_population() rejects unknown versions and bad refresh", {
  expect_error(rti_population("06075", version = "2019"), "\"2010_ver1\"")
  expect_error(rti_population("06075", refresh = NA), "`refresh`")
})

test_that("rti_zip_path() builds county and state paths", {
  expect_equal(rti_zip_path("06075", "2010_ver1"), "2010/County/06/2010_ver1_06075.zip")
  expect_equal(rti_zip_path("06", "2010_ver1"), "2010/State/2010_ver1_06.zip")
})

test_that("rti_cache_dir() defaults to the user cache and can be overridden", {
  withr::local_options(helios.cache_dir = NULL)
  expect_equal(rti_cache_dir(), tools::R_user_dir("helios", which = "cache"))
  withr::local_options(helios.cache_dir = "somewhere")
  expect_equal(rti_cache_dir(), "somewhere")
})

test_that("rti_population() downloads, reduces and caches a county", {
  dir <- local_cache_dir()
  calls <- local_rti_mock()

  pop <- rti_population("06075")

  expect_equal(calls$zip, 1)
  expect_equal(calls$urls, c(
    "https://raw.githubusercontent.com/RTIInternational/SyntheticPopulations/main/2010/County/06/2010_ver1_06075.zip",
    "https://media.githubusercontent.com/media/RTIInternational/SyntheticPopulations/main/2010/County/06/2010_ver1_06075.zip"
  ))
  # Only the cache entry is left behind
  expect_equal(cache_contents(dir), c("rti", "rti/2010_ver1", "rti/2010_ver1/06075.rds"))

  expect_s3_class(pop, "helios_synthetic_population")
  expect_named(pop, c("people", "schools", "workplaces", "metadata"))
  expect_equal(pop$metadata, list(
    source = "RTI International U.S. Synthetic Population",
    version = "2010_ver1",
    fips = "06075",
    n_people = 27L,
    n_households = 8L
  ))

  people <- pop$people
  expect_named(people, c("household_id", "age", "school_id", "workplace_id"))
  for (column in people) expect_type(column, "integer")
  expect_equal(nrow(people), 27)
  expect_equal(sort(unique(people$household_id)), 1:8)
  expect_equal(max(table(people$household_id)), 12)
  expect_equal(people$age[1:4], c(40L, 38L, 10L, 7L))

  # Empty ids are NA and the rest are renumbered by row of the schools/workplaces files
  expect_equal(people$school_id[1:6], c(NA, NA, 1L, 1L, NA, 2L))
  expect_equal(people$workplace_id[1:6], c(1L, 2L, NA, NA, NA, 3L))
  expect_equal(sum(!is.na(people$school_id) & !is.na(people$workplace_id)), 1)
  expect_equal(sum(!is.na(people$school_id)), 10)
  expect_equal(sum(!is.na(people$workplace_id)), 11)

  expect_equal(pop$schools, data.frame(school_id = 1:3, reference_size = c(350L, 1200L, 40L)))
  expect_equal(
    pop$workplaces,
    data.frame(workplace_id = 1:5, reference_size = c(12L, 3L, 250L, 40L, 5L))
  )
})

test_that("a cached population is read without network access", {
  local_cache_dir()
  calls <- local_rti_mock()
  first <- rti_population("06075")

  local_mocked_bindings(rti_download_file = function(...) stop("no network"))
  expect_identical(rti_population("06075"), first)
  expect_equal(calls$zip, 1)
})

test_that("refresh = TRUE downloads again", {
  local_cache_dir()
  calls <- local_rti_mock()
  first <- rti_population("06075")
  second <- rti_population("06075", refresh = TRUE)
  expect_equal(calls$zip, 2)
  expect_identical(second, first)
})

test_that("versions and FIPS codes are cached separately", {
  dir <- local_cache_dir()
  calls <- local_rti_mock()
  rti_population("06075")
  expect_false(file.exists(file.path(dir, "rti", "2010_ver1", "06.rds")))
  expect_error(rti_population("06"), "missing: 2010_ver1_06_synth_people.txt")
  expect_equal(calls$zip, 2)
})

test_that("cache entries in an old format are downloaded again", {
  dir <- local_cache_dir()
  calls <- local_rti_mock()
  path <- file.path(dir, "rti", "2010_ver1", "06075.rds")
  dir.create(dirname(path), recursive = TRUE)
  saveRDS(list(format = 0L, population = "stale"), path)

  pop <- rti_population("06075")
  expect_s3_class(pop, "helios_synthetic_population")
  expect_equal(calls$zip, 1)
  expect_equal(readRDS(path)$population, pop)
})

test_that("an interrupted download leaves no cache entry", {
  dir <- local_cache_dir()
  local_rti_mock(zip = function(url, destfile) {
    writeBin(readBin(fixture_zip, "raw", n = 100), destfile)
    stop("connection reset")
  })
  expect_error(rti_population("06075"), "connection reset")
  expect_equal(cache_contents(dir), c("rti", "rti/2010_ver1"))

  # A later call succeeds
  local_rti_mock()
  expect_s3_class(rti_population("06075"), "helios_synthetic_population")
})

test_that("an interruption does not overwrite an existing cache entry on refresh", {
  dir <- local_cache_dir()
  local_rti_mock()
  first <- rti_population("06075")

  local_rti_mock(zip = function(url, destfile) stop("connection reset"))
  expect_error(rti_population("06075", refresh = TRUE), "connection reset")
  expect_equal(cache_contents(dir), c("rti", "rti/2010_ver1", "rti/2010_ver1/06075.rds"))
  expect_identical(readRDS(file.path(dir, "rti", "2010_ver1", "06075.rds"))$population, first)
})

test_that("a truncated download is rejected by its size", {
  dir <- local_cache_dir()
  local_rti_mock(zip = function(url, destfile) {
    writeBin(readBin(fixture_zip, "raw", n = 100), destfile)
  })
  expect_error(rti_population("06075"), "size 100 bytes but should have")
  expect_equal(cache_contents(dir), c("rti", "rti/2010_ver1"))
})

test_that("a download with the wrong checksum is rejected", {
  skip_if_not_installed("digest")
  dir <- local_cache_dir()
  local_rti_mock(pointer = lfs_pointer_text(fixture_zip, sha256 = strrep("a", 64)))
  expect_error(rti_population("06075"), "sha256")
  expect_equal(cache_contents(dir), c("rti", "rti/2010_ver1"))
})

test_that("an unknown FIPS gives an error naming it", {
  dir <- local_cache_dir()
  local_mocked_bindings(rti_download_file = function(url, destfile, quiet = FALSE) {
    stop("HTTP status was '404 Not Found'")
  })
  expect_error(rti_population("99999"), "FIPS \"99999\".*404")
  expect_equal(cache_contents(dir), c("rti", "rti/2010_ver1"))
})

test_that("a malformed LFS pointer is an error", {
  local_cache_dir()
  local_rti_mock(pointer = "<html>not a pointer</html>")
  expect_error(rti_population("06075"), "Unexpected Git LFS pointer")
})

test_that("rti_download_file() restores the timeout option", {
  withr::local_options(timeout = 60)
  seen <- NULL
  local_mocked_bindings(
    download.file = function(url, destfile, mode, quiet) {
      seen <<- getOption("timeout")
      0L
    },
    .package = "utils"
  )
  rti_download_file("https://example.com", tempfile())
  expect_equal(seen, 3600)
  expect_equal(getOption("timeout"), 60)
})

test_that("read_csv_columns() reads the same with and without data.table", {
  skip_if_not_installed("data.table")
  dir <- withr::local_tempdir()
  file <- utils::unzip(fixture_zip, files = "2010_ver1_06075_synth_people.txt", exdir = dir)
  columns <- c("sp_hh_id", "age", "sp_school_id", "sp_work_id")
  expect_equal(
    read_csv_columns(file, columns, use_fread = TRUE),
    read_csv_columns(file, columns, use_fread = FALSE)
  )
})

test_that("read_csv_columns() treats empty and X as missing and errors on missing columns", {
  csv <- withr::local_tempfile(lines = c(
    "sp_id,school_id,work_id,other",
    "1,X,502309163,a",
    "2,450000001,,b"
  ))
  x <- read_csv_columns(csv, c("work_id", "school_id"), use_fread = FALSE)
  expect_equal(x, data.frame(work_id = c(502309163L, NA), school_id = c(NA, 450000001L)))
  expect_error(read_csv_columns(csv, c("sp_id", "sp_work_id")), "missing column\\(s\\): sp_work_id")
})

test_that("rti_population() downloads a real county", {
  # Check the opt-in first: skip_if_offline() itself uses the network
  skip_if(Sys.getenv("HELIOS_TEST_RTI_DOWNLOAD") != "true", "Set HELIOS_TEST_RTI_DOWNLOAD=true to run")
  skip_on_cran()
  skip_if_offline()
  local_cache_dir()

  # Kent County, Delaware: a 5 MB download
  pop <- rti_population("10001")
  expect_equal(pop$metadata$n_people, 157282)
  expect_equal(pop$metadata$n_households, 60278)
  expect_false(anyNA(pop$people$household_id))
  expect_false(anyNA(pop$people$age))
})
