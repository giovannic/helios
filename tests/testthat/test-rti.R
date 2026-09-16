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
    catch_all_schools = "synthetic",
    synthetic_school_size = 141,
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

  # The catch-all school (4th in the file) is replaced by a synthetic school, last
  expect_equal(pop$schools, data.frame(school_id = 1:5))
  expect_equal(pop$workplaces, data.frame(workplace_id = 1:5))
})

test_that("rti_population() checks catch_all_schools and synthetic_school_size", {
  expect_error(rti_population("06075", catch_all_schools = "drop"), "\"synthetic\" or \"nearest\"")
  expect_error(rti_population("06075", catch_all_schools = NA), "\"synthetic\" or \"nearest\"")
  expect_error(rti_population("06075", synthetic_school_size = 0), "whole number, at least 1")
  expect_error(rti_population("06075", synthetic_school_size = 10.5), "whole number, at least 1")
  expect_error(rti_population("06075", synthetic_school_size = NA_real_), "whole number, at least 1")
  expect_error(rti_population("06075", synthetic_school_size = c(10, 20)), "whole number, at least 1")
})

test_that("catch-all students are put in synthetic schools by default", {
  local_cache_dir()
  local_rti_mock()
  people <- rti_population("06075")$people

  # The large household: 12- and 9-year-olds were in the catch-all, 6- and 4-year-olds at school 3
  large <- people$household_id == 6
  expect_equal(people$age[large], c(50L, 48L, 18L, 16L, 14L, 12L, 9L, 6L, 4L, 1L, 0L, 75L))
  expect_equal(people$school_id[large], c(NA, NA, NA, 2L, 2L, 5L, 5L, 3L, 3L, NA, NA, NA))
  expect_equal(people$school_id[people$household_id == 8], c(NA, NA, 5L))
})

test_that("synthetic schools take households in block group order", {
  local_cache_dir()
  local_rti_mock()
  pop <- rti_population("06075", synthetic_school_size = 1)
  people <- pop$people

  # Household 8 is in an earlier block group than household 6, so it fills the first school.
  # Household 6's two students stay together, above the size of 1.
  expect_equal(people$school_id[people$household_id == 8], c(NA, NA, 5L))
  expect_equal(people$school_id[people$household_id == 6][6:7], c(6L, 6L))
  expect_equal(pop$schools, data.frame(school_id = 1:6))
  expect_equal(pop$metadata$synthetic_school_size, 1)
})

test_that("catch_all_schools = \"nearest\" uses the nearest private school teaching the grade", {
  local_cache_dir()
  local_rti_mock()
  pop <- rti_population("06075", catch_all_schools = "nearest")
  people <- pop$people

  # Household 6 lives by school 5 (4th after removing the catch-all), which teaches grades
  # 1-12. Household 8's preschooler also lives by it, but goes to school 3, the only private
  # preschool.
  large <- people$household_id == 6
  expect_equal(people$school_id[large], c(NA, NA, NA, 2L, 2L, 4L, 4L, 3L, 3L, NA, NA, NA))
  expect_equal(people$school_id[people$household_id == 8], c(NA, NA, 3L))

  expect_equal(pop$schools, data.frame(school_id = 1:4))
  expect_equal(pop$metadata$catch_all_schools, "nearest")
  expect_equal(pop$metadata$synthetic_school_size, NA_real_)
})

test_that("both methods keep every person, household and workplace", {
  local_cache_dir()
  local_rti_mock()
  synthetic <- rti_population("06075")
  nearest <- rti_population("06075", catch_all_schools = "nearest")
  expect_equal(synthetic$people[-3], nearest$people[-3])
  expect_equal(synthetic$workplaces, nearest$workplaces)
  expect_equal(!is.na(synthetic$people$school_id), !is.na(nearest$people$school_id))
})

test_that("rti_read_extract() keeps what's needed to move catch-all students", {
  dir <- withr::local_tempdir()
  files <- utils::unzip(fixture_zip, exdir = dir)
  names(files) <- sub("^2010_ver1_06075_(.*)\\.txt$", "\\1", basename(files))
  extract <- rti_read_extract(files[rti_files])

  expect_named(extract, c("people", "schools", "workplaces", "catch_all_students"))
  expect_named(extract$people, c("sp_id", "sp_hh_id", "age", "sp_school_id", "sp_work_id"))
  expect_equal(extract$catch_all_students, data.frame(
    row = c(17L, 18L, 27L),
    block_group = c("060750101001", "060750101001", "060750100001"),
    sch = c("3", "3", "3"),
    schg = c("4", "3", "1"),
    latitude = 37.70,
    longitude = -122.50
  ))
})

# A small extract, as returned by rti_read_extract(): a public school, two private schools
# with a location and a catch-all. Households 1 to 4 have students in the catch-all.
catch_all_extract <- function() {
  people <- data.frame(
    sp_id = 1:9,
    sp_hh_id = c(1, 1, 2, 3, 3, 3, 4, 4, 5),
    age = c(10, 3, 12, 6, 8, 16, 7, 40, 9),
    sp_school_id = c(40, 40, 40, 40, 40, 40, 40, NA, 10),
    sp_work_id = NA
  )
  list(
    people = people,
    schools = data.frame(
      sp_id = c(10, 20, 30, 40),
      prek = c(0, 5, 0, 10),
      kinder = c(10, 5, 5, 10),
      gr01_gr12 = c(90, 40, 45, 980),
      latitude = c(37.75, 37.80, 37.60, 0),
      longitude = c(-122.45, -122.40, -122.50, 0),
      source = c("NCES", "schoolinformation.com", "schoolinformation.com", "schoolinformation.com")
    ),
    workplaces = data.frame(sp_id = integer()),
    catch_all_students = data.frame(
      row = 1:7,
      block_group = c("B", "B", "A", "A", "A", "A", "B"),
      sch = "3",
      schg = c("4", "1", "4", "3", "3", "5", "3"),
      latitude = c(37.61, 37.61, 37.79, 37.79, 37.79, 37.79, 37.61),
      longitude = c(-122.49, -122.49, -122.41, -122.41, -122.41, -122.41, -122.49)
    )
  )
}

test_that("rti_synthetic_schools() fills schools in block group order, keeping households together", {
  x <- catch_all_extract()
  fixed <- rti_synthetic_schools(x, size = 3)

  # Order: household 2 (A, 1 student), 3 (A, 3), 1 (B, 2), 4 (B, 1). A school takes households
  # until it has at least 3 students.
  expect_equal(fixed$people$sp_school_id, c(-2, -2, -1, -1, -1, -1, -3, NA, 10))
  expect_equal(fixed$schools$sp_id, c(10, 20, 30, -1, -2, -3))
  expect_equal(fixed$people[-4], x$people[-4])

  pop <- rti_reduce(x, "06075", "2010_ver1", "synthetic", 3)
  expect_equal(pop$schools, data.frame(school_id = 1:6))
})

test_that("rti_synthetic_schools() doesn't depend on the order of students", {
  x <- catch_all_extract()
  shuffled <- x
  shuffled$catch_all_students <- x$catch_all_students[c(7, 3, 1, 5, 2, 6, 4), ]
  expect_equal(rti_synthetic_schools(shuffled, 3), rti_synthetic_schools(x, 3))
})

test_that("rti_synthetic_schools() puts everyone in one school when it is large enough", {
  fixed <- rti_synthetic_schools(catch_all_extract(), size = 141)
  expect_equal(fixed$people$sp_school_id, c(rep(-1, 7), NA, 10))
  expect_equal(fixed$schools$sp_id, c(10, 20, 30, -1))
})

test_that("rti_nearest_schools() uses the nearest school with the grade", {
  fixed <- rti_nearest_schools(catch_all_extract())
  # Households 1 and 4 live by school 30, and households 2 and 3 by school 20. Household 1's
  # preschooler goes to school 20, the only private school with a preschool.
  expect_equal(fixed$people$sp_school_id, c(30, 20, 20, 20, 20, 20, 30, NA, 10))
  expect_equal(fixed$schools$sp_id, c(10, 20, 30))
})

test_that("both methods only remove catch-all schools when nobody is in them", {
  x <- catch_all_extract()
  x$people$sp_school_id <- c(10, 20, NA, 30, NA, NA, NA, NA, 10)
  x$catch_all_students <- x$catch_all_students[0, ]
  for (fixed in list(rti_synthetic_schools(x, 3), rti_nearest_schools(x))) {
    expect_equal(fixed$people, x$people)
    expect_equal(fixed$schools, x$schools[1:3, ])
  }
})

test_that("both methods error on unknown school sources and public catch-alls", {
  x <- catch_all_extract()
  x$schools$source[2] <- "private"
  expect_error(rti_synthetic_schools(x, 3), "Unknown school source.*\"private\"")
  expect_error(rti_nearest_schools(x), "Unknown school source.*\"private\"")

  x <- catch_all_extract()
  x$schools$latitude[1] <- 0
  x$schools$longitude[1] <- 0
  expect_error(rti_synthetic_schools(x, 3), "public school .* no location")
  expect_error(rti_nearest_schools(x), "public school .* no location")
})

test_that("rti_nearest_schools() errors when students can't be moved", {
  x <- catch_all_extract()
  x$catch_all_students$sch[3] <- NA
  expect_error(rti_nearest_schools(x), "private school according to the census \\(PUMS SCH = 3\\)")
  # The synthetic method doesn't need the census
  expect_no_error(rti_synthetic_schools(x, 3))

  x <- catch_all_extract()
  x$catch_all_students$sch[3] <- "2"
  expect_error(rti_nearest_schools(x), "PUMS SCH = 3")

  x <- catch_all_extract()
  x$catch_all_students$schg[3] <- "6"
  expect_error(rti_nearest_schools(x), "PUMS SCHG 1 to 5")

  x <- catch_all_extract()
  x$catch_all_students$latitude[3] <- NA
  expect_error(rti_nearest_schools(x), "1 student\\(s\\) .* no household location")

  x <- catch_all_extract()
  x$schools$prek[2] <- 0
  expect_error(rti_nearest_schools(x), "no private school with a location has enrolment in prek")
})

test_that("nearest_location() uses great-circle distance", {
  # Across the antimeridian, and at a high latitude where degrees of longitude are short
  expect_equal(nearest_location(0, 179, c(0, 0), c(170, -179)), 2)
  expect_equal(nearest_location(80, 0, c(80, 78), c(10, 0)), 1)
  # Ties go to the first, and repeated points agree
  expect_equal(nearest_location(c(0, 1, 0), c(0, 0, 0), c(0, 0), c(1, -1), chunk_size = 1), c(1, 1, 1))
})

test_that("changing the catch-all method or school size uses the cache", {
  local_cache_dir()
  calls <- local_rti_mock()
  rti_population("06075")
  rti_population("06075", catch_all_schools = "nearest")
  rti_population("06075", synthetic_school_size = 2)
  expect_equal(calls$zip, 1)
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
  saveRDS(list(format = 0L, extract = "stale"), path)

  pop <- rti_population("06075")
  expect_s3_class(pop, "helios_synthetic_population")
  expect_equal(calls$zip, 1)
  cached <- readRDS(path)
  expect_equal(cached$format, rti_cache_format)
  expect_named(cached$extract, c("people", "schools", "workplaces", "catch_all_students"))
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
  expect_identical(rti_population("06075"), first)
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

test_that("read_csv_columns() reads tab-separated files and text columns", {
  tsv <- withr::local_tempfile(lines = c(
    "sp_id\tserialno\tschool_id",
    "1\t2007000000001\tX",
    "2\t2007000000002\t450000001"
  ))
  expected <- data.frame(serialno = c("2007000000001", "2007000000002"), school_id = c(NA, 450000001L))
  expect_equal(read_csv_columns(tsv, c("serialno", "school_id"), character = "serialno", use_fread = FALSE), expected)
  skip_if_not_installed("data.table")
  expect_equal(read_csv_columns(tsv, c("serialno", "school_id"), character = "serialno", use_fread = TRUE), expected)
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
