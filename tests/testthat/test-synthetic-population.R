example_people <- function() {
  data.frame(
    sp_id = c(101, 102, 103, 104, 105),
    sp_hh_id = c(900, 900, 700, 800, 800),
    age = c(40, 8, 67, 17, 45),
    sp_school_id = c("", "450000002", "X", "450000001", NA),
    sp_work_id = c(510000003, NA, NA, 510000001, 510000001)
  )
}
example_schools <- function() data.frame(sp_id = c(450000001, 450000002), total = c(300, 20))
example_workplaces <- function() {
  data.frame(sp_id = c(510000001, 510000002, 510000003), workers = c(12, 3, 250))
}

read_example <- function(people = example_people(), schools = example_schools(),
                         workplaces = example_workplaces()) {
  read_synthetic_population(people, schools, workplaces)
}

test_that("read_synthetic_population() reduces data frames", {
  pop <- read_example()
  expect_s3_class(pop, "helios_synthetic_population")
  expect_named(pop, c("people", "schools", "workplaces", "metadata"))
  expect_equal(pop$people, data.frame(
    household_id = c(1L, 1L, 2L, 3L, 3L),
    age = c(40L, 8L, 67L, 17L, 45L),
    school_id = c(NA, 2L, NA, 1L, NA),
    workplace_id = c(3L, NA, NA, 1L, 1L)
  ))
  expect_equal(pop$schools, data.frame(school_id = 1:2))
  expect_equal(pop$workplaces, data.frame(workplace_id = 1:3))
  expect_equal(pop$metadata, list(
    source = "user-supplied", version = NA_character_, fips = NA_character_,
    catch_all_schools = NA_character_, synthetic_school_size = NA_real_,
    n_people = 5L, n_households = 3L
  ))
})

test_that("read_synthetic_population() reads files the same as data frames", {
  dir <- withr::local_tempdir()
  paths <- file.path(dir, c("people.txt", "schools.txt", "workplaces.txt"))
  utils::write.csv(example_people(), paths[1], row.names = FALSE, na = "")
  utils::write.csv(example_schools(), paths[2], row.names = FALSE)
  utils::write.csv(example_workplaces(), paths[3], row.names = FALSE)
  expect_equal(read_synthetic_population(paths[1], paths[2], paths[3]), read_example())
})

test_that("FRED and 2010 RTI column spellings read the same", {
  fred <- example_people()
  names(fred) <- c("sp_id", "sp_hh_id", "age", "school_id", "work_id")
  fred$school_id[is.na(fred$school_id) | fred$school_id == ""] <- "X"
  fred$work_id <- ifelse(is.na(fred$work_id), "X", fred$work_id)
  path <- withr::local_tempfile(fileext = ".txt")
  utils::write.table(fred, path, sep = "\t", quote = FALSE, row.names = FALSE)

  expect_equal(read_synthetic_population(path), read_synthetic_population(example_people()))
  expect_equal(read_example(people = path), read_example())

  helios_names <- data.frame(
    household_id = fred$sp_hh_id, age = fred$age,
    school_id = fred$school_id, workplace_id = fred$work_id
  )
  schools <- data.frame(school_id = example_schools()$sp_id)
  workplaces <- data.frame(workplace_id = example_workplaces()$sp_id)
  expect_equal(read_example(helios_names, schools, workplaces), read_example())
})

test_that("without setting tables, ids are numbered by first appearance", {
  pop <- read_synthetic_population(example_people())
  expect_equal(pop$people$school_id, c(NA, 1L, NA, 2L, NA))
  expect_equal(pop$people$workplace_id, c(1L, NA, NA, 2L, 2L))
  expect_null(pop$schools)
  expect_null(pop$workplaces)
})

test_that("school and workplace columns are optional", {
  pop <- read_synthetic_population(example_people()[c("sp_hh_id", "age")])
  expect_named(pop$people, c("household_id", "age"))
  expect_error(
    read_example(people = example_people()[c("sp_hh_id", "age", "sp_work_id")]),
    "`schools` is given but `people` has no school_id column"
  )
})

test_that("every household has at least one member", {
  pop <- read_example()
  expect_true(all(tabulate(pop$people$household_id, pop$metadata$n_households) > 0))
})

test_that("missing household or age columns are errors", {
  expect_error(read_example(people = example_people()[-2]), "missing a household_id column \\(named sp_hh_id or household_id\\)")
  expect_error(read_example(people = example_people()[-3]), "missing a age column")
})

test_that("missing household ids are errors", {
  people <- example_people()
  people$sp_hh_id[2] <- NA
  expect_error(read_example(people = people), "missing household ids")
})

test_that("ages must be present, whole and between 0 and 120", {
  people <- example_people()
  people$age[1] <- NA
  expect_error(read_example(people = people), "`people\\$age` has missing values")
  people$age[1] <- 40.5
  expect_error(read_example(people = people), "whole numbers")
  people$age[1] <- -1
  expect_error(read_example(people = people), "between 0 and 120")
  people$age[1] <- 121
  expect_error(read_example(people = people), "between 0 and 120")
  people$age <- as.character(people$age)
  people$age[1] <- "forty"
  expect_error(read_example(people = people), "must be numeric")
  people$age[1] <- "120"
  expect_equal(read_example(people = people)$people$age[1], 120L)
})

test_that("duplicated person ids are errors", {
  people <- example_people()
  people$sp_id[2] <- people$sp_id[1]
  expect_error(read_example(people = people), "duplicated person ids")
})

test_that("a population with no people is an error", {
  expect_error(read_example(people = example_people()[0, ]), "`people` has no rows")
})

test_that("school or work ids missing from their tables are errors", {
  expect_error(
    read_example(schools = example_schools()[1, ]),
    "1 school_id value\\(s\\) that are not in `schools`, such as 450000002"
  )
  expect_error(
    read_example(workplaces = example_workplaces()[2:3, ]),
    "2 workplace_id value\\(s\\) that are not in `workplaces`, such as 510000001"
  )
})

test_that("setting tables must have unique, present ids", {
  schools <- example_schools()
  expect_error(read_example(schools = schools[c(1, 2, 1), ]), "`schools` has duplicated ids")
  schools$sp_id[1] <- NA
  expect_error(read_example(schools = schools), "`schools` has missing ids")
  expect_error(read_example(workplaces = example_workplaces()[2]), "missing a id column \\(named sp_id or workplace_id\\)")
})

test_that("inputs must be paths or data frames", {
  expect_error(read_synthetic_population(list(age = 1)), "`people` must be a path to a file or a data frame")
  expect_error(read_synthetic_population("does-not-exist.txt"), "`people` file does not exist")
})

test_that("validate_synthetic_population() accepts valid objects and returns them", {
  pop <- read_example()
  expect_identical(validate_synthetic_population(pop), pop)
  expect_invisible(validate_synthetic_population(pop))
  no_settings <- read_synthetic_population(example_people()[c("sp_hh_id", "age")])
  expect_identical(validate_synthetic_population(no_settings), no_settings)
})

test_that("validate_synthetic_population() rejects malformed objects", {
  pop <- read_example()
  expect_error(validate_synthetic_population(unclass(pop)), "must be a helios_synthetic_population")
  expect_error(validate_synthetic_population(structure(pop[-4], class = class(pop))), "missing: metadata")

  x <- pop
  x$people <- x$people[0, ]
  expect_error(validate_synthetic_population(x), "`people` has no rows")

  x <- pop
  x$people$age <- as.numeric(x$people$age)
  expect_error(validate_synthetic_population(x), "`people\\$age` must be an integer")

  x <- pop
  x$people$age[1] <- 200L
  expect_error(validate_synthetic_population(x), "between 0 and 120")

  x <- pop
  x$people$household_id[x$people$household_id == 2L] <- 4L
  expect_error(validate_synthetic_population(x), "no gaps")

  x <- pop
  x$people$household_id[1] <- NA
  expect_error(validate_synthetic_population(x), "household_id` has missing values")

  x <- pop
  x$people$school_id[1] <- 3L
  expect_error(validate_synthetic_population(x), "school_id` has ids that are not in `schools`")

  x <- pop
  x$people$workplace_id[1] <- 0L
  expect_error(validate_synthetic_population(x), "must be NA or at least 1")

  x <- pop
  x$schools$school_id <- 2:1
  expect_error(validate_synthetic_population(x), "must be 1, 2, ... in row order")

  x <- pop
  x$people$school_id <- NULL
  expect_error(validate_synthetic_population(x), "`schools` is given but `people` has no school_id column")

  x <- pop
  x$metadata$n_people <- 4L
  expect_error(validate_synthetic_population(x), "n_people")

  x <- pop
  x$metadata$n_households <- 4L
  expect_error(validate_synthetic_population(x), "n_households")
})
