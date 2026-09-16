#' Read a synthetic population from your own files or data frames
#'
#' Reduces a synthetic population to what helios needs: each person's household,
#' age, school and workplace. Use this for data that [rti_population()] can't
#' download, such as a local copy of an RTI or FRED extract, or a population you
#' built yourself.
#'
#' Columns are matched by name, and other columns are ignored. Both the 2010 RTI
#' spellings and the FRED export spellings are accepted:
#'
#' | Table | Column | Accepted names |
#' |---|---|---|
#' | `people` | household (required) | `sp_hh_id`, `household_id` |
#' | `people` | age in years (required) | `age` |
#' | `people` | school | `sp_school_id`, `school_id` |
#' | `people` | workplace | `sp_work_id`, `work_id`, `workplace_id` |
#' | `people` | person | `sp_id` |
#' | `schools` | school (required) | `sp_id`, `school_id` |
#' | `workplaces` | workplace (required) | `sp_id`, `workplace_id` |
#'
#' Empty strings, `"X"` and `NA` mean none. Files can be comma or tab
#' separated.
#'
#' Household, school and workplace ids are renumbered to `1, 2, ...`. When
#' `schools` or `workplaces` is given, ids are numbered by row of that table, and
#' every id in `people` must appear in it. Otherwise they are numbered in order of
#' first appearance.
#'
#' @param people Path to a file, or a data frame, with one row per person.
#' @param schools Optional path to a file, or a data frame, with one row per
#'   school.
#' @param workplaces Optional path to a file, or a data frame, with one row per
#'   workplace.
#'
#' @return A `helios_synthetic_population`, as described in [rti_population()].
#'   `metadata$source` is `"user-supplied"`, and `metadata$version`,
#'   `metadata$fips`, `metadata$catch_all_schools` and
#'   `metadata$synthetic_school_size` are `NA`. `people` only has a `school_id` or `workplace_id`
#'   column if the input has one. `schools` or `workplaces` is `NULL` when not
#'   given.
#'
#' @seealso [validate_synthetic_population()] for the checks made.
#' @family data
#' @export
#' @examples
#' people <- data.frame(
#'   sp_hh_id = c(10, 10, 11),
#'   age = c(40, 8, 67),
#'   sp_school_id = c(NA, 5, NA),
#'   sp_work_id = c(7, NA, NA)
#' )
#' schools <- data.frame(sp_id = 5)
#' workplaces <- data.frame(sp_id = 7)
#' read_synthetic_population(people, schools, workplaces)
read_synthetic_population <- function(people, schools = NULL, workplaces = NULL) {
  build_synthetic_population(
    people, schools, workplaces,
    metadata = list(
      source = "user-supplied", version = NA_character_, fips = NA_character_,
      catch_all_schools = NA_character_, synthetic_school_size = NA_real_
    )
  )
}

#' Check a synthetic population
#'
#' Checks that a `helios_synthetic_population` is well formed.
#' [rti_population()] and [read_synthetic_population()] both call this, so you
#' only need it for an object you built or changed yourself.
#'
#' The checks are:
#' * `people` has at least one row, and integer `household_id` and `age`
#'   columns with no `NA`.
#' * Ages are between 0 and 120.
#' * Household ids are `1, 2, ...` with no gaps, so every household has at least
#'   one member.
#' * `school_id` and `workplace_id`, when present, are integers that are `NA` or
#'   a row of `schools`/`workplaces`, when those are given.
#' * `schools` and `workplaces`, when given, have ids `1, 2, ...`.
#' * `metadata$n_people` and `metadata$n_households` match `people`.
#'
#' @param x A `helios_synthetic_population`.
#'
#' @return `x`, invisibly. An error is raised if any check fails.
#'
#' @family data
#' @export
validate_synthetic_population <- function(x) {
  if (!inherits(x, "helios_synthetic_population")) {
    stop("`x` must be a helios_synthetic_population")
  }
  missing <- setdiff(c("people", "schools", "workplaces", "metadata"), names(x))
  if (length(missing) > 0) {
    stop("`x` is missing: ", paste(missing, collapse = ", "))
  }

  people <- x$people
  if (!is.data.frame(people)) {
    stop("`people` must be a data frame")
  }
  if (nrow(people) == 0) {
    stop("`people` has no rows")
  }
  for (column in c("household_id", "age")) {
    if (!column %in% names(people)) {
      stop("`people` is missing column ", column)
    }
    if (!is.integer(people[[column]])) {
      stop("`people$", column, "` must be an integer")
    }
  }
  if (anyNA(people$household_id)) {
    stop("`people$household_id` has missing values")
  }
  check_ages(people$age)
  if (min(people$household_id) < 1 || any(tabulate(people$household_id) == 0)) {
    stop("`people$household_id` must number households 1, 2, ... with no gaps")
  }

  validate_settings(people, x$schools, "school_id", "schools")
  validate_settings(people, x$workplaces, "workplace_id", "workplaces")

  metadata <- x$metadata
  if (!identical(as.integer(metadata$n_people), nrow(people))) {
    stop("`metadata$n_people` must be the number of rows of `people`")
  }
  if (!identical(as.integer(metadata$n_households), max(people$household_id))) {
    stop("`metadata$n_households` must be the number of households in `people`")
  }

  invisible(x)
}

validate_settings <- function(people, settings, id, table) {
  ids <- people[[id]]
  if (!is.null(ids)) {
    if (!is.integer(ids)) {
      stop("`people$", id, "` must be an integer")
    }
    if (any(ids < 1, na.rm = TRUE)) {
      stop("`people$", id, "` must be NA or at least 1")
    }
  }
  if (is.null(settings)) {
    return(invisible())
  }

  if (is.null(ids)) {
    stop("`", table, "` is given but `people` has no ", id, " column")
  }
  if (!is.data.frame(settings) || !id %in% names(settings)) {
    stop("`", table, "` must be a data frame with column ", id)
  }
  if (!identical(settings[[id]], seq_len(nrow(settings)))) {
    stop("`", table, "$", id, "` must be 1, 2, ... in row order")
  }
  if (any(ids > nrow(settings), na.rm = TRUE)) {
    stop("`people$", id, "` has ids that are not in `", table, "`")
  }
}

check_ages <- function(age, table = "people") {
  if (!is.numeric(age)) {
    stop("`", table, "$age` must be numeric")
  }
  if (anyNA(age)) {
    stop("`", table, "$age` has missing values")
  }
  if (any(age != round(age))) {
    stop("`", table, "$age` must be whole numbers of years")
  }
  if (any(age < 0 | age > 120)) {
    stop("`", table, "$age` must be between 0 and 120")
  }
}

# Column names accepted for each table, in order of preference
synthetic_population_columns <- list(
  people = list(
    id = "sp_id",
    household_id = c("sp_hh_id", "household_id"),
    age = "age",
    school_id = c("sp_school_id", "school_id"),
    workplace_id = c("sp_work_id", "work_id", "workplace_id")
  ),
  schools = list(id = c("sp_id", "school_id")),
  workplaces = list(id = c("sp_id", "workplace_id"))
)

# Reads, checks and reduces the tables of a synthetic population. `people`,
# `schools` and `workplaces` are paths or data frames using any of the accepted
# column names.
build_synthetic_population <- function(people, schools, workplaces, metadata) {
  columns <- synthetic_population_columns
  people <- read_population_table(people, "people", columns$people, required = c("household_id", "age"))
  if (nrow(people) == 0) {
    stop("`people` has no rows")
  }
  if (anyNA(people$household_id)) {
    stop("`people` has missing household ids")
  }
  check_ages(people$age)
  if ("id" %in% names(people) && anyDuplicated(people$id[!is.na(people$id)])) {
    stop("`people` has duplicated person ids (sp_id)")
  }

  schools <- reduce_settings(people, schools, "school_id", "schools", columns$schools)
  workplaces <- reduce_settings(people, workplaces, "workplace_id", "workplaces", columns$workplaces)

  household_id <- match(people$household_id, unique(people$household_id))
  reduced <- data.frame(household_id = household_id, age = as.integer(people$age))
  if (!is.null(schools$ids)) reduced$school_id <- schools$ids
  if (!is.null(workplaces$ids)) reduced$workplace_id <- workplaces$ids

  population <- structure(
    list(
      people = reduced,
      schools = schools$table,
      workplaces = workplaces$table,
      metadata = c(metadata, list(n_people = nrow(reduced), n_households = max(household_id)))
    ),
    class = "helios_synthetic_population"
  )
  validate_synthetic_population(population)
}

# Renumbers people's ids for one type of setting, and reduces its table.
# Returns the new ids (NULL if people has no such column) and the table (NULL if not given).
reduce_settings <- function(people, settings, id, table, columns) {
  ids <- people[[id]]
  if (is.null(settings)) {
    if (!is.null(ids)) ids <- match(ids, unique(ids[!is.na(ids)]))
    return(list(ids = ids, table = NULL))
  }

  settings <- read_population_table(settings, table, columns, required = "id")
  if (is.null(ids)) {
    stop("`", table, "` is given but `people` has no ", id, " column")
  }
  if (anyNA(settings$id)) {
    stop("`", table, "` has missing ids")
  }
  if (anyDuplicated(settings$id)) {
    stop("`", table, "` has duplicated ids")
  }

  new_ids <- match(ids, settings$id)
  unknown <- !is.na(ids) & is.na(new_ids)
  if (any(unknown)) {
    stop(
      "`people` has ", sum(unknown), " ", id, " value(s) that are not in `", table, "`, ",
      "such as ", ids[unknown][1]
    )
  }

  out <- data.frame(seq_len(nrow(settings)))
  names(out) <- id
  list(ids = new_ids, table = out)
}

# Reads the columns in `columns` (a list of accepted names for each field) from a
# path or data frame, and returns them named by field. "" and "X" are NA.
read_population_table <- function(x, table, columns, required) {
  if (is.character(x) && length(x) == 1) {
    if (!file.exists(x)) {
      stop("`", table, "` file does not exist: ", x)
    }
    header <- read_header(x)
  } else if (is.data.frame(x)) {
    header <- names(x)
  } else {
    stop("`", table, "` must be a path to a file or a data frame")
  }

  found <- vapply(columns, function(names) intersect(names, header)[1], character(1))
  for (field in required) {
    if (is.na(found[[field]])) {
      stop(
        "`", table, "` is missing a ", field, " column (named ",
        paste(columns[[field]], collapse = " or "), ")"
      )
    }
  }
  found <- found[!is.na(found)]

  if (is.data.frame(x)) {
    x <- normalise_missing(as.data.frame(x)[found])
  } else {
    x <- read_csv_columns(x, unname(found))
  }
  x <- as.data.frame(x)
  names(x) <- names(found)
  x
}
