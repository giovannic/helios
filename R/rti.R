#' Synthetic population for a US county or state from RTI International
#'
#' Downloads the RTI International U.S. Synthetic Population for a county or
#' state, reduces it to the people, schools and workplaces helios needs, and
#' caches the result. Later calls for the same `fips` and `version` read the
#' cache and need no network access.
#'
#' The data come from
#' <https://github.com/RTIInternational/SyntheticPopulations> and are licensed
#' under CC BY 4.0. Downloads can be large: a county is typically tens of MB, and
#' a large state over 1 GB.
#'
#' Group quarters (such as care homes, prisons and dormitories) are not
#' included.
#'
#' @param fips A FIPS code as a character string: 2 digits for a state (e.g.
#'   `"06"` for California) or 5 digits for a county (e.g. `"06075"` for San
#'   Francisco). Numbers are not accepted, because they lose leading zeros.
#' @param version The synthetic population release. Only `"2010_ver1"` is
#'   available.
#' @param refresh If `TRUE`, download the data again even if it is cached.
#'
#' @return A `helios_synthetic_population`: a list with
#'   * `people`: a data frame with one row per person and integer columns
#'     `household_id`, `age`, `school_id` and `workplace_id` (`NA` means none).
#'   * `schools`: a data frame with columns `school_id` and `reference_size`,
#'     the school's number of students across the whole US synthetic population.
#'   * `workplaces`: a data frame with columns `workplace_id` and
#'     `reference_size`, the workplace's number of workers across the whole US
#'     synthetic population.
#'   * `metadata`: a list with `source`, `version`, `fips`, `n_people` and
#'     `n_households`.
#'
#'   A county extract lists only the county's residents, so a school or
#'   workplace that also has members from elsewhere has fewer people in
#'   `people` than its `reference_size`.
#'
#' @seealso [rti_cache_dir()] for where the data are cached.
#' @family data
#' @export
#' @examples
#' \dontrun{
#' san_francisco <- rti_population("06075")
#' }
rti_population <- function(fips, version = "2010_ver1", refresh = FALSE) {
  fips <- rti_check_fips(fips)
  rti_check_version(version)
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("`refresh` must be TRUE or FALSE")
  }

  dir <- file.path(rti_cache_dir(), "rti", version)
  path <- file.path(dir, paste0(fips, ".rds"))

  if (!refresh && file.exists(path)) {
    cached <- readRDS(path)
    # Entries written by an older way of reducing the data are downloaded again
    if (identical(cached$format, rti_cache_format)) {
      return(cached$population)
    }
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  population <- rti_download_population(fips, version, dir)

  # Write next to the final path and rename, so the cache never holds a partial file
  tmp <- tempfile("population-", tmpdir = dir, fileext = ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(list(format = rti_cache_format, population = population), tmp)
  if (!file.rename(tmp, path)) {
    stop("Could not write the cached population to ", path)
  }

  population
}

#' Directory where downloaded synthetic populations are cached
#'
#' By default this is the user cache directory given by
#' [tools::R_user_dir()]. Set `options(helios.cache_dir = "path")` to use a
#' different directory. Populations from [rti_population()] are stored under
#' `rti/{version}/{fips}.rds` inside it; delete a file to remove it from the
#' cache.
#'
#' @return The path of the cache directory. It may not exist yet.
#'
#' @family data
#' @export
rti_cache_dir <- function() {
  dir <- getOption("helios.cache_dir")
  if (is.null(dir)) {
    dir <- tools::R_user_dir("helios", which = "cache")
  }
  dir
}

# Bump when the reduced object changes, so existing cache entries are rebuilt
rti_cache_format <- 1L

rti_versions <- list(
  "2010_ver1" = list(year = "2010")
)

# Files extracted from each zip. Households and pums_p are needed to fix
# private-school assignments.
rti_files <- c("synth_people", "synth_households", "schools", "workplaces", "pums_p")

rti_raw_url <- "https://raw.githubusercontent.com/RTIInternational/SyntheticPopulations/main/"
rti_media_url <- "https://media.githubusercontent.com/media/RTIInternational/SyntheticPopulations/main/"

rti_check_fips <- function(fips) {
  if (is.numeric(fips)) {
    stop(
      "`fips` must be a character string, not a number: numbers lose their leading zeros. ",
      "For example, use \"06075\" rather than 6075 for San Francisco."
    )
  }
  if (!is.character(fips) || length(fips) != 1 || is.na(fips) ||
      !grepl("^([0-9]{2}|[0-9]{5})$", fips)) {
    stop(
      "`fips` must be a single 2-digit state or 5-digit county FIPS code, ",
      "such as \"06\" or \"06075\""
    )
  }
  fips
}

rti_check_version <- function(version) {
  if (!is.character(version) || length(version) != 1 || !version %in% names(rti_versions)) {
    stop(
      "`version` must be one of: ",
      paste0("\"", names(rti_versions), "\"", collapse = ", ")
    )
  }
}

# Path of a zip inside the RTI repository
rti_zip_path <- function(fips, version) {
  year <- rti_versions[[version]]$year
  zip <- paste0(version, "_", fips, ".zip")
  if (nchar(fips) == 2) {
    paste(year, "State", zip, sep = "/")
  } else {
    paste(year, "County", substr(fips, 1, 2), zip, sep = "/")
  }
}

rti_download_file <- function(url, destfile, quiet = FALSE) {
  # State zips take longer than the default 60 second timeout
  old <- options(timeout = max(3600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  status <- utils::download.file(url, destfile, mode = "wb", quiet = quiet)
  if (status != 0) {
    stop("Download of ", url, " failed with status ", status)
  }
  invisible(destfile)
}

# The zips are stored with Git LFS. The raw file is a pointer giving their size and sha256.
rti_lfs_pointer <- function(zip_path, fips, dir) {
  url <- paste0(rti_raw_url, zip_path)
  tmp <- tempfile("pointer-", tmpdir = dir, fileext = ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  tryCatch(
    rti_download_file(url, tmp, quiet = TRUE),
    error = function(e) {
      stop(
        "Could not find the synthetic population for FIPS \"", fips, "\" at ", url,
        ". Check the FIPS code and your internet connection. ",
        "Original error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )
  lines <- readLines(tmp, warn = FALSE)
  size <- sub("^size ", "", grep("^size [0-9]+$", lines, value = TRUE))
  sha256 <- sub("^oid sha256:", "", grep("^oid sha256:[0-9a-f]{64}$", lines, value = TRUE))
  if (length(size) != 1 || length(sha256) != 1) {
    stop("Unexpected Git LFS pointer file at ", url)
  }
  list(size = as.numeric(size), sha256 = sha256)
}

rti_check_integrity <- function(file, pointer) {
  size <- file.size(file)
  if (is.na(size) || size != pointer$size) {
    stop(
      "Downloaded file has size ", size, " bytes but should have ", pointer$size,
      ". The download may have been interrupted: try again."
    )
  }
  if (requireNamespace("digest", quietly = TRUE)) {
    sha256 <- digest::digest(file = file, algo = "sha256")
    if (!identical(sha256, pointer$sha256)) {
      stop("Downloaded file has the wrong sha256 checksum. Try again.")
    }
  }
}

rti_download_population <- function(fips, version, dir) {
  zip_path <- rti_zip_path(fips, version)
  pointer <- rti_lfs_pointer(zip_path, fips, dir)

  work <- tempfile("download-", tmpdir = dir)
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)

  zip <- file.path(work, basename(zip_path))
  rti_download_file(paste0(rti_media_url, zip_path), zip)
  rti_check_integrity(zip, pointer)

  files <- stats::setNames(paste0(version, "_", fips, "_", rti_files, ".txt"), rti_files)
  missing <- setdiff(files, utils::unzip(zip, list = TRUE)$Name)
  if (length(missing) > 0) {
    stop("The downloaded zip is missing: ", paste(missing, collapse = ", "))
  }
  utils::unzip(zip, files = files, exdir = work)

  rti_reduce(stats::setNames(file.path(work, files), rti_files), fips, version)
}

rti_reduce <- function(files, fips, version) {
  people <- read_csv_columns(files[["synth_people"]], c("sp_hh_id", "age", "sp_school_id", "sp_work_id"))
  schools <- read_csv_columns(files[["schools"]], c("sp_id", "total"))
  workplaces <- read_csv_columns(files[["workplaces"]], c("sp_id", "workers"))

  household_id <- match(people$sp_hh_id, unique(people$sp_hh_id))

  structure(
    list(
      people = data.frame(
        household_id = household_id,
        age = as.integer(people$age),
        school_id = match(people$sp_school_id, schools$sp_id),
        workplace_id = match(people$sp_work_id, workplaces$sp_id)
      ),
      schools = data.frame(
        school_id = seq_len(nrow(schools)),
        reference_size = as.integer(schools$total)
      ),
      workplaces = data.frame(
        workplace_id = seq_len(nrow(workplaces)),
        reference_size = as.integer(workplaces$workers)
      ),
      metadata = list(
        source = "RTI International U.S. Synthetic Population",
        version = version,
        fips = fips,
        n_people = nrow(people),
        n_households = max(c(0L, household_id))
      )
    ),
    class = "helios_synthetic_population"
  )
}

# Reads only `columns` from a CSV file, treating "" and "X" as missing.
# data.table::fread is much faster on large files, so it's used when installed.
read_csv_columns <- function(path, columns,
                             use_fread = requireNamespace("data.table", quietly = TRUE)) {
  header <- names(utils::read.csv(path, nrows = 0, check.names = FALSE))
  missing <- setdiff(columns, header)
  if (length(missing) > 0) {
    stop(basename(path), " is missing column(s): ", paste(missing, collapse = ", "))
  }

  na_strings <- c("", "X")
  if (use_fread) {
    x <- data.table::fread(
      path,
      select = columns, na.strings = na_strings,
      data.table = FALSE, showProgress = FALSE
    )
  } else {
    x <- utils::read.csv(
      path,
      colClasses = ifelse(header %in% columns, NA, "NULL"),
      na.strings = na_strings, check.names = FALSE
    )
  }
  x[columns]
}
