#' Synthetic population for a US county or state from RTI International
#'
#' Downloads the RTI International U.S. Synthetic Population for a county or
#' state, caches the parts helios needs, and reduces them to the people, schools
#' and workplaces of each person. Later calls for the same `fips` and `version`
#' read the cache and need no network access.
#'
#' The data come from
#' <https://github.com/RTIInternational/SyntheticPopulations> and are licensed
#' under CC BY 4.0. Downloads can be large: a county is typically tens of MB, and
#' a large state over 1 GB.
#'
#' Group quarters (such as care homes, prisons and dormitories) are not
#' included.
#'
#' Some students in the RTI data are assigned to placeholder schools that have no
#' location and totals such as 1,985,724, rather than to a real school. Every one
#' of them attends a private school according to the census. Between about 5% and
#' 20% of students are in these schools, depending on the location. The
#' placeholder schools are always removed, and `catch_all_schools` sets what
#' happens to their students:
#' * `"synthetic"` (the default) puts them in new private schools of
#'   `synthetic_school_size` students. Households are taken in order of census
#'   block group, so neighbours share a school, and each household's students stay
#'   together. A new school takes households until it has at least
#'   `synthetic_school_size` students, so it can be slightly larger, and the last
#'   one is smaller.
#' * `"nearest"` moves each student to the nearest private school, with a
#'   location, that teaches their grade (preschool, kindergarten or grades 1 to
#'   12), even if it is full. This is the fallback RTI describes in the "School
#'   Assignments" section of the
#'   [data's documentation](https://github.com/RTIInternational/SyntheticPopulations/blob/main/README.md#school-assignments).
#'   Private schools can end up several times their real size.
#'
#' The cache doesn't depend on `catch_all_schools` or `synthetic_school_size`, so
#' changing them doesn't download the data again.
#'
#' @param fips A FIPS code as a character string: 2 digits for a state (e.g.
#'   `"06"` for California) or 5 digits for a county (e.g. `"06075"` for San
#'   Francisco). Numbers are not accepted, because they lose leading zeros.
#' @param version The synthetic population release. Only `"2010_ver1"` is
#'   available.
#' @param catch_all_schools What to do with students in placeholder schools with
#'   no location: `"synthetic"` or `"nearest"`. See Details.
#' @param synthetic_school_size The number of students in each new private
#'   school when `catch_all_schools = "synthetic"`. The default, 141, is the mean
#'   size of US private schools in 2009–10: 4,700,119 students in 33,366 schools
#'   (Table 1 of Broughman, Swaim and Hryczaniuk (2011), *Characteristics of
#'   Private Schools in the United States: Results From the 2009–10 Private School
#'   Universe Survey*, NCES 2011-339).
#' @param refresh If `TRUE`, download the data again even if it is cached.
#'
#' @return A `helios_synthetic_population`: a list with
#'   * `people`: a data frame with one row per person and integer columns
#'     `household_id`, `age`, `school_id` and `workplace_id` (`NA` means none).
#'   * `schools`: a data frame with column `school_id`, one row per school. The
#'     new schools made for `catch_all_schools = "synthetic"` come last.
#'   * `workplaces`: a data frame with column `workplace_id`, one row per
#'     workplace.
#'   * `metadata`: a list with `source`, `version`, `fips`,
#'     `catch_all_schools`, `synthetic_school_size` (`NA` unless
#'     `catch_all_schools = "synthetic"`), `n_people` and `n_households`.
#'
#'   A county extract lists only the county's residents, so a school or
#'   workplace that also has members who live elsewhere has fewer people in
#'   `people` than in reality. A state extract has fewer such settings.
#'
#' @seealso [rti_cache_dir()] for where the data are cached, and
#'   [read_synthetic_population()] to read data you already have.
#' @family data
#' @export
#' @examples
#' \dontrun{
#' san_francisco <- rti_population("06075")
#' san_francisco_nearest <- rti_population("06075", catch_all_schools = "nearest")
#' }
rti_population <- function(fips, version = "2010_ver1", catch_all_schools = "synthetic",
                           synthetic_school_size = 141, refresh = FALSE) {
  fips <- rti_check_fips(fips)
  rti_check_version(version)
  if (!is.character(catch_all_schools) || length(catch_all_schools) != 1 ||
      !catch_all_schools %in% c("synthetic", "nearest")) {
    stop("`catch_all_schools` must be \"synthetic\" or \"nearest\"")
  }
  if (!is.numeric(synthetic_school_size) || length(synthetic_school_size) != 1 ||
      is.na(synthetic_school_size) || synthetic_school_size < 1 ||
      synthetic_school_size != round(synthetic_school_size)) {
    stop("`synthetic_school_size` must be a whole number, at least 1")
  }
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("`refresh` must be TRUE or FALSE")
  }

  extract <- rti_cached_extract(fips, version, refresh)
  rti_reduce(extract, fips, version, catch_all_schools, synthetic_school_size)
}

# The extract for `fips`, from the cache if present, otherwise downloaded and cached
rti_cached_extract <- function(fips, version, refresh) {
  dir <- file.path(rti_cache_dir(), "rti", version)
  path <- file.path(dir, paste0(fips, ".rds"))

  if (!refresh && file.exists(path)) {
    cached <- readRDS(path)
    # Entries written in an older format are downloaded again
    if (identical(cached$format, rti_cache_format)) {
      return(cached$extract)
    }
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  extract <- rti_download_extract(fips, version, dir)

  # Write next to the final path and rename, so the cache never holds a partial file
  tmp <- tempfile("extract-", tmpdir = dir, fileext = ".tmp")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(list(format = rti_cache_format, extract = extract), tmp)
  if (!file.rename(tmp, path)) {
    stop("Could not write the cached population to ", path)
  }

  extract
}

#' Directory where downloaded synthetic populations are cached
#'
#' By default this is the user cache directory given by
#' [tools::R_user_dir()]. Set `options(helios.cache_dir = "path")` to use a
#' different directory. Extracts downloaded by [rti_population()] are stored under
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

# Bump when the cached extract changes, so existing cache entries are rebuilt
rti_cache_format <- 1L

rti_versions <- list(
  "2010_ver1" = list(year = "2010")
)

# Files extracted from each zip. Households and pums_p are needed to move students
# out of catch-all schools with catch_all_schools = "nearest".
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

rti_download_extract <- function(fips, version, dir) {
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

  rti_read_extract(stats::setNames(file.path(work, files), rti_files))
}

# Reads the columns helios needs from an extract's files. Everything needed by either
# way of handling catch-all schools is kept, so the result can be cached once.
rti_read_extract <- function(files) {
  people <- read_csv_columns(
    files[["synth_people"]],
    c("sp_id", "sp_hh_id", "stcotrbg", "serialno", "sporder", "age", "sp_school_id", "sp_work_id"),
    character = c("stcotrbg", "serialno")
  )
  schools <- read_csv_columns(
    files[["schools"]],
    c("sp_id", "prek", "kinder", "gr01_gr12", "latitude", "longitude", "source")
  )
  workplaces <- read_csv_columns(files[["workplaces"]], "sp_id")

  # Students in catch-all schools, with what's needed to move them
  catch_all <- schools$sp_id[rti_is_catch_all(schools)]
  row <- which(people$sp_school_id %in% catch_all)
  students <- data.frame(
    row = row,
    block_group = people$stcotrbg[row],
    sch = rep(NA_character_, length(row)),
    schg = rep(NA_character_, length(row)),
    latitude = rep(NA_real_, length(row)),
    longitude = rep(NA_real_, length(row))
  )
  if (length(row) > 0) {
    pums <- read_csv_columns(
      files[["pums_p"]], c("serialno", "sporder", "sch", "schg"),
      character = c("serialno", "sch", "schg")
    )
    census <- match(
      paste(people$serialno[row], people$sporder[row]),
      paste(pums$serialno, pums$sporder)
    )
    students$sch <- pums$sch[census]
    students$schg <- pums$schg[census]

    households <- read_csv_columns(files[["synth_households"]], c("sp_id", "latitude", "longitude"))
    home <- match(people$sp_hh_id[row], households$sp_id)
    students$latitude <- households$latitude[home]
    students$longitude <- households$longitude[home]
  }

  list(
    people = people[c("sp_id", "sp_hh_id", "age", "sp_school_id", "sp_work_id")],
    schools = schools,
    workplaces = workplaces,
    catch_all_students = students
  )
}

rti_reduce <- function(extract, fips, version, catch_all_schools, synthetic_school_size) {
  fixed <- switch(catch_all_schools,
    synthetic = rti_synthetic_schools(extract, synthetic_school_size),
    nearest = rti_nearest_schools(extract)
  )

  build_synthetic_population(
    people = fixed$people,
    schools = fixed$schools["sp_id"],
    workplaces = extract$workplaces,
    metadata = list(
      source = "RTI International U.S. Synthetic Population",
      version = version,
      fips = fips,
      catch_all_schools = catch_all_schools,
      synthetic_school_size = if (catch_all_schools == "synthetic") synthetic_school_size else NA_real_
    )
  )
}

# Some rows of the schools file are not real schools but catch-alls, at latitude and
# longitude 0 with totals such as 1,985,724. Their students are private-school students
# that RTI's assignment didn't place in a private school with a location.
rti_is_catch_all <- function(schools) {
  schools$latitude == 0 & schools$longitude == 0
}

# Values of `source` in the schools file. They match the census answers of the students
# at each school: every public-school student (PUMS SCH = 2) is at an "NCES" school, and
# every private-school student (SCH = 3) at a "schoolinformation.com" school.
rti_school_sources <- c(public = "NCES", private = "schoolinformation.com")

# Whether each school is private. Errors on unknown sources and on public catch-alls.
rti_is_private <- function(schools) {
  unknown <- setdiff(unique(schools$source), rti_school_sources)
  if (length(unknown) > 0) {
    stop(
      "Unknown school source(s) in the schools file: ",
      paste0("\"", unknown, "\"", collapse = ", ")
    )
  }
  private <- schools$source == rti_school_sources[["private"]]
  if (any(rti_is_catch_all(schools) & !private)) {
    stop("A public school in the schools file has no location")
  }
  private
}

# Puts students in catch-all schools into new private schools of `size` students.
#
# helios has no geography, so a school only needs its members. Households are taken in
# order of census block group, then household id, so neighbours share a school. A new
# school is started once the current one has at least `size` students, which keeps each
# household's students together. New schools get negative ids, which can't clash with
# RTI's.
rti_synthetic_schools <- function(extract, size) {
  people <- extract$people
  schools <- extract$schools
  rti_is_private(schools)
  located <- schools[!rti_is_catch_all(schools), , drop = FALSE]
  students <- extract$catch_all_students
  if (nrow(students) == 0) {
    return(list(people = people, schools = located))
  }

  household <- people$sp_hh_id[students$row]
  first <- !duplicated(household)
  order <- order(students$block_group[first], household[first])
  households <- household[first][order]
  n_students <- tabulate(match(household, households), length(households))
  school <- floor((cumsum(n_students) - n_students) / size) + 1

  people$sp_school_id[students$row] <- -school[match(household, households)]
  new_schools <- located[rep(NA_integer_, max(school)), , drop = FALSE]
  new_schools$sp_id <- -seq_len(max(school))
  rownames(new_schools) <- NULL
  list(people = people, schools = rbind(located, new_schools))
}

# The school column counting enrolment for each PUMS SCHG grade code
rti_grade_columns <- c(
  "1" = "prek",      # nursery school or preschool
  "2" = "kinder",    # kindergarten
  "3" = "gr01_gr12", # grades 1 to 4
  "4" = "gr01_gr12", # grades 5 to 8
  "5" = "gr01_gr12"  # grades 9 to 12
)

# Moves students in catch-all schools with the fallback in RTI's 2010 Quick Start Guide,
# "School Assignments"
# (https://github.com/RTIInternational/SyntheticPopulations/blob/main/README.md#school-assignments):
# "If there are no private schools within 50 kilometers that have capacity, then the
# student is assigned to the closest private school servicing the students' grade category
# (even if already full)."
#
# Each student goes to the private school nearest their home, among those with a location
# and enrolment in their grade category. The grade comes from the census (PUMS SCHG), as in
# RTI's assignment.
rti_nearest_schools <- function(extract) {
  people <- extract$people
  schools <- extract$schools
  private <- rti_is_private(schools)
  located <- !rti_is_catch_all(schools)
  students <- extract$catch_all_students
  if (nrow(students) == 0) {
    return(list(people = people, schools = schools[located, , drop = FALSE]))
  }

  if (any(is.na(students$sch) | students$sch != "3")) {
    stop("Students in catch-all schools must all attend private school according to the census (PUMS SCH = 3)")
  }
  grade <- unname(rti_grade_columns[students$schg])
  if (anyNA(grade)) {
    stop("Students in catch-all schools must all be in preschool to grade 12 (PUMS SCHG 1 to 5)")
  }
  if (anyNA(students$latitude) || anyNA(students$longitude)) {
    stop(
      sum(is.na(students$latitude) | is.na(students$longitude)),
      " student(s) in catch-all schools have no household location"
    )
  }

  school <- integer(nrow(students))
  for (column in unique(grade)) {
    candidates <- which(located & private & schools[[column]] > 0)
    if (length(candidates) == 0) {
      stop(
        "Students in catch-all schools can't be moved: no private school with a ",
        "location has enrolment in ", column
      )
    }
    in_grade <- grade == column
    nearest <- nearest_location(
      students$latitude[in_grade], students$longitude[in_grade],
      schools$latitude[candidates], schools$longitude[candidates]
    )
    school[in_grade] <- candidates[nearest]
  }

  people$sp_school_id[students$row] <- schools$sp_id[school]
  list(people = people, schools = schools[located, , drop = FALSE])
}

# For each point (lat, lon), the index of the nearest of (to_lat, to_lon) by great-circle
# distance. The first is chosen on ties.
nearest_location <- function(lat, lon, to_lat, to_lon, chunk_size = 1000) {
  # Points that share a location, such as members of one household, are computed once
  key <- paste(lat, lon)
  unique_points <- !duplicated(key)
  u_lat <- lat[unique_points] * pi / 180
  u_lon <- lon[unique_points] * pi / 180
  to_lat <- to_lat * pi / 180
  to_lon <- to_lon * pi / 180

  nearest <- integer(length(u_lat))
  for (start in seq(1, length(u_lat), by = chunk_size)) {
    i <- start:min(start + chunk_size - 1, length(u_lat))
    # The haversine term increases with distance, so the arcsine isn't needed
    h <- sin(outer(u_lat[i], to_lat, "-") / 2)^2 +
      outer(cos(u_lat[i]), cos(to_lat)) * sin(outer(u_lon[i], to_lon, "-") / 2)^2
    nearest[i] <- max.col(-h, ties.method = "first")
  }
  nearest[match(key, key[unique_points])]
}

# Reads only `columns` from a comma- or tab-separated file, treating "" and "X" as
# missing. Columns in `character` are read as text, e.g. ids too long for an integer.
# data.table::fread is much faster on large files, so it's used when installed.
read_csv_columns <- function(path, columns, character = NULL,
                             use_fread = requireNamespace("data.table", quietly = TRUE)) {
  header <- read_header(path)
  missing <- setdiff(columns, header)
  if (length(missing) > 0) {
    stop(basename(path), " is missing column(s): ", paste(missing, collapse = ", "))
  }

  na_strings <- c("", "X")
  sep <- if (grepl("\t", readLines(path, n = 1, warn = FALSE))) "\t" else ","
  if (use_fread) {
    x <- data.table::fread(
      path,
      sep = sep, select = columns, na.strings = na_strings,
      colClasses = if (length(character) > 0) list(character = character),
      data.table = FALSE, showProgress = FALSE
    )
  } else {
    col_classes <- ifelse(header %in% columns, NA, "NULL")
    col_classes[header %in% character] <- "character"
    x <- utils::read.csv(
      path,
      sep = sep, colClasses = col_classes,
      na.strings = na_strings, check.names = FALSE
    )
  }
  normalise_missing(x[columns], character)
}

# Sets "" and "X" to NA in text columns, and converts text columns not in `character` to
# numbers where they allow it. fread keeps quoted "X" as text, and user data frames may
# have ids stored as text.
normalise_missing <- function(x, character = NULL) {
  for (column in names(x)) {
    if (is.factor(x[[column]])) x[[column]] <- as.character(x[[column]])
    if (is.character(x[[column]])) {
      x[[column]][x[[column]] %in% c("", "X")] <- NA
      if (!column %in% character) x[[column]] <- utils::type.convert(x[[column]], as.is = TRUE)
    }
  }
  x
}

read_header <- function(path) {
  line <- readLines(path, n = 1, warn = FALSE)
  if (length(line) == 0) {
    stop(basename(path), " is empty")
  }
  sep <- if (grepl("\t", line)) "\t" else ","
  names(utils::read.csv(text = line, sep = sep, check.names = FALSE))
}
