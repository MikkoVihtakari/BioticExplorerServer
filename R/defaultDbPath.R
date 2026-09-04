## Internal: kept as a function so that the Windows branch of defaultDbPath() can be
## exercised on other platforms in the tests.

.isWindows <- function() {
  identical(.Platform$OS.type, "windows")
}

#' @title Default location of the IMR Biotic database
#' @description Returns the platform-appropriate default folder for the
#'   \link[duckdb]{duckdb} database compiled by \code{\link{compileDatabase}}.
#' @param folder Character string giving the name of the database folder inside the
#'   user's home directory. Defaults to \code{"IMR_biotic_BES_database"}.
#' @details On macOS and Linux the default is \code{~/IMR_biotic_BES_database}. On
#'   Windows, R expands \code{~} through the "personal"/Documents special folder, which
#'   OneDrive's Known Folder Move frequently redirects into a synchronized
#'   \code{OneDrive - <Organization>\\Documents} folder. A multi-gigabyte DuckDB file does
#'   not belong in a cloud-synchronized folder, and the redirection made the documented
#'   default resolve to different real locations depending on the machine. The default
#'   therefore resolves to \code{\%USERPROFILE\%\\IMR_biotic_BES_database} on Windows,
#'   matching the convention documented by
#'   \href{https://github.com/DeepWaterIMR/BAIT}{BAIT}. If \code{USERPROFILE} is not set,
#'   the function falls back to \code{~} expansion.
#' @return Character string giving an absolute path to the default database folder. The
#'   folder is not created.
#' @seealso \code{\link{dbPathCandidates}} and \code{\link{findDatabase}} for locating a
#'   database that was installed to either of the two historical Windows locations.
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @examples
#' defaultDbPath()
#' @export

defaultDbPath <- function(folder = "IMR_biotic_BES_database") {
  if (.isWindows()) {
    profile <- Sys.getenv("USERPROFILE")

    if (nzchar(profile)) {
      return(file.path(
        normalizePath(profile, winslash = "/", mustWork = FALSE), folder
      ))
    }
  }

  path.expand(file.path("~", folder))
}

#' @title Plausible locations of the IMR Biotic database
#' @description Returns the folders in which an IMR Biotic database may have been
#'   installed, most likely first.
#' @inheritParams defaultDbPath
#' @details On macOS and Linux there is only one candidate, \code{~/IMR_biotic_BES_database}.
#'   On Windows the function returns \code{\link{defaultDbPath}} first, followed by the
#'   location that raw \code{~} expansion produces (typically the Documents folder, which
#'   may be redirected into OneDrive). Databases compiled before
#'   BioticExplorerServer 0.8.7 may live in the latter, so both are worth checking.
#' @return Character vector of unique absolute paths. The folders are not created and are
#'   not guaranteed to exist.
#' @seealso \code{\link{defaultDbPath}}, \code{\link{findDatabase}}
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @examples
#' dbPathCandidates()
#' @export

dbPathCandidates <- function(folder = "IMR_biotic_BES_database") {
  unique(c(
    defaultDbPath(folder = folder),
    path.expand(file.path("~", folder))
  ))
}

#' @title Locate an existing IMR Biotic database
#' @description Searches the plausible database locations and returns the path to the
#'   first \link[duckdb]{duckdb} file that exists.
#' @param dbName Character string or \code{NULL} giving the name of the database file
#'   without the \code{.duckdb} extension. If \code{NULL} uses the default name
#'   ("bioticexplorer").
#' @inheritParams defaultDbPath
#' @details Checks \code{\link{dbPathCandidates}} in order, so a database installed to the
#'   current default takes precedence over one left in the location that raw \code{~}
#'   expansion produced on Windows. Intended for downstream code, such as
#'   \href{https://github.com/DeepWaterIMR/BioticExplorer}{BioticExplorer}, which has to
#'   find a database it did not compile itself.
#' @return Character string giving the absolute path to an existing \code{.duckdb} file,
#'   or \code{NULL} if no database was found.
#' @seealso \code{\link{defaultDbPath}}, \code{\link{dbPathCandidates}}
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @examples
#' findDatabase()
#' @export

findDatabase <- function(dbName = NULL, folder = "IMR_biotic_BES_database") {
  if (is.null(dbName)) dbName <- "bioticexplorer"

  candidates <- file.path(
    dbPathCandidates(folder = folder), paste0(dbName, ".duckdb")
  )

  found <- candidates[file.exists(candidates)]

  if (length(found) == 0) return(NULL)

  normalizePath(found[1], winslash = "/", mustWork = FALSE)
}
