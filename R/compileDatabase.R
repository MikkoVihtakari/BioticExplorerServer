#' @title Download IMR Biotic database and to place it into a \link[duckdb]{duckdb} database
#' @description Downloads, formulates and indexes IMR Biotic database into a format used by BioticExplorer
#' @param years Vector of integers specifying the years to be downloaded. The database reaches 1914:year(Sys.Date())
#' @param dbPath Character string specifying the folder where the \link[duckdb]{duckdb} and \link[=indexDatabase]{dbIndex} files should be saved. Defaults to \code{\link{defaultDbPath}()}, which is \code{~/IMR_biotic_BES_database} on macOS and Linux and \code{\%USERPROFILE\%\\IMR_biotic_BES_database} on Windows, where R's \code{~} expansion may otherwise land the database in a OneDrive-synchronized Documents folder.
#' @param dbIndexFile Character string specifying the file path where the index of the database should be saved. Must include \code{.rda} at the end. Defaults to \code{dbIndex.rda} inside \code{dbPath}. The index is used by \href{https://github.com/DeepWaterIMR/BioticExplorer}{BioticExplorer}.
#' @param dbName Character string or \code{NULL}. If \code{NULL} uses the default name ("bioticexplorer").
#' @param overwrite Logical indicating whether requested years and reference tables already present in the \link[duckdb]{duckdb} database should be downloaded again and replaced. Existing annual rows are deleted transactionally before replacement, so they are not duplicated.
#' @details Runs the \code{\link{prepareCruiseSeriesList}}, \code{\link{prepareGearList}}, \code{\link{prepareTaxaList}}, \code{\link{prepareReferenceCodes}}, \code{\link{downloadDatabase}} and \code{\link{indexDatabase}} functions, and saves the results into a \link[duckdb]{duckdb}. The cruise-series, gear and taxa reference lists are written as the \code{csindex}, \code{gearindex} and \code{taxaindex} tables, respectively, and coded \code{KeyType} fields exposed by the Reference API (such as \code{sex}, \code{maturationstage}, and \code{nation}) are written as the long-format \code{codeindex} table so they can be decoded offline with a join. Completed databases are stamped with the package and database-schema versions used to build them; \code{\link{updateDatabase}} uses this information to decide whether an incremental update is safe. Be aware that running these functions requires access to the IMR intranet and reasonably stable internet. It is advisable to run the function in a separate R session or in a screen session in the terminal on Unix machines, as downloading the database takes several hours and requires a stable internet connection. If the connection is unstable, the function may return an error. In such cases, ensure that the connection is stable and rerun the function. The function should continue downloading from where it left off.
#' @return Called for its side effects: creates and populates a DuckDB database and index file. Returns \code{NULL} invisibly.
#' @import data.table
#' @author Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research)
#' @export

# years = 1914; dbPath = "~/Desktop/IMR_biotic_BES_database"; dbIndexFile = file.path(dbPath, "dbIndex.rda"); dbName = NULL; overwrite = TRUE
# compileDatabase(years = 1914, dbName = "spedenpatukka", overwrite = TRUE)

compileDatabase <- function(
  years = 1900:data.table::year(Sys.time()),
  dbPath = defaultDbPath(),
  dbIndexFile = file.path(dbPath, "dbIndex.rda"),
  dbName = NULL,
  overwrite = FALSE
) {
  operation_started_at <- .start_operation_timer("Compilation")
  operation_succeeded <- FALSE
  on.exit({
    if (operation_succeeded) {
      .finish_operation_timer("Compilation", operation_started_at)
    }
  }, add = TRUE)

  ## Expand the paths before they are used

  dbPath <- path.expand(dbPath)
  dbIndexFile <- path.expand(dbIndexFile)

  ## Create the database folder if it does not exist

  if (!dir.exists(dbPath)) {
    ## utils::menu() cannot be answered in a background Rscript, which is how the
    ## download is normally run. Create the folder without asking in that case.

    if (!interactive()) {
      ret.val <- 1
    } else {
      message(dbPath, " does not exist. Do you want to create the folder?")
      ret.val <- utils::menu(c("Yes", "No"), "")
    }

    if (ret.val != 1) {
      msg <- paste0(
        "Selected not to create the folder. Redefine dbPath and try again."
      )
      stop(paste(strwrap(msg), collapse = "\n"))
    } else {
      dir.create(dbPath, recursive = TRUE)
      msg <- paste0("duckdb IMR database created to ", dbPath)
      message(paste(strwrap(msg), collapse = "\n"))
    }
  }

  ## Define dbName and dbIndexPath

  if (is.null(dbName)) {
    dbName <- "bioticexplorer"
  }
  # if(Sys.getenv(c("SERVER_MODE"))=="") {
  #   dbHost <- "localhost"
  #   if(is.null(dbName)) dbName <- "bioticexplorer"
  # } else {
  #   dbHost <- "dbserver"
  #   if(is.null(dbName)) dbName <- "bioticexplorer-next"
  # }

  con_db <-
    try(
      {
        DBI::dbConnect(
          duckdb::duckdb(
            dbdir = normalizePath(
              paste0(file.path(dbPath, dbName), ".duckdb"),
              mustWork = FALSE
            )
          )
        )
      },
      silent = TRUE
    )

  ## Cruise series

  message("1. Compiling cruise series list")
  if (
    inherits(try(dplyr::tbl(con_db, "csindex"), silent = TRUE), "try-error") |
      overwrite
  ) {
    cruiseSeries <- prepareCruiseSeriesList()
  } else {
    cruiseSeries <- dplyr::collect(dplyr::tbl(con_db, "csindex"))
    message(
      "Cruise series information found from ",
      dbName,
      ". The information was not rewritten. Delete the database or use the overwrite argument if you want to re-download the data."
    )
  }

  if (
    inherits(try(dplyr::tbl(con_db, "csindex"), silent = TRUE), "try-error") |
      overwrite
  ) {
    DBI::dbWriteTable(con_db, "csindex", cruiseSeries, overwrite = overwrite) #, csvdump = TRUE,
    #transaction = FALSE, overwrite = TRUE)
  }

  ## Gear list

  message("2. Compiling gear list")

  if (
    inherits(try(dplyr::tbl(con_db, "gearindex"), silent = TRUE), "try-error") |
      overwrite
  ) {
    gearCodes <- prepareGearList()
  } else {
    gearCodes <- dplyr::collect(dplyr::tbl(con_db, "gearindex"))
    message(
      "Gear codes found from ",
      dbName,
      ". The information was not rewritten. Delete the database or use the overwrite argument if you want to re-download the data."
    )
  }

  if (
    inherits(try(dplyr::tbl(con_db, "gearindex"), silent = TRUE), "try-error") |
      overwrite
  ) {
    DBI::dbWriteTable(con_db, "gearindex", gearCodes, overwrite = overwrite) #csvdump = TRUE,
    # transaction = FALSE, overwrite = TRUE)
  }

  ## Taxa list

  message("3. Compiling taxa list")

  if (
    inherits(try(dplyr::tbl(con_db, "taxaindex"), silent = TRUE), "try-error") |
      overwrite
  ) {
    taxaList <- prepareTaxaList()
  } else {
    taxaList <- dplyr::collect(dplyr::tbl(con_db, "taxaindex"))
    message(
      "Taxa list found from ",
      dbName,
      ". The information was not rewritten. Delete the database or use the overwrite argument if you want to re-download the data."
    )
  }

  ## Reference codes (coded KeyType fields: sex, maturationstage, …)

  message("4. Compiling reference codes")

  if (
    inherits(try(dplyr::tbl(con_db, "codeindex"), silent = TRUE), "try-error") |
      overwrite
  ) {
    codeIndex <- prepareReferenceCodes()
    if (nrow(codeIndex) > 0) {
      DBI::dbWriteTable(con_db, "codeindex", codeIndex, overwrite = overwrite)
    } else {
      message(
        "No reference codes could be read (off the IMR network?). The codeindex table ",
        "was not written; agents fall back to the cached codes in BAIT or the API."
      )
    }
  } else {
    message(
      "Reference codes found from ",
      dbName,
      ". The information was not rewritten. Delete the database or use the overwrite argument if you want to re-download the data."
    )
  }

  ## Download

  message("5. Compiling database")
  downloadDatabase(
    years = years,
    connection = con_db,
    cruiseSeries = cruiseSeries,
    gearCodes = gearCodes,
    taxaList = taxaList,
    overwrite = overwrite
  )

  # Index

  message("6. Indexing database")
  indexDatabase(connection = con_db, dbIndexFile = dbIndexFile)

  DBI::dbDisconnect(con_db)
  operation_succeeded <- TRUE
  invisible(NULL)
}
