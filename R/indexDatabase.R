#' @title Index BioticExplorer database
#' @description Loads BioticExplorer database and creates an index used by BioticExplorer to save processing time
#' @param dbIndexPath Character string specifying the file path where the database should be saved. Must include \code{.rda} at the end.
#' @param fileOnly Logical indicating whether the result should only be saved to a file and not returned. If FALSE, no file is made and the result is returned instead.
#' @inheritParams downloadDatabase
#' @import data.table DBI
#' @rawNamespace import(dplyr, except = c(last, first, between))
#' @author Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research)
#' @export

# dbPath = "~/Desktop/IMR_db.duckdb"; dbIndexPath = "~/Desktop/dbIndex.rda"
indexDatabase <- function(dbIndexPath = "~/Desktop/dbIndex.rda", fileOnly = TRUE, dbName = NULL) {
  
  pb <- utils::txtProgressBar(max = 6, style = 3)
  
  if(Sys.getenv(c("SERVER_MODE"))=="") {
    dbHost <- "localhost"
    if(is.null(dbName)) dbName <- "bioticexplorer"
  } else {
    dbHost <- "dbserver"
    if(is.null(dbName)) dbName <- "bioticexplorer-next"
  }
  
  if(!DBI::dbCanConnect(MonetDB.R::MonetDB(), host=dbHost, dbname=dbName, user="monetdb", password="monetdb")) {
    stop("Cannot connect to the database")
  }
  
  con_db <- DBI::dbConnect(MonetDB.R::MonetDB.R(), host=dbHost, dbname=dbName, user="monetdb", password="monetdb")
  
  utils::setTxtProgressBar(pb, 1)
  
  rv <- list()
  rv$inputData$stnall <- dplyr::tbl(con_db, "stnall")
  rv$inputData$indall <- dplyr::tbl(con_db, "indall")
  rv$inputData$mission <- dplyr::tbl(con_db, "mission")
  rv$inputData$meta <- dplyr::tbl(con_db, "metadata")
  rv$inputData$csindex <- dplyr::tbl(con_db, "csindex")
  rv$inputData$filesize <- dplyr::tbl(con_db, "filesize")
  # rv$inputData$gearlist <- dplyr::tbl(con_db, "gearindex")
  
  utils::setTxtProgressBar(pb, 2)
  
  ## Index
  
  index <- list()
  index$missiontypename <- rv$inputData$mission %>% select(missiontypename) %>% distinct() %>% pull() %>% sort()
  index$cruise <- rv$inputData$mission %>% select(cruise) %>% distinct() %>% pull() %>% sort()
  index$year <- rv$inputData$mission %>% select(startyear) %>% distinct() %>% pull() %>% sort()
  index$nstations <- rv$inputData$stnall %>% select(missionid, startyear, serialnumber) %>% distinct() %>% count() %>% pull()
  
  utils::setTxtProgressBar(pb, 3)
  
  index$commonname <- rv$inputData$stnall %>% select(commonname) %>% distinct() %>% pull() %>% sort()
  index$platformname <- rv$inputData$stnall %>% select(platformname) %>% distinct() %>% pull() %>% sort()
  index$serialnumber <- rv$inputData$stnall %>% select(serialnumber) %>% distinct() %>% pull() %>% sort()
  index$gear <- rv$inputData$stnall %>% select(gear) %>% distinct() %>% pull() %>% sort()
  index$gearcategory <- rv$inputData$stnall %>% select(gearcategory) %>% distinct() %>% pull() %>% sort()
  index$date <- rv$inputData$stnall %>% summarise(min = min(stationstartdate, na.rm = TRUE), max = max(stationstartdate, na.rm = TRUE)) %>% collect()
  
  utils::setTxtProgressBar(pb, 4)
  
  index$nmeasured <- rv$inputData$indall %>% select(length) %>% count() %>% pull()
  
  utils::setTxtProgressBar(pb, 5)
  
  tmp <- rv$inputData$csindex %>% select(cruiseseriescode, name) %>% distinct() %>% arrange(cruiseseriescode) %>% collect()
  index$cruiseseries <- tmp$cruiseseriescode
  names(index$cruiseseries) <- tmp$name
  
  index$icesarea <- rv$inputData$stnall %>% select(icesarea) %>% distinct() %>% pull() %>% sort()
  index$fdirarea <- rv$inputData$stnall %>% select(area) %>% distinct() %>% pull() %>% sort() 
  
  utils::setTxtProgressBar(pb, 6)
  
  index$downloadstart <- rv$inputData$meta %>% select(timestart) %>% pull()
  index$downloadend <- rv$inputData$meta %>% select(timeend) %>% pull()
  index$filesize <- rv$inputData$filesize %>% summarise(size = sum(filesize, na.rm = TRUE)/1e9) %>% pull() # in GB

  DBI::dbDisconnect(con_db)
  
  ## Save and return
  
  utils::setTxtProgressBar(pb, 7)
  
  if(fileOnly) {
    save(index, file = dbIndexPath, compress = "xz")
  } else {
    return(index)
  }
}



