#' @title Prepare taxa list
#' @description Downloads and compiles the taxa reference list from the IMR API. The result can be bundled as package data. Run this function and save with \code{usethis::use_data(taxaList, overwrite = TRUE)} to refresh the bundled data.
#' @param verbose Logical indicating whether the function should print progress messages.
#' @return A \code{\link[data.table]{data.table}} with columns \code{tsn} and \code{name} (plus additional synonym columns where available). Rows where \code{name} or \code{tsn} is missing are excluded.
#' @details Records served by the Reference API occasionally lack a \code{tsn} or contain empty elements. Such records cannot be linked to Biotic data and are dropped with a warning instead of aborting the compilation.
#' @author Mikko Vihtakari, Ibrahim Umar
#' @export

prepareTaxaList <- function(verbose = FALSE) {
  
  # Read taxa list reference
  taxa_url <- paste0(.REFERENCE_API_BASE, "/dataset/taxa?version=2.0")
  if(verbose) message("Downloading ", taxa_url, "...")
  
  doc <- .reference_api_get_xml(taxa_url)
  
  if(verbose) message("Reading the xml file...")
  
  .parseTaxaXml(doc, verbose = verbose)
}

# Turn the taxa reference document into a tsn - synonym table.
#
# The rows and their synonyms are read with vectorised xml2 calls and mapped
# back to their parent through element counts. Querying each node separately
# (or converting the document with xml2::as_list()) is orders of magnitude
# slower on the ~20 MB taxa document, and the earlier as_list() approach also
# aborted the whole compilation when a single record had an empty <tsn>
# element (issues #6 and #7).
.parseTaxaXml <- function(doc, verbose = FALSE) {
  
  rows <- xml2::xml_find_all(doc, "//d1:row")
  
  if(length(rows) == 0) {
    stop("The taxa reference document contains no records. Are you on the IMR network/VPN?",
         call. = FALSE)
  }
  
  ## Row level elements ####
  
  rowChildren <- xml2::xml_children(rows)
  rowOfChild <- rep(seq_along(rows), xml2::xml_length(rows))
  rowChildNames <- xml2::xml_name(rowChildren)
  
  tsn <- rep(NA_character_, length(rows))
  sel <- which(rowChildNames == "tsn")
  tsn[rowOfChild[sel]] <- .emptyToNA(xml2::xml_text(rowChildren[sel]))
  
  ## Synonyms ####
  
  sel <- which(rowChildNames == "TaxaSynonyms")
  synonymParents <- rowChildren[sel]
  rowOfSynonym <- rep(rowOfChild[sel], xml2::xml_length(synonymParents))
  synonyms <- xml2::xml_children(synonymParents)
  
  if(length(synonyms) == 0) {
    stop("No taxa synonyms found in the taxa reference document.", call. = FALSE)
  }
  
  synonymChildren <- xml2::xml_children(synonyms)
  synonymOfChild <- rep(seq_along(synonyms), xml2::xml_length(synonyms))
  synonymChildNames <- xml2::xml_name(synonymChildren)
  synonymChildText <- .emptyToNA(xml2::xml_text(synonymChildren))
  
  if(verbose) message("Merging tsn lists...")
  
  synonymFields <- unique(synonymChildNames)
  
  out <- lapply(synonymFields, function(field) {
    x <- rep(NA_character_, length(synonyms))
    sel <- which(synonymChildNames == field)
    x[synonymOfChild[sel]] <- synonymChildText[sel]
    x
  })
  names(out) <- synonymFields
  
  splist <- data.table::data.table(
    tsn = tsn[rowOfSynonym], data.table::as.data.table(out)
  )
  
  ## Combine and format ####
  
  if(verbose) message("Finishing...")
  
  if(!"name" %in% names(splist)) {
    stop("The taxa reference document contains no taxa names. Has the API changed?",
         call. = FALSE)
  }
  
  splist <- splist[!is.na(name),]
  
  # Records without a tsn cannot be linked to Biotic data
  nMissingTsn <- sum(is.na(splist$tsn))
  
  if(nMissingTsn > 0) {
    warning(nMissingTsn, " taxa record(s) returned by the Reference API had no tsn ",
            "and were excluded from the taxa list.", call. = FALSE)
    splist <- splist[!is.na(tsn),]
  }
  
  return(splist[])
}

# Empty xml elements are read as "". Treat them as missing values.
.emptyToNA <- function(x) {
  x[!nzchar(trimws(x))] <- NA_character_
  x
}
