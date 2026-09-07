#' @title Prepare coded-field reference list
#' @description Downloads and compiles the simple (non-composite) coded-field reference
#'   tables from the IMR NMD Reference API into a single long-format lookup. These are the
#'   Biotic columns flagged as codes of type \code{KeyType} (e.g. \code{sex},
#'   \code{maturationstage}, \code{samplequality}, \code{gearcondition}) whose meaning is otherwise
#'   only resolvable against the API. The result is written to the DuckDB database as the
#'   \code{codeindex} table by \code{\link{compileDatabase}}, so agents and the Shiny app can
#'   decode these fields offline with a join instead of a per-code network call.
#' @param tables Character vector of reference datasets to pull. Defaults to the coded
#'   \code{KeyType} columns that occur in \code{mission}/\code{stnall}/\code{indall}
#'   and are exposed as Reference API datasets. The vector may be \emph{named}: names are
#'   the Biotic column (written to the \code{reftable} column so a join on the column name
#'   works), values are the Reference API dataset that defines the codes. Unnamed elements
#'   use the dataset name as the label.
#'   Note that a few Biotic columns map to a differently named reference dataset. The
#'   \code{stationtype} column resolves against the \code{fishstationtype} dataset and is
#'   handled here through that naming. The \code{gear} column resolves against the
#'   \code{equipment} table — handled separately by \code{\link{prepareGearList}} and
#'   therefore not included here.
#' @param lang Language for \code{shortname}/\code{description}: \code{"en"} (default) or
#'   \code{"no"}.
#' @details Reads each table from \code{.../reference/v2/dataset/\{table\}} (the same endpoint
#'   family used by \code{\link{prepareGearList}}) and keeps only the \code{code -> meaning}
#'   mapping. Deprecated reference rows are excluded. Editor-identity columns from the registry (\code{updatedBy}, \code{insertedBy},
#'   timestamps, \code{...By} fields) are dropped on purpose so no staff usernames land in the
#'   database. Tables that fail to download (e.g. off the IMR network) are skipped with a
#'   warning rather than aborting the build. Composite reference tables that are keyed by taxa
#'   and/or sex (\code{specialstage}, \code{eggstage}, \code{moultingstage},
#'   \code{spawningfrequency}) are intentionally \emph{not} handled here — they need a
#'   taxa/sex-aware lookup and are left to the API.
#' @return A \code{\link[data.table]{data.table}} with columns \code{reftable}, \code{code},
#'   \code{shortname}, and \code{description}, stacked across all successfully read tables.
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @export

prepareReferenceCodes <- function(tables = NULL, lang = c("en", "no")) {

  lang <- match.arg(lang)

  # Default set of simple KeyType coded columns. Unnamed elements are columns whose name
  # matches the reference dataset; named elements map a Biotic column (the name) onto the
  # differently named dataset that defines its codes (the value). `stationtype` is stored
  # in Biotic under that column name, but the registry publishes it as `fishstationtype`.
  if (is.null(tables)) {
    tables <- c(
      "sex", "maturationstage", "nation",
      "samplequality", "gearcondition", "haulvalidity", "sampletype",
      "agingstructure", "lengthmeasurement", "lengthresolution", "fat",
      "digestion", "liver", "identification", "abundancecategory",
      stationtype = "fishstationtype", "samplerecipient"
    )
  }

  # Label each dataset with the Biotic column name so `reftable` can be joined directly
  # against the column being decoded.
  labels <- names(tables)
  if (is.null(labels)) labels <- rep("", length(tables))
  labels[is.na(labels) | labels == ""] <- tables[is.na(labels) | labels == ""]

  pb <- utils::txtProgressBar(max = length(tables), style = 3)

  # Read one reference dataset and reduce it to code -> meaning rows
  readOne <- function(i) {
    utils::setTxtProgressBar(pb, i)
    tab <- unname(tables[i])
    label <- unname(labels[i])

    url <- sprintf(
      "%s/dataset/%s?version=2.0&lang=%s", .REFERENCE_API_BASE, tab, lang
    )

    doc <- try(.reference_api_get_xml(url), silent = TRUE)
    if (inherits(doc, "try-error")) {
      warning("Could not read reference table '", tab, "' (skipped): ",
              conditionMessage(attr(doc, "condition")), call. = FALSE)
      return(NULL)
    }

    row_nodes <- xml2::xml_find_all(doc, "//d1:row")
    deprecated <- tolower(trimws(xml2::xml_attr(row_nodes, "deprecated")))
    row_nodes <- row_nodes[is.na(deprecated) | deprecated %in% c("", "false", "0", "no")]

    rows <- lapply(row_nodes, function(x) {
      ch <- xml2::xml_children(x)
      y <- xml2::xml_text(ch)
      names(y) <- xml2::xml_name(ch)
      data.table::data.table(t(y))
    })

    if (length(rows) == 0) return(NULL)
    out <- data.table::rbindlist(rows, fill = TRUE)
    out[, reftable := label]
    out[]
  }

  raw <- data.table::rbindlist(lapply(seq_along(tables), readOne), fill = TRUE)
  close(pb)

  if (nrow(raw) == 0) {
    warning("No reference tables could be read. Are you on the IMR network/VPN?",
            call. = FALSE)
    return(data.table::data.table(
      reftable = character(), code = character(),
      shortname = character(), description = character()
    ))
  }

  # Some datasets call the human-readable label "name", others "shortname"
  if (!"shortname" %in% names(raw) && "name" %in% names(raw)) {
    data.table::setnames(raw, "name", "shortname")
  }

  # Keep ONLY the code -> meaning mapping. Dropping everything else also strips the
  # registry's editor-identity columns (updatedBy/insertedBy/... and timestamps), so no
  # staff usernames are written to the database.
  keep <- intersect(c("reftable", "code", "shortname", "description"), names(raw))
  codeindex <- raw[, ..keep]

  data.table::setcolorder(
    codeindex,
    intersect(c("reftable", "code", "shortname", "description"), names(codeindex))
  )

  return(codeindex[])
}
