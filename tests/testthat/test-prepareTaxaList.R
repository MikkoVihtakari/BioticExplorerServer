taxa_row <- function(tsn, synonyms = "") {
  paste0(
    '<row xsi:type="TaxaElementType" deprecated="false">',
    '<tsn>', tsn, '</tsn><imr>false</imr><ruCode>0</ruCode>',
    if (nzchar(synonyms)) paste0('<TaxaSynonyms>', synonyms, '</TaxaSynonyms>'),
    '</row>'
  )
}

taxa_synonym <- function(name, num = 1, description = NULL) {
  paste0(
    '<synonym deprecated="false">',
    '<idTaxa>id-', num, '</idTaxa><num>', num, '</num>',
    '<languageCode>1</languageCode><language>Scientific</language>',
    '<name>', name, '</name>',
    '<preferred>true</preferred><tradename>false</tradename>',
    if (!is.null(description)) paste0('<description>', description, '</description>'),
    '</synonym>'
  )
}

taxa_xml <- function(rows) {
  xml2::read_xml(
    paste0(
      '<list xmlns="http://www.imr.no/formats/nmdreference/v2.0" ',
      'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">',
      paste(rows, collapse = ""),
      '</list>'
    )
  )
}

mock_taxa_api <- function(rows) {
  local_mocked_bindings(
    .reference_api_get_xml = function(url) taxa_xml(rows),
    .package = "BioticExplorerServer",
    .env = parent.frame()
  )
}

test_that("taxa records are compiled into one row per synonym", {
  mock_taxa_api(c(
    taxa_row(180541, paste0(taxa_synonym("Ursus"), taxa_synonym("Bear", num = 2))),
    taxa_row(180540, taxa_synonym("Ursidae"))
  ))

  taxa <- prepareTaxaList()

  expect_s3_class(taxa, "data.table")
  expect_identical(taxa$tsn, c("180541", "180541", "180540"))
  expect_identical(taxa$name, c("Ursus", "Bear", "Ursidae"))
  expect_identical(names(taxa)[1:2], c("tsn", "idTaxa"))
})

test_that("records with an empty tsn do not abort the compilation (#6, #7)", {
  mock_taxa_api(c(
    taxa_row(180541, taxa_synonym("Ursus")),
    taxa_row("")  # record served by the API without a tsn and without synonyms
  ))

  expect_silent(taxa <- prepareTaxaList())
  expect_identical(taxa$tsn, "180541")
  expect_identical(taxa$name, "Ursus")
})

test_that("named records without a tsn are dropped with a warning", {
  mock_taxa_api(c(
    taxa_row(180541, taxa_synonym("Ursus")),
    taxa_row("", taxa_synonym("Nameless taxon"))
  ))

  expect_warning(taxa <- prepareTaxaList(), "no tsn")
  expect_identical(taxa$tsn, "180541")
  expect_false("Nameless taxon" %in% taxa$name)
})

test_that("synonyms without a name are excluded and missing fields become NA", {
  mock_taxa_api(c(
    taxa_row(180541, paste0(
      taxa_synonym("Ursus", description = "Bears"),
      taxa_synonym("", num = 2)
    )),
    taxa_row(180540, taxa_synonym("Ursidae"))
  ))

  taxa <- prepareTaxaList()

  expect_identical(taxa$name, c("Ursus", "Ursidae"))
  expect_identical(taxa$description, c("Bears", NA_character_))
})

test_that("an empty taxa document is reported rather than parsed", {
  mock_taxa_api(character())

  expect_error(prepareTaxaList(), "no records")
})
