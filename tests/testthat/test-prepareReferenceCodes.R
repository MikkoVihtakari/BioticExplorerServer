quality_reference_xml <- function() {
  xml2::read_xml(
    paste0(
      '<list xmlns="http://www.imr.no/formats/nmdreference/v2.0" ',
      'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">',
      '<row xsi:type="KeyValueElementType" deprecated="false">',
      '<code>1</code><shortname>Active</shortname><description>Active code</description>',
      '</row>',
      '<row xsi:type="KeyValueElementType" deprecated="true">',
      '<code>101</code><shortname>Deprecated</shortname><description>Old code</description>',
      '</row>',
      '</list>'
    )
  )
}

test_that("default reference tables include central survey-quality fields", {
  requested_urls <- character()

  local_mocked_bindings(
    .reference_api_get_xml = function(url) {
      requested_urls <<- c(requested_urls, url)
      quality_reference_xml()
    },
    .package = "BioticExplorerServer"
  )

  codes <- suppressWarnings(prepareReferenceCodes())

  expect_true(any(grepl("/dataset/samplequality\\?", requested_urls)))
  expect_true(any(grepl("/dataset/gearcondition\\?", requested_urls)))
  expect_setequal(unique(codes$reftable[codes$reftable %in% c("samplequality", "gearcondition")]),
                  c("samplequality", "gearcondition"))
})

test_that("deprecated reference rows are excluded", {
  local_mocked_bindings(
    .reference_api_get_xml = function(url) quality_reference_xml(),
    .package = "BioticExplorerServer"
  )

  codes <- suppressWarnings(prepareReferenceCodes(tables = "gearcondition"))

  expect_identical(codes$code, "1")
  expect_false("deprecated" %in% names(codes))
  expect_setequal(names(codes), c("reftable", "code", "shortname", "description"))
})
