test_graph_package <- function(dir) {
    if (missing(dir)) {
        dir <- system.file("unitTests", package="graph")
    }
    require("RUnit", quietly=TRUE) || stop("RUnit package not found")
    suite <- defineTestSuite(name="graph RUnit Tests", dirs=dir,
                             testFileRegexp=".*_test\\.R$",
                             rngKind="default",
                             rngNormalKind="default")
    result <- runTestSuite(suite)
    printTextProtocol(result, showDetails=FALSE)
    if (.any_errors(result) || .any_fail(result)) {
        stop("test_graph_package FAIL")
    }
    result
}

.any_errors <- function(result) {
    any(sapply(result, function(r) r$nErr > 0))
}

.any_fail <- function(result) {
    any(sapply(result, function(r) r$nFail > 0))
}
