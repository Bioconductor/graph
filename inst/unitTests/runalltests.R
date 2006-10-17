require("RUnit", quietly=TRUE) || stop("RUnit package not found")
require("graph")


TEST_DATA_DIR <- "data"
runitPat <- ".*_test\\.[rR]$"
runitDirs <- c(".")
suite <- defineTestSuite(name="graph Test Suite",
                         dirs=runitDirs,
                         testFileRegexp=runitPat,
                         rngKind="default",
                         rngNormalKind="default")
result <- runTestSuite(suite)
printTextProtocol(result, showDetails=FALSE)


