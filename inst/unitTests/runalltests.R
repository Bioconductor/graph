require("RUnit", quietly=TRUE) || stop("RUnit package not found")
require("graph")

## Override checkException so that it will be quiet
myCheckException <- function (expr, msg, silent=TRUE) {
    if (exists(".testLogger", envir = .GlobalEnv)) {
        .testLogger$incrementCheckNum()
    }
    if (!inherits(try(eval(expr, envir = parent.frame()), silent=silent), "try-error")) {
        if (exists(".testLogger", envir = .GlobalEnv)) {
            .testLogger$setFailure()
        }
        stop("Error not generated as expected.")
    }
    else {
        return(TRUE)
    }
}

TEST_DATA_DIR <- "data"
runitPat <- ".*_test\.[rR]$"
runitDirs <- c(".")
suite <- defineTestSuite(name="graph Test Suite",
                         dirs=runitDirs,
                         testFileRegexp=runitPat)
result <- runTestSuite(suite)

printTextProtocol(result, showDetails=FALSE)

nil <- printHTMLProtocol(result, fileName="runit-result.html")

