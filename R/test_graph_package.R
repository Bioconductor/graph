.test <- function(dir, pattern = ".*_test\\.R$") {
    if (missing(dir))
        dir <- system.file(package="graph", "unitTests")
    BiocGenerics:::testPackage("graph", dir, pattern=pattern)
}
