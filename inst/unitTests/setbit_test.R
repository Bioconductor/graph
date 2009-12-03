.testbit <- graph:::testbit
setbitv <- graph:::setbitv
setbit <- graph:::setbit

test_setbitv <- function()
{
    xx <- raw(5)
    len <- 5L * 8L
    for (i in seq_len(len)) {
        checkEquals(FALSE, .testbit(xx, i))
        xx <- setbitv(xx, i, FALSE)
        checkEquals(FALSE, .testbit(xx, i))
    }

    xx <- raw(5)
    for (i in seq_len(len)) {
        checkEquals(FALSE, .testbit(xx, i))
        xx <- setbitv(xx, i, TRUE)
        checkEquals(TRUE, .testbit(xx, i))
    }
    for (i in seq_len(len)) checkEquals(TRUE, .testbit(xx, i))
    for (i in seq_len(len)) xx <- setbitv(xx, i, FALSE)
    for (i in seq_len(len)) checkEquals(FALSE, .testbit(xx, i))
}

test_setbitv_vectorized <- function()
{
    xx <- raw(3)
    len <- 3L * 8L
    xxall <- setbitv(xx, 1:10, rep(TRUE, 10))
    checkTrue(all(.testbit(xxall, 1:10)))
    checkTrue(!any(.testbit(xxall, 11:len)))
}

test_setbit_basics <- function()
{
    xx <- raw(5)
    tf <- function(n) rawToBits(setbit(xx, n)[1])
    got <- as.logical(do.call(rbind, lapply(1:8, tf)))
    dim(got) <- c(8, 8)
    want <- matrix(FALSE, nrow=8, ncol=8)
    diag(want) <- TRUE
    checkEquals(want, got)

    tf <- function(n) rawToBits(setbit(xx, n)[2])
    got <- as.logical(do.call(rbind, lapply(9:16, tf)))
    dim(got) <- c(8, 8)
    want <- matrix(FALSE, nrow=8, ncol=8)
    diag(want) <- TRUE
    checkEquals(want, got)

    x2 <- setbit(xx, 38)
    for (i in 1:4) checkTrue(!as.logical(x2[i]))
    checkTrue(as.logical(x2[5]))
}

test_testbit <- function()
{
    xx <- raw(5)
    for (i in 1:(5 * 8)) {
        checkTrue(!.testbit(xx, i))
    }
    checkTrue(!any(.testbit(xx, 1:40)))

    xx <- as.raw(rep(255L, 5))
    for (i in 1:(5 * 8)) {
        checkTrue(.testbit(xx, i), i)
    }
    checkTrue(all(.testbit(xx, 1:40)))

    xx <- setbit(as.raw(5), 23)
    checkTrue(.testbit(xx, 23))
    checkEquals(c(TRUE, TRUE), .testbit(xx, c(23, 23)))
    checkEquals(c(FALSE, TRUE), .testbit(xx, c(21, 23)))
    checkEquals(c(TRUE, FALSE), .testbit(xx, c(23, 24)))
}
