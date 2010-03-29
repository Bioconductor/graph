
.indexToCoord <- function(i, nrow)
{
    ## only for square
    if (nrow == 1L) return(cbind(1L, 1L))
    ans <- matrix(0L, nrow = length(i), ncol = 2L)
    ans[ , 2L] <- ((i - 1L) %/% nrow) + 1L
    ans[ , 1L] <- ((i - 1L) %% nrow) + 1L
    ans
}

.coordToIndex <- function(x, y, nrow) {
    if (nrow == 1L) return(1L)
    (y * nrow) - (nrow - x)
}

.rowIndex <- function(x, n_row, n_col)
    .coordToIndex(rep(x, n_col), seq_len(n_col), n_row)

.columnIndex <- function(y, n_row)
    .coordToIndex(seq_len(n_row), rep(y, n_row), n_row)

makebits <- function(n, bitdim=NULL) {
    if (!is.null(bitdim)) bitdim <- as.integer(bitdim)
    n <- as.integer(n)
    structure(raw(ceiling(n / 8)),
              bitlen = n, bitdim = bitdim, nbitset = 0L)
}

bitdim <- function(x, dims)
{
    attr(x, "bitdim") <- dims
    x
}

setBitCell <- function(xx, x, y, val)
{
    dim <- attr(xx, "bitdim")
    idx <- .coordToIndex(x, y, dim[1L])
    setbitv(xx, idx, val)
}

getBitCell <- function(xx, x, y)
{
    dim <- attr(xx, "bitdim")
    idx <- .coordToIndex(x, y, dim[1L])
    testbit(xx, idx)
}

getColumn <- function(xx, y)
{
    dim <- attr(xx, "bitdim")
    idx <- .columnIndex(y, dim[1L])
    testbit(xx, idx)
    ## wonder if there is a nice optimization since we will be reading
    ## consecutive bits for column oriented storage.
}

getRow <- function(xx, x)
{
    dim <- attr(xx, "bitdim")
    n_row <- dim[1L]
    n_col <- dim[2L]
    idx <- .rowIndex(x, n_row, n_col)
    testbit(xx, idx)
}

bitlen <- function(x) attr(x, "bitlen")

nbitset <- function(x) attr(x, "nbitset")

bitToLogical <- function(x) {
    len <- attr(x, "bitlen")
    if (is.null(len)) len <- length(x) * 8L
    sapply(seq_len(len), function(i) testbit(x, i))
}

setbitv <- function(xx, ii, v)
{
    .Call(graph:::graph_bitarray_set, xx, ii, v)
}


## can we vectorize these?
setbit <- function(xx, ii)
{
    i <- ii - 1L
    byteIdx <- (i %/% 8L) + 1L
    bit <- (i %% 8L)
    byte <- xx[byteIdx]
    xx[byteIdx] <- byte | rawShift(as.raw(1L), bit)
    xx
}

testbit <- function(xx, ii)
{
    i <- ii - 1L
    byteIdx <- (i %/% 8L) + 1L
    bit <- (i %% 8L)
    byte <- xx[byteIdx]
    ans <- logical(length(byte))
    for (i in seq_len(length(ans))) {
        ans[i] <- as.logical(byte[i] & rawShift(as.raw(1L), bit[i]))
    }
    ans
}

sumbits <- function(xx, ii)
{
    s <- 0L
    i <- ii - 1L
    byteIdx <- (i %/% 8L) + 1L
    bit <- (i %% 8L)
    byte <- xx[byteIdx]
    for (i in seq_len(length(byte))) {
        s <- s + as.logical(byte[i] & rawShift(as.raw(1L), bit[i]))
    }
    s
}


bitToInteger <- function(x) {
    len <- attr(x, "bitlen")
    if (is.null(len)) len <- length(x) * 8L
    sapply(seq_len(len), function(i) if (testbit(x, i)) 1L else 0L)
}

bitToMat <- function(x) {
    len <- attr(x, "bitlen")
    bitdim <- attr(x, "bitdim")
    matrix(sapply(seq_len(len), function(i) if (testbit(x, i)) 1L else 0L),
           nrow = bitdim[1L], ncol = bitdim[2L])
}
