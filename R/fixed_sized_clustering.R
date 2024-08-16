#' Create clusters of fixed size
#'
#' Create clusters of fixes size using the nearest neighbor approach.
#'
#' @param d `dist`, dissimilarity structure as produced by `dist`.
#' @param size `integer`, fixed target size of the clusters.
#' @return `integer`, vector of the same length as `attr(d, "Size")`
#'  with cluster identification numbers.
#' @export
#' @examples
#' x <- c(1:3, 1e4, 11:13, 1.1e4, 111:113)
#' d <- dist(x)
#' fixed_sized_clustering(d, size = 3)
fixed_sized_clustering <- function(d, size) {
    if (!inherits(d, "dist"))
        stop("'d' has to be a dissimilarity 'dist' structure.")

    dist_mat <- as.matrix(d)
    nr <- nrow(dist_mat)

    if (size[1L] > nr)
        stop("Size of clusters has to be lower than size of 'dist'.")

    cl <- rep.int(NA_integer_, nr)
    ncl <- ceiling(nr / size[1L])
    sizes <- rep.int(size[1L], ncl)

    remainder <- nr %% size[1L]
    if (as.logical(remainder))
        sizes[length(sizes)] <- remainder

    i <- 1L

    while (anyNA(cl)) {
        rs <- rowSums(dist_mat, na.rm = TRUE)

        ## find nearest neighbors
        i_min_sum_dist <- which.min(rs)
        sel <- head(order(dist_mat[i_min_sum_dist,]), sizes[i])

        cl[sel] <- i

        ## generate high rowsums for already selected rows
        dist_mat[sel,] <- Inf
        ## disable already selected values (in columns) to avoid choosing them
        ## in the ordering step multiple times
        dist_mat[, sel] <- NA

        i <- i + 1L
    }

    cl
}
