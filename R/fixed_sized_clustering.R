#' Create clusters of fixed size
#'
#' Create clusters of fixes size using the nearest neighbor approach.
#'
#' @param d `dist`, dissimilarity structure as produced by `dist`.
#' @param size `integer`, fixed target size of the clusters.
#' @param method `character`, start with the closest (min)/farest (max) point in average.
#' @return `integer`, vector of the same length as `attr(d, "Size")`
#'  with cluster identification numbers.
#' @references
#' Algorithm taken from Jean Monlong
#' \url{https://github.com/jmonlong/Hippocamplus/blob/master/content/post/2018-06-09-ClusterEqualSize.Rmd}
#' @export
#' @examples
#' x <- c(1:3, 1e4, 11:13, 1.1e4, 111:113)
#' d <- dist(x)
#' fixed_sized_clustering(d, size = 3)
fixed_sized_clustering <- function(d, size, method = c("max", "min")) {
    if (!inherits(d, "dist"))
        stop("'d' has to be a dissimilarity 'dist' structure.")

    dist_mat <- as.matrix(d)
    nr <- nrow(dist_mat)

    if (size[1L] > nr)
        stop("Size of clusters has to be lower than size of 'dist'.")

    method <- match.arg(method)

    cl <- rep.int(NA_integer_, nr)
    ncl <- ceiling(nr / size[1L])
    sizes <- rep.int(size[1L], ncl)

    remainder <- nr %% size[1L]

    if (as.logical(remainder))
        sizes[1L] <- remainder

    already_selected <- -Inf

    if (method == "min") {
        already_selected <- -already_selected
        sizes <- rev(sizes)
    }

    cur_cluster <- 1L

    while (anyNA(cl)) {
        rs <- rowSums(dist_mat, na.rm = TRUE)

        ## start with cluster closest/farest away in average
        if (method == "max")
            i <- which.max(rs)
        else
            i <- which.min(rs)

        ## find nearest neighbors
        sel <- head(order(dist_mat[i,]), sizes[cur_cluster])

        cl[sel] <- cur_cluster

        ## generate low/high rowsums for already selected rows
        dist_mat[sel,] <- already_selected
        ## disable already selected values (in columns) to avoid choosing them
        ## in the ordering step multiple times
        dist_mat[, sel] <- NA

        cur_cluster <- cur_cluster + 1L
    }

    cl
}
