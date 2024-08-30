#' Create clusters of fixed size
#'
#' Create clusters of fixes size using the nearest neighbor approach.
#'
#' @param d `dist`, dissimilarity structure as produced by `dist`.
#' @param size `integer`, fixed target size of the clusters.
#' @param method `character`, start with the closest (min)/farthest (max) point in average.
#' @return `integer`, vector of the same length as `attr(d, "Size")`
#'  with cluster identification numbers.
#' @references
#' Algorithm taken from Jean Monlong
#' \url{https://github.com/jmonlong/Hippocamplus/blob/master/content/post/2018-06-09-ClusterEqualSize.Rmd}
#' @export
#' @examples
#' x <- c(1:3, 1e4, 11:13, 1.1e4, 111:113)
#' d <- dist(x)
#' fixed_sized_clustering_nearest_neighbor(d, size = 3)
fixed_sized_clustering_nearest_neighbor <-
    function(d, size, method = c("max", "min")) {
    if (!inherits(d, "dist"))
        stop("'d' has to be a dissimilarity 'dist' structure.")

    dist_mat <- as.matrix(d)
    nr <- nrow(dist_mat)

    if (size[1L] > nr)
        stop("Size of clusters has to be lower than size of 'dist'.")

    method <- match.arg(method)

    cl <- rep.int(NA_integer_, nr)

    sizes <- .cluster_size(nr, size)

    already_selected <- -Inf

    if (method == "min") {
        already_selected <- -already_selected
    } else {
        ## start with the cluster with the lowest size for the farthest points
        sizes <- rev(sizes)
    }

    cur_cluster <- 1L

    while (anyNA(cl)) {
        rs <- rowSums(dist_mat, na.rm = TRUE)

        ## start with cluster closest/farthest away in average
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

#' Create vector of equal cluster sizes
#'
#' Create vector of equal cluster sizes except the last one, which take the
#' remainder.
#'
#' @param n `integer(1)`, number of elements to be clustered
#' @param size `integer(1)`, target size of each cluster.
#' @return `integer(k)`, where `k` is the number of clusters and each element in
#' the vector store the size of the cluster.
#' @noRd
.cluster_size <- function(n, size) {
    if (size > n)
        stop("cluster size 'size' has to be smaller ",
             "or equal than number of elements")

    k <- ceiling(n / size[1L])
    sizes <- rep.int(size[1L], k)

    remainder <- n %% size[1L]

    if (as.logical(remainder))
        sizes[length(sizes)] <- remainder

    sizes
}
