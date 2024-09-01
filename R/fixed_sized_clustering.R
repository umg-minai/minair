#' Create clusters of fixed size
#'
#' Create clusters of fixed size using the nearest neighbor approach.
#'
#' @param d `dist`, dissimilarity structure as produced by `dist`.
#' @param size `integer`, fixed target size of the clusters.
#' @param method `character`, start with the closest (min)/farthest (max) point in average.
#' @return `integer`, vector of the same length as `attr(d, "Size")`
#'  with cluster identification numbers.
#' @references
#' Algorithm taken from Jean Monlong
#' \url{https://github.com/jmonlong/Hippocamplus/blob/master/content/post/2018-06-09-ClusterEqualSize.Rmd}
#' @importFrom utils head
#' @export
#' @rdname fixed_size_clustering
#' @examples
#' x <- c(1:3, 1e4, 11:13, 1.1e4, 111:113)
#' d <- dist(x)
#' fixed_size_clustering_nearest_neighbor(d, size = 3)
fixed_size_clustering_nearest_neighbor <-
    function(d, size, method = c("max", "min")) {
    if (!inherits(d, "dist"))
        stop("'d' has to be a dissimilarity 'dist' structure.")

    dist_mat <- as.matrix(d)
    nr <- nrow(dist_mat)

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

#' @param x `numeric`/`matrix`, points to be clustered.
#'
#' @details
#' `fixed_size_clustering_kmeans`:
#' This algorithm runs `[stats::kmeans()]` once to determine the center position
#' and reassigns the points to the nearest center with respect to the cluster
#' size. The `kmeans` could run multiple times to update every center after a
#' cluster was filled but we used the current approach for simplicity.
#'
#' `methods`: `min` (`max`), start with the closest (farthest) points. `range`
#' start with the points where the difference in farthest minus nearest distance
#' is largest.
#'
#' @references
#' kmeans Algorithm inspired from from Jean Monlong
#' \url{https://github.com/jmonlong/Hippocamplus/blob/master/content/post/2018-06-09-ClusterEqualSize.Rmd}
#' and \url{https://elki-project.github.io/tutorial/same-size_k_means}
#' @export
#' @rdname fixed_size_clustering
#' @importFrom stats kmeans
#' @examples
#' fixed_size_clustering_kmeans(d, size = 3)
fixed_size_clustering_kmeans <-
    function(x, size, method = c("range", "min", "max")) {
    m <- as.matrix(x)
    nr <- nrow(m)

    cl <- rep.int(NA_integer_, nr)

    sizes <- .cluster_size(nr, size)

    nsizes <- length(sizes)

    km <- kmeans(m, nsizes)
    centers <- km$centers
    ## reorder centers by their average dist to all points to put the farthest
    ## points in the smallest cluster
    centers <- centers[order(rowSums(centers)), , drop = FALSE]
    d <- .distance2center(m, centers)

    method <- match.arg(method)

    if (method == "range")
        o <- .benefit_order(d)
    else if (method == "min")
        o <- order(apply(d, 1, min))
    else
        o <- order(apply(d, 1, max), decreasing = TRUE)

    for (i in o) {
        best_cluster <- which.min(d[i, ])
        cl[i] <- best_cluster
        sizes[best_cluster] <- sizes[best_cluster] - 1L

        if (sizes[best_cluster] <= 0)
            d[, best_cluster] <- NA
    }

    cl
}

#' @param x `numeric`/`matrix`, points to be clustered.
#'
#' @details
#' `fixed_size_clustering_hclust`:
#' This algorithm runs `[stats::hclust()]` once to determine the clustering
#' and cuts the tree at different heigths to ensure the fixed cluster sizes.
#'
#' `methods`: `ward.D2` clustering method, see `[stats::hclust()]` for details.
#'
#' @importFrom stats hclust
#' @export
#' @rdname fixed_size_clustering
#' @examples
#' fixed_size_clustering_hclust(d, size = 3)
fixed_size_clustering_hclust <- function(d, size, method = "ward.D2") {
    if (!inherits(d, "dist"))
        stop("'d' has to be a dissimilarity 'dist' structure.")

    hc <- hclust(d, method = method)
    n <- length(hc$order)
    cl <- rep.int(NA_integer_, n)
    mask <- rep.int(0L, n)
    sizes <- .cluster_size(n, size)
    cur_cluster <- 1L
    ncluster <- length(sizes)

    for (k in seq(from = length(cl) - length(sizes), to = 1)) {
        groups <- cutree(hc, k = k)
        masked_groups <- groups + mask
        tbl <- table(masked_groups)
        imx <- which.max(tbl)
        if (tbl[imx] >= sizes[cur_cluster]) {
            p <- head(
                which(as.numeric(names(tbl)[imx]) == masked_groups),
                sizes[cur_cluster]
            )
            mask[p] <- NA_integer_
            cl[p] <- cur_cluster
            cur_cluster <- cur_cluster + 1

            if (cur_cluster == ncluster)
                break
        }
    }
    ## last cluster
    cl[is.na(cl)] <- ncluster

    cl
}

#' Calculate euclidean distance between points and centers
#'
#' @param m `matrix`, with points/data.
#' @param centers `matrix`, centers, output from `kmeans`
#' @return `matrix`, containing the distance to each cluster center in
#' of a point in a row, nrow == nrow(m), ncol == number of centers
#' @noRd
.distance2center <- function(m, centers) {
    do.call(cbind,
        lapply(seq_len(nrow(centers)), \(i) {
            d <- t(m) - centers[i, ]
            sqrt(apply(d, 2L, \(x)sum(x * x)))
        })
    )
}

#' Benefit is the largest difference for a point between closest/farthest center
#'
#' @param x `matrix`, nrow = number of points, ncol == number of centers, output
#' from .distance2center
#' @return `numeric`, length == number of points, order of largest benifit of
#' best over worst assignment
#'
#' @references
#' \url{https://elki-project.github.io/tutorial/same-size_k_means}
#' @noRd
.benefit_order <- function(x) {
    if (!is.matrix(x))
        stop("'x' has to be a matrix")
    r <- apply(x, 1L, range)
    ## should be max - min; but then we would need decreasing = TRUE
    order(r[1,] - r[2,])
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
