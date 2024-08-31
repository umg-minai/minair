#' Calculate sum of squares
#'
#' @param x `matrix`
#' @return `double` sum of squares
#' @export
#' @rdname sum_of_squares
#' @examples
#' total_sum_of_squares(as.matrix(1:3))
total_sum_of_squares <- function(x)sum(scale(x, scale = FALSE) ^ 2L)

#' @param cl `integer`, vector with cluster ids, length has to be equal
#' `nrow(x))`.
#' @export
#' @rdname sum_of_squares
#' @examples
#' m <- matrix(1:4, nrow = 4)
#' within_cluster_sum_of_squares(m, c(1, 1, 1, 2))
within_cluster_sum_of_squares <- function(x, cl) {
    if (nrow(x) != length(cl))
        stop("`nrow(x)` has to be equal to length of `cl`")
    unlist(
        tapply(seq_len(nrow(x)), cl, function(i)
           total_sum_of_squares(x[i, , drop = FALSE]), simplify = FALSE)
    )
}
