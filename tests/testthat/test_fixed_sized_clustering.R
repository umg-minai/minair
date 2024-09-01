test_that("fixed_size_clustering_nearest_neighbor", {
    x <- c(1:3, 1e4, 11:13, 1.1e4, 111:113)
    d <- dist(x)
    expect_error(fixed_size_clustering_nearest_neighbor(x), "dist")
    expect_error(
        fixed_size_clustering_nearest_neighbor(d, size = 12), "lower than"
    )
    expect_equal(
        fixed_size_clustering_nearest_neighbor(d, size = 3),
        c(rep(3L, 3), 1, rep(4L, 3), 1, rep(2L, 3))
    )
    expect_equal(
        fixed_size_clustering_nearest_neighbor(d, size = 3, method = "min"),
        c(rep(3L, 3), 4, rep(1L, 3), 4, rep(2L, 3))
    )
    expect_equal(
        fixed_size_clustering_nearest_neighbor(d, size = length(x)),
        rep(1, length(x))
    )
})

test_that("fixed_size_clustering_kmeans", {
    x <- c(1:3, 1e4, 11:13, 1.1e4, 111:113)
    expect_error(
        fixed_size_clustering_kmeans(x, size = 12), "lower than"
    )
    expect_equal(
        withr::with_seed(
            123, fixed_size_clustering_kmeans(x, size = 3, "range")
        ),
        c(rep(1L, 3), 4, rep(2L, 3), 4, rep(3L, 3))
    )
    set.seed(123)
    expect_equal(
        withr::with_seed(
            123, fixed_size_clustering_kmeans(x, size = 3, "min")
        ),
        c(rep(1L, 3), 4, rep(2L, 3), 4, rep(3L, 3))
    )
    set.seed(123)
    expect_equal(
        withr::with_seed(
            123, fixed_size_clustering_kmeans(x, size = 3, "max")
        ),
        c(rep(1L, 3), 4, rep(2L, 3), 4, rep(3L, 3))
    )
    expect_equal(
        fixed_size_clustering_kmeans(x, size = length(x)),
        rep(1, length(x))
    )
})

test_that("fixed_size_clustering_hclust", {
    x <- c(1:3, 1e4, 11:13, 1.1e4, 111:113)
    d <- dist(x)
    expect_error(fixed_size_clustering_hclust(x), "dist")
    expect_error(
        fixed_size_clustering_hclust(d, size = 12), "lower than"
    )
    expect_equal(
        fixed_size_clustering_hclust(d, size = 3),
        c(rep(3L, 3), 1, rep(4L, 3), 1, rep(2L, 3))
    )
    expect_equal(
        fixed_size_clustering_hclust(d, size = length(x)),
        rep(1, length(x))
    )
})

test_that(".distance2center", {
    centers <- matrix(c(1, 2, 3, 1, 2, 3), nrow  = 2, byrow = TRUE)
    m <- matrix(1:3, nrow = 3, ncol = 2)
    expect_equal(
        .distance2center(m, centers),
        matrix(c(1, sqrt(2), 1), nrow = 3, ncol = 2)
    )
})

test_that(".benefit_order", {
    expect_error(.benefit_order(1:3), "matrix")
    expect_equal(
        .benefit_order(matrix(c(1, 5, 1, 2, 1, 4), byrow = TRUE, nrow = 3)),
        c(1, 3, 2)
    )
})

test_that(".cluster_size", {
    expect_error(.cluster_size(10, 20), "has to be smaller")
    expect_equal(.cluster_size(4, 2), c(2, 2))
    expect_equal(.cluster_size(4, 3), c(3, 1))
})
