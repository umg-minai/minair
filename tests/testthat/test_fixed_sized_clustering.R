test_that("fixed_sized_clustering_nearest_neighbor", {
    x <- c(1:3, 1e4, 11:13, 1.1e4, 111:113)
    d <- dist(x)
    expect_error(fixed_sized_clustering_nearest_neighbor(x), "dist")
    expect_error(
        fixed_sized_clustering_nearest_neighbor(d, size = 12), "lower than"
    )
    expect_equal(
        fixed_sized_clustering_nearest_neighbor(d, size = 3),
        c(rep(3L, 3), 1, rep(4L, 3), 1, rep(2L, 3))
    )
    expect_equal(
        fixed_sized_clustering_nearest_neighbor(d, size = 3, method = "min"),
        c(rep(3L, 3), 4, rep(1L, 3), 4, rep(2L, 3))
    )
    expect_equal(
        fixed_sized_clustering_nearest_neighbor(d, size = length(x)),
        rep(1, length(x))
    )
})

test_that(".cluster_size", {
    expect_error(.cluster_size(10, 20), "has to be smaller")
    expect_equal(.cluster_size(4, 2), c(2, 2))
    expect_equal(.cluster_size(4, 3), c(3, 1))
})
