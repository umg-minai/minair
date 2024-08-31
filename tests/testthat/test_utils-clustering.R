test_that("total_sum_of_squares", {
    expect_equal(total_sum_of_squares(as.matrix(1:3)), 2)
    expect_equal(total_sum_of_squares(as.matrix(c(2, 4, 6))), 8)
})

test_that("within_cluster_sum_of_squares", {
    expect_error(within_cluster_sum_of_squares(as.matrix(1:3), 1), "length")
    expect_equal(
        within_cluster_sum_of_squares(as.matrix(1:3), 1:3), 
        c("1" = 0, "2" = 0, "3" = 0)
    )
    expect_equal(
        within_cluster_sum_of_squares(as.matrix(1:4), c(rep(1, 3), 2)),
        c("1" = 2, "2" = 0)
    )
})
