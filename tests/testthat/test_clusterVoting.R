context("Cluster Voting")

test_that("Dimensions of output", {
    expect_equal(
        length(clusterVoting(toy_genes,
                             min.k = 2,
                             max.k = 7,
                             algorithm = "hc"))
        , 5)
    expect_equal(
        dim(clusterVoting(toy_genes,
                             min.k = 2,
                             max.k = 7,
                             algorithm = "hc")[[2]])[1]
        , 100)
})

test_that("Type of output", {
    expect_equal(
        typeof(clusterVoting(toy_genes,
                             min.k = 2,
                             max.k = 7,
                             algorithm = "hc"))
        , "list")
})