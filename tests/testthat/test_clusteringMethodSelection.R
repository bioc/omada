context("Clustering Method Selection")

test_that("Type of output", {
    expect_equal(
        typeof(clusteringMethodSelection(toy_genes,
                                         method.upper.k = 2,
                                         number.of.comparisons = 2))
        , "list")
})
