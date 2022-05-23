context("Clustering Method Selection")

test_that("Dimensions of output", {
    expect_equal(
        length(clusteringMethodSelection(toy_genes,
                                         method.upper.k = 3,
                                         number.of.comparisons = 3))
        , 2)
    expect_equal(
        dim(clusteringMethodSelection(toy_genes,
                                      method.upper.k = 5,
                                      number.of.comparisons = 2)[[1]])[1]
        , 5)
})

test_that("Type of output", {
    expect_equal(
        typeof(clusteringMethodSelection(toy_genes,
                                         method.upper.k = 2,
                                         number.of.comparisons = 2))
        , "list")
})
