context("Feature Selection")

test_that("Dimensions of output", {
    expect_equal(
        length(featureSelection(toy_genes,
                                       min.k = 2,
                                       max.k = 4,
                                       step = 3)
               )
        , 4)
})

test_that("Type of output", {
    expect_equal(
        typeof(featureSelection(toy_genes,
                                    min.k = 2,
                                    max.k = 5,
                                    step = 3)
        )
        , "list")
})
