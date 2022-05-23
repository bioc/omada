context("Partition Agreement")

test_that("length of output", {
    expect_equal(
        length(partitionAgreement(toy_genes,
                                   algorithm.1 = "spectral",
                                   measure.1 = "rbfdot",
                                   hier.agglo.algorithm.1 = "average",
                                   algorithm.2 = "hierarchical",
                                   measure.2 = "manhattan",
                                   hier.agglo.algorithm.2 = "average",
                                   number.of.clusters = 5)
               )
        , 5)
})

measures.kmeans <- c("Hartigan-Wong", "Lloyd", "Forgy",
                     "MacQueen")

measures.kmeans.combinations <- combn(measures.kmeans, 2)

test_that("Number of clusters check", {

    for (rep in seq(2, 20))
    {
        expect_equal(
            length(partitionAgreement(toy_genes,
                                       algorithm.1 = "hierarchical",
                                       measure.1 = "euclidean",
                                       hier.agglo.algorithm.1 = "average",
                                       algorithm.2 = "hierarchical",
                                       measure.2 = "manhattan",
                                       hier.agglo.algorithm.2 = "average",
                                       number.of.clusters = rep)
            )
            , rep)
    }
})

test_that("Algorithm combinations", {

    for (rep in seq(1, dim(measures.kmeans.combinations)[2]))
    {
        expect_equal(
            length(partitionAgreement(toy_genes,
                                       algorithm.1 = "spectral",
                                       measure.1 =
                                          measures.kmeans.combinations[1,rep],
                                       hier.agglo.algorithm.1 = "average",
                                       algorithm.2 = "spectral",
                                       measure.2 =
                                          measures.kmeans.combinations[2,rep],
                                       hier.agglo.algorithm.2 = "average",
                                       number.of.clusters = 3)
            )
            , 3)
    }

    for (rep in seq(1, dim(measures.kmeans.combinations)[2]))
    {
        expect_equal(
            length(partitionAgreement(toy_genes,
                                       algorithm.1 = "kmeans",
                                       measure.1 =
                                          measures.kmeans.combinations[1,rep],
                                       hier.agglo.algorithm.1 = "average",
                                       algorithm.2 = "kmeans",
                                       measure.2 =
                                          measures.kmeans.combinations[2,rep],
                                       hier.agglo.algorithm.2 = "average",
                                       number.of.clusters = 2)
            )
            , 2)
    }
})
