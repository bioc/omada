#' A wrapper function that utilizes all tools to produce the optimal
#' sample memberships
#'
#' @param data A dataframe, where columns are features and rows are data points
#' @param method.upper.k The upper limit of clusters, k, to be considered.
#' Must be more than 2
#'
#' @return A list with all the tool results.
#'
#' @export
#'
#' @examples
#' omada(toy_genes, method.upper.k = 5)
#' omada(toy_genes, method.upper.k = 3)


omada <- function(data, method.upper.k = 5) {

    if(method.upper.k <= 2) {
        print("Increasing k to 3...")
        method.upper.k <- 3
    }

    # Running method selection with a default comparison number of 3
    methods.results <-
        clusteringMethodSelection(data, method.upper.k = method.upper.k,
                                  number.of.comparisons = 3)

    pa.df <- methods.results[[1]] # partition agreement values
    pa.plot <- methods.results[[2]] # partition agreement line plot

    optimal.method <- names(which.max(colMeans(pa.df[1:3])))

    # Running feature selection so that we consider 5 steps in total
    step <- dim(data)[2]/5
    feature.results <- featureSelection(data, min.k = 2,
                                        max.k = method.upper.k, step = step)

    fs.df <- feature.results[[1]] # stability values
    fs.plot <- feature.results[[2]] # stability line plot

    optimal.number.of.variant.features <-
        fs.df[which.max(fs.df$means),]["featureSet"][[1]]

    # Running the voting for k
    if (optimal.method == "spectral") {
        optimal.method.abbr <- "sc"
    } else if (optimal.method == "hierarchical") {
        optimal.method.abbr <- "hc"
    } else {
        optimal.method.abbr <- "km"
    }

    cluster.voting.results <- clusterVoting(data, 2, method.upper.k,
                                            optimal.method.abbr)
    cv.scores <- cluster.voting.results[[1]]
    cv.clusters <- cluster.voting.results[[2]]
    cv.votes <- cluster.voting.results[[3]]
    cv.ensemble <- cluster.voting.results[[4]]
    cv.plot <- cluster.voting.results[[5]]

    optimal.k <-
        as.numeric(substring(
            cv.ensemble[which.max(cv.ensemble$Frequency),][[1]], 2))

    optimal.clustering <- optimalClustering(data, optimal.k, optimal.method)
    memberships <- optimal.clustering[[1]]

    gene.signature.results <- geneSignatures(data, memberships)
    gs.matrix <- gene.signature.results[[1]]
    gs.plot <- gene.signature.results[[2]]

    return(list(pa.df, pa.plot, fs.df, fs.plot, cv.scores,
                cv.clusters, cv.votes, cv.ensemble, cv.plot, memberships,
                gs.matrix, gs.plot))
}
