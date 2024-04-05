#' A wrapper function that utilizes all tools to produce the optimal
#' sample memberships
#'
#' @param data A dataframe, where columns are features and rows are data points
#' @param method.upper.k The upper limit of clusters, k, to be considered.
#' Must be more than 2
#'
#' @return An object of class "clusterAnalysis" containing
#' partition.agreement.scores,partition.agreement.plot,feature.selection.scores,
#' feature.selection.plot, feature.selection.optimal.features,
#' feature.selection.optimal.number.of.features, cluster.voting.scores,
#' cluster.voting.cluster.memberships,cluster.voting.metric.votes,
#' cluster.voting.k.votes,cluster.voting.plot,sample.memberships
#'
#' @export
#'
#' @examples
#' omada(toy_genes, method.upper.k = 3)

omada <- function(data, method.upper.k = 5) {

    rnames <- row.names(data)

    if(method.upper.k <= 2) {
        method.upper.k <- 3
    }

    # Running method selection with a default comparison number of 3
    methods.results <-
        clusteringMethodSelection(data, method.upper.k = method.upper.k,
                                  number.of.comparisons = 3)

    pa.df <- get_partition_agreement_scores(methods.results)
    pa.plot <- plot_partition_agreement(methods.results)

    optimal.method <- names(which.max(colMeans(pa.df[seq_len(3)]))) # Method

    # Running feature selection so that we consider 5 steps in total
    step <- dim(data)[2]/5
    feature.results <- featureSelection(data, min.k = 2,
                                        max.k = method.upper.k, step = step)

    fs.df <- get_average_feature_k_stabilities(feature.results)
    fs.plot <- plot_average_stabilities(feature.results) # stability line plot

    # Selected  features
    optimal.features <- get_optimal_features(feature.results)
    optimal.number.of.features <-
        get_optimal_number_of_features(feature.results)

    # Selected dataset
    data <- data[,optimal.features]
    row.names(data) <- rnames

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

    cv.scores <- get_internal_metric_scores(cluster.voting.results)
    cv.clusters <- get_cluster_memberships_k(cluster.voting.results)
    cv.votes <- get_metric_votes_k(cluster.voting.results)
    cv.ensemble <- get_vote_frequencies_k(cluster.voting.results)
    cv.plot <- plot_vote_frequencies(cluster.voting.results)

    optimal.k <-
        as.numeric(substring(
            cv.ensemble[which.max(cv.ensemble$Frequency),][[1]], 2))

    optimal.clustering <- optimalClustering(data, optimal.k, optimal.method)

    # Generate memberships
    memberships <- get_optimal_memberships(optimal.clustering)
    optimal.stability.score <- get_optimal_stability_score(optimal.clustering)
    optimal.parameter.used <- get_optimal_parameter_used(optimal.clustering)

    clusterAnalysis <- function(partition.agreement.scores=pa.df,
                                partition.agreement.plot=pa.plot,
                                feature.selection.scores=fs.df,
                                feature.selection.plot=fs.plot,
                                feature.selection.optimal.features=
                                    optimal.features,
                                feature.selection.optimal.number.of.features=
                                    optimal.number.of.features,
                                cluster.voting.scores=cv.scores,
                                cluster.voting.memberships=cv.clusters,
                                cluster.voting.metric.votes=cv.votes,
                                cluster.voting.k.votes=cv.ensemble,
                                cluster.voting.plot=cv.plot,
                                sample.memberships=memberships){

        ca <- list(partition.agreement.scores = partition.agreement.scores,
                   partition.agreement.plot = partition.agreement.plot,
                   feature.selection.scores = feature.selection.scores,
                   feature.selection.plot = feature.selection.plot,
                   feature.selection.optimal.features=
                       feature.selection.optimal.features,
                   feature.selection.optimal.number.of.features=
                       feature.selection.optimal.number.of.features,
                   cluster.voting.scores = cluster.voting.scores,
                   cluster.voting.memberships = cluster.voting.memberships,
                   cluster.voting.metric.votes = cluster.voting.metric.votes,
                   cluster.voting.k.votes = cluster.voting.k.votes,
                   cluster.voting.plot = cluster.voting.plot,
                   sample.memberships = sample.memberships)

        ## Set the name for the class
        class(ca) <- "clusterAnalysis"

        return(ca)
    }

    cluster.analysis <- clusterAnalysis()

    return(cluster.analysis)
}
