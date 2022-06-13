#' Estimating number of clusters through internal exhaustive ensemble majority
#' voting
#'
#' @param data A dataframe, where columns are features and rows are data points
#' @param min.k Minimum number of clusters for which we calculate stabilities
#' @param max.k Maximum number of clusters for which we calculate stabilities
#' @param algorithm The clustering algorithm to use for the multiple clustering
#' runs to be measured
#'
#' @return An object of class "clusterVoting" containing a matrix with metric
#'  scores for every k and internal index, cluster memberships for every k, a
#'  dataframe with the k votes for every index, k vote frequencies and the
#'  frequency barplot of the k votes
#'
#' @export
#'
#' @examples
#' clusterVoting(toy_genes, 4,14,"sc")
#' clusterVoting(toy_genes, 2,7,"hc")
#' clusterVoting(toy_genes, 2,4,"km")
#'
#' @importFrom diceR prepare_data
#' @import ggplot2

clusterVoting <- function(data ,min.k ,max.k, algorithm) {

  print("Deciding on the number of clusters...")

  data <- as.matrix(data)
  data <- prepare_data(data)

  # Metrics' sizes
  k_array <- sprintf("k%s",min.k:max.k)
  k_length <- length(k_array)

  # Initializing metric scores table
  scores <- matrix(, nrow = 15, ncol = k_length)
  colnames(scores) <- k_array
  rownames(scores) <- c("calinski_harabasz","dunn","pbm","tau","gamma",
                        "c_index","davies_bouldin","mcclain_rao","sd_dis",
                        "ray_turi","g_plus","silhouette","s_dbw",
                        "Compactness","Connectivity")

  clusters <- matrix(, nrow = dim(data)[1], ncol = k_length)
  colnames(clusters) <- k_array
  rownames(clusters) <- rownames(data)

  counter <- 1
  for(current_k in min.k:max.k) {

    if(algorithm == "sc") {
      cl <- kernlab::specc(data, centers=current_k, kernel = "rbfdot")
      cls <- cl@.Data
    } else if(algorithm == "hc") {
      dist_mat <- stats::dist(data, method = "euclidean")
      cl <- stats::hclust(dist_mat, method = "average")
      cls <- stats::cutree(cl, k = current_k)
    } else if(algorithm == "km") {
      cl <- stats::kmeans(data, current_k, algorithm = "Hartigan-Wong")
      cls <- cl$cluster
    }

    criteria <- clusterCrit::intCriteria(data,cls,c("calinski_harabasz","dunn",
                                                    "pbm","tau", "gamma",
                                                    "c_index","davies_bouldin",
                                       "mcclain_rao","sd_dis", "ray_turi",
                                       "g_plus","silhouette","s_dbw"))

    con <- clValid::connectivity(clusters = cls, Data = data)
    comp <- diceR::compactness(data, cls)
    criteria <- c(criteria, connectivity=con, compactness=comp)
    criteria <- array(as.numeric(unlist(criteria)))
    scores[, counter] <- criteria
    clusters[, counter] <- cls
    counter <- counter + 1
  }

  scores[is.na(scores)] = 0

  votes <- data.frame(result=integer(), metric=character(),
                      stringsAsFactors=FALSE)
  b.counter <- 1

  # Determine which k has the best score
  for(metric in 1:nrow(scores)) {

    if(metric %in% c(1,2,3,4,5,12,14,15)) {
      metric.res <- which(scores[metric,]==max(scores[metric,]))
    } else {
      metric.res <- which(scores[metric,]==min(scores[metric,]))
    }

    current.metric <- row.names(scores)[metric]

    for(res in metric.res) {
      votes[b.counter, ] <- c(res, current.metric)
      b.counter <- b.counter + 1
    }
  }

  # Remove metric votes that voted for every k
  votes <- votes[votes$metric %in%
                   names(which(table(votes$metric) != dim(scores)[2])), ]

  # converting into familiar kX form
  votes[, 1] <- paste0("k", (as.numeric(votes[, 1]) + 1))

  ensemble.results <- as.data.frame(table(votes[,1]))
  colnames(ensemble.results) <- c("k", "Frequency")
  ensemble.results$Frequency <- as.numeric(ensemble.results$Frequency)

  ensemble.plot <- ggplot2::ggplot(ensemble.results, aes(k, Frequency,
                                                         fill = k)) +
    geom_col() +
    scale_fill_brewer(palette="Dark2")



  clusterVoting <- function(internal.metric.scores = scores,
                            cluster.memberships.k = clusters,
                               metric.votes.k = votes,
                            vote.frequencies.k = ensemble.results,
                            vote.frequencies.plot = ensemble.plot){

    cv <- list(internal.metric.scores = internal.metric.scores,
               cluster.memberships.k = cluster.memberships.k,
               metric.votes.k = metric.votes.k,
               vote.frequencies.k = vote.frequencies.k,
               vote.frequencies.plot = vote.frequencies.plot)

    ## Set the name for the class
    class(cv) <- "clusterVoting"

    return(cv)
  }

  cluster.voting <- clusterVoting()

  return(cluster.voting)
}

# Getters
#' @export
get_internal_metric_scores <- function(object) {
  UseMethod("get_internal_metric_scores")
}
#' @export
get_internal_metric_scores.clusterVoting <- function(object) {
  object$internal.metric.scores
}

#' @export
get_cluster_memberships_k <- function(object) {
  UseMethod("get_cluster_memberships_k")
}
#' @export
get_cluster_memberships_k.clusterVoting <- function(object) {
  object$cluster.memberships.k
}

#' @export
get_metric_votes_k <- function(object) {
  UseMethod("get_metric_votes_k")
}
#' @export
get_metric_votes_k.clusterVoting <- function(object) {
  object$metric.votes.k
}

#' @export
get_vote_frequencies_k <- function(object) {
  UseMethod("get_vote_frequencies_k")
}
#' @export
get_vote_frequencies_k.clusterVoting <- function(object) {
  object$vote.frequencies.k
}

#' @export
plot_vote_frequencies <- function(object) {
  UseMethod("plot_vote_frequencies")
}
#' @export
plot_vote_frequencies.clusterVoting <- function(object) {
  object$vote.frequencies.plot
}
