#' Predictor variable subsampling sets and bootstrapping stability set selection
#'
#' @param data A dataframe, where columns are features and rows are data points
#' @param min.k Minimum number of clusters for which we calculate stabilities
#' @param max.k Maximum number of clusters for which we calculate stabilities
#' @param step The number for additional features each feature set will contain
#'
#' @return An object of class "featureSelection" containing the dataframe of
#' average bootstrap stabilities, where rows represent feature sets and columns
#' number of clusters, the corresponding line plot, the number and the names of
#' the selected features
#'
#' @export
#'
#' @examples
#' featureSelection(toy_genes, min.k = 2, max.k = 4, step = 10)
#'
#' @importFrom fpc speccCBI
#' @import ggplot2


featureSelection <- function(data, min.k = 2, max.k = 4, step = 5) {

  print("Selecting feature subset...")

  # Initiating the list of feature sets to be compared
  featureset.list <- seq(step, dim(data)[2], by=step)

  # Averages
  averages.of.all.k <- list()

  # Sorted features based on variance across data points
  features.variance <- data.frame(apply(data, 2, stats::var))
  colnames(features.variance) <- "variance"
  sorted.features.variance <-
    features.variance[order(features.variance$variance,decreasing = TRUE), ,
                      drop = FALSE]
  sorted.features.variance$names <- rownames(sorted.features.variance)

  # Calculating stability for each feature set
  for(fs in featureset.list) {

    # Initiate stabilities for k for this feature set
    average.k.stabilities <- c()

    # Holds the boot stabilities for this feature set and every k
    boot.vector <- vector()

    for(rep in min.k:max.k) {

      cur <- sorted.features.variance$names[1:fs]

      sc.boot <- fpc::clusterboot(data[,cur],
                             B = 25,
                             bootmethod = "boot",
                             clustermethod = speccCBI,
                             k = rep,
                             seed = 28588,
                             showplots = FALSE,
                             count=FALSE
      )
      temp <- list(sc.boot$bootmean)
      boot.vector <- c(boot.vector, temp)
    }

    # Average over cluster stabilities for each k
    for (i in 1:length(boot.vector)) {
      average.k.stabilities <- append(
        average.k.stabilities, mean(boot.vector[[i]]))
    }

    averages.of.all.k[[length(averages.of.all.k)+1]] <-
      list(average.k.stabilities)
  }

  all.feature.k.stabilities <-
    as.data.frame(matrix(unlist(averages.of.all.k),
                         nrow=length(unlist(averages.of.all.k[1]))))
  colnames(all.feature.k.stabilities) <- featureset.list
  all.feature.k.stabilities <- t(all.feature.k.stabilities)
  colnames(all.feature.k.stabilities) <- paste0("k", min.k:max.k)
  all.feature.k.stabilities <- as.data.frame(all.feature.k.stabilities)

  # Calculating means over different runs for each k
  all.feature.k.stabilities$means <- rowMeans(all.feature.k.stabilities)
  all.feature.k.stabilities$featureSet <- rownames(all.feature.k.stabilities)

  all.feature.k.stabilities$featureSet <-
    as.integer(as.character(all.feature.k.stabilities$featureSet))

  stabilities.plot <- ggplot2::ggplot(data = all.feature.k.stabilities,
                             aes(x=featureSet, y=means)) +
    geom_line(color='firebrick',group = 1, size = 0.5) +
    geom_point(color='firebrick', group = 1) +
    ylab("Bootstrap stability") +
    xlab("Most variable sets") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12,face="bold"),
          axis.text.x=element_text(angle = 90,vjust = 0.5),
          plot.title = element_text(hjust = 0.5, size = 14,
                                    face="bold"),
          legend.position="none") +
    theme_bw()

  optimal.number <-
    all.feature.k.stabilities[
      which.max(all.feature.k.stabilities$means),]["featureSet"][[1]]

  optimal.feats <- sorted.features.variance$names[1:optimal.number]

  featureSelection <-
    function(average.feature.k.stabilities = all.feature.k.stabilities,
             average.stabilities.plot = stabilities.plot,
             optimal.number.of.features  = optimal.number,
             optimal.features = optimal.feats){

    fs <- list(average.feature.k.stabilities = average.feature.k.stabilities,
               average.stabilities.plot = average.stabilities.plot,
               optimal.number.of.features  = optimal.number.of.features,
               optimal.features = optimal.features)

    ## Set the name for the class
    class(fs) <- "featureSelection"

    return(fs)
  }

  feature.selection <- featureSelection()

  return(feature.selection)
}
