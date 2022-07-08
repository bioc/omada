#' Simulating dataset based on existing dataset's dimensions, mean and standard
#' deviation
#'
#' @param data The dataset to base the simulation extracting the number of
#' samples, features and numeric
#' @param classes The number of classes of samples to be reflected in the
#' simulated dataset. Also determines the ks to be considered
#' (classes-2, classes+2)
#' @return An object of class "feasibilityAnalysis" containing the average
#' stabilities for all numbers of clusters(k), the average (over all k) and
#' maximum stabilities observed and the generated dataset
#'
#' @export
#'
#' @examples
#' feasibilityAnalysisDataBased(data = toy_genes, classes = 2)
#'
#' @importFrom fpc speccCBI
#' @importFrom stats sd

feasibilityAnalysisDataBased <- function(data, classes = 3) {
  samples <- dim(data)[1]
  features <- dim(data)[2]

  class <-
    rep(paste0("Class_", LETTERS[seq_len(classes)]), length.out = samples)
  dataset <- data.frame(class)

  class.sizes <- table(dataset$class)
  class.names <- names(class.sizes)

  data.mean <- mean(data)
  data.sd <- sd(data)

  class.means <-
    as.integer(seq(data.mean, classes * 10, length.out = classes))
  class.sd <-
    as.integer(seq(data.sd, classes * 2, length.out = classes))


  cl.index <- 1
  feature.index <- 1
  for (i in seq_len(features)) {
    temp <- stats::rnorm(n = samples,
                  mean = c(class.means),
                  sd = class.sd)
    dataset[, ncol(dataset) + 1] <- temp # Append temp column
    colnames(dataset)[ncol(dataset)] <-
      paste0("feature_", feature.index)
    feature.index <- feature.index + 1
  }
  cl.index <- cl.index + 1
  rownames(dataset) <- paste0("sample_",seq_len(samples))

  stability.dataset <- dataset
  stability.dataset$class <- NULL

  # Initiate stabilities for k for this feature set
  average.k.stabilities <- c()

  # Holds the boot stabilities for this feature set and every k
  boot.vector <- vector()

  if (classes > 3) {
    c.min <- classes - 2
    c.max <- classes + 2
  } else {
    c.min <- 2
    c.max <- 4
  }

  for (rep in c.min:c.max) {
    sc.boot <- fpc::clusterboot(
      stability.dataset,
      B = 25,
      bootmethod = "boot",
      clustermethod = speccCBI,
      k = rep,
      seed = 28588,
      showplots = FALSE,
      count = FALSE
    )
    temp <- list(sc.boot$bootmean)
    boot.vector <- c(boot.vector, temp)
  }

  # Average over cluster stabilities for each k
  for (i in seq_len(length(boot.vector))) {
    average.k.stabilities <- append(average.k.stabilities,
                                    mean(boot.vector[[i]]))
  }

  names(average.k.stabilities) <- c(paste0("k_", seq(c.min, c.max)))

  feasibilityAnalysis <-
    function(stabilities = average.k.stabilities,
             maximum_stability = max(average.k.stabilities),
             average_stability = mean(average.k.stabilities),
             ds = dataset) {

      fa <- list(
        avg.stabilities.per.k = stabilities,
        max_stability = maximum_stability,
        avg_stability = average_stability,
        generated.dataset = ds
      )

      ## Set the name for the class
      class(fa) <- "feasibilityAnalysis"

      return(fa)
    }
  feasibility.analysis <- feasibilityAnalysis()

  return(feasibility.analysis)
}
