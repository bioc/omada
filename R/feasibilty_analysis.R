#' Simulating dataset and calculate stabilities over different number of
#' clusters
#'
#' @param classes The number of classes of samples to be reflected in the
#' simulated dataset. Also determines the ks to be considered
#' (classes-2, classes+2)
#' @param samples The number of samples in the simulated dataset
#' @param features The number of features in the simulated dataset
#'
#' @return An object of class "feasibilityAnalysis" containing the average
#' stabilities for all number of clusters(k), the average (over all k) and
#' maximum stabilities observed and the generated dataset
#'
#' @export
#'
#' @examples
#' feasibilityAnalysis(classes = 3, samples = 320, features = 400)
#' feasibilityAnalysis(classes = 4, samples = 400, features = 120)

feasibilityAnalysis <- function(classes = 3, samples = 320, features = 400) {

  class <- rep(paste0("Class_", LETTERS[1:classes]), length.out = samples)
  dataset <- data.frame(class)
  class.sizes <- table(dataset$class)
  class.names <- names(class.sizes)
  class.means <- as.integer(seq(5,classes*10,length.out = classes))
  class.sd <- as.integer(seq(1,classes*2,length.out = classes))

  cl.index <- 1
  feature.index <- 1
  for(i in 1:features) {
    temp <- rnorm(n = samples, mean = c(class.means), sd = class.sd)
    dataset[ , ncol(dataset) + 1] <- temp # Append temp column
    colnames(dataset)[ncol(dataset)] <- paste0("feature_", feature.index)
    feature.index <- feature.index + 1
  }

  cl.index <- cl.index + 1
  rownames(dataset) <- paste0("sample_", 1:samples)
  stability.dataset <- dataset
  stability.dataset$class <- NULL

  # Initiate stabilities for k for this feature set
  average.k.stabilities <- c()

  # Holds the boot stabilities for this feature set and every k
  boot.vector <- vector()

  if(classes > 3) {
    c.min <- classes-2
    c.max <- classes+2
  } else {
    c.min <- 2
    c.max <- 4
  }

  for(rep in c.min:c.max) {

    sc.boot <- clusterboot(stability.dataset,
                           B = 25,
                           bootmethod = "boot",
                           clustermethod = speccCBI,
                           k = rep,
                           seed= 28588,
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

  names(average.k.stabilities) <- c(paste0("k_",seq(c.min,c.max)))

  feasibilityAnalysis <- function(stabilities = average.k.stabilities,
                                   maximum_stability = max(average.k.stabilities),
                                   average_stability = mean(average.k.stabilities),
                                   ds = dataset){

    fa <- list(avg_stabilities_per_k = stabilities,
               max_stability = maximum_stability,
               avg_stability = average_stability,
               generated.dataset = ds)

    ## Set the name for the class
    class(fa) <- "feasibilityAnalysis"

    return(fa)
  }

  feasibility.analysis <- feasibilityAnalysis()

  return(feasibility.analysis)
}

# Getters
#' @export
get_average_stabilities_per_k <- function(object) {
  UseMethod("get_average_stabilities_per_k")
}

#' @export
get_average_stabilities_per_k.feasibilityAnalysis <- function(object) {
  object$avg_stabilities_per_k
}

#' @export
get_average_stability <- function(object) {
  UseMethod("get_average_stability")
}

#' @export
get_average_stability.feasibilityAnalysis <- function(object) {
  object$avg_stability
}

#' @export
get_max_stability <- function(object) {
  UseMethod("get_max_stability")
}

#' @export
get_max_stability.feasibilityAnalysis <- function(object) {
  object$max_stability
}

#' @export
get_generated_dataset <- function(object) {
  UseMethod("get_generated_dataset")
}

#' @export
get_generated_dataset.feasibilityAnalysis <- function(object) {
  object$generated.dataset
}
