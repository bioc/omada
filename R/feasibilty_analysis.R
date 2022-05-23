#' Simulating dataset and calculate stabilities over different number of clusters
#'
#' @param classes The number of classes of samples to be reflected in the simulated dataset
#' @param samples The number of samples in the simulated dataset
#' @param features The number of features in the simulated dataset
#'
#' @return A list containing the average stabilities for all number of clusters(k),
#' the average (over all k) and maximum stabilities observed and the generated dataset
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
    colnames(dataset)[ncol(dataset)] <- paste0("feature_", feature.index)  # Rename column name
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

  for(rep in 2:(classes*2)) {

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

  res <-  list(stabilities = average.k.stabilities,
               maximum_stability = max(average.k.stabilities),
               average_stability = mean(average.k.stabilities),
               dataset = dataset)

  return(res)
}
