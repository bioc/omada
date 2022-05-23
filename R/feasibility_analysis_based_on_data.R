#' Simulating dataset based on existing dataset's dimensions, mean and standard
#' deviation
#'
#' @param data The dataset to base the simulation extracting the number of
#' samples, features and numeric
#' @param classes The number of classes of samples to be reflected in the
#' simulated dataset
#'
#' @return A list containing the average stabilities for all number of
#' clusters(k),
#' the average (over all k) and maximum stabilities observed and the generated
#' dataset
#'
#' @export
#'
#' @examples
#' feasibilityAnalysisDataBased(data = toy_genes, classes = 3)
#' feasibilityAnalysisDataBased(data = toy_genes, classes = 2)

feasibilityAnalysisDataBased <- function(data, classes = 3) {

  samples = dim(data)[1]
  features = dim(data)[2]

  class <- rep(paste0("Class_", LETTERS[1:classes]), length.out = samples)
  dataset <- data.frame(class)

  class.sizes <- table(dataset$class)
  class.names <- names(class.sizes)

  data.mean <- mean(data)
  data.sd <- sd(data)

  class.means <- as.integer(seq(data.mean,classes*10,length.out = classes))
  class.sd <- as.integer(seq(data.sd,classes*2,length.out = classes))


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
