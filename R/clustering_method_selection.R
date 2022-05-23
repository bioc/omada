#' Method Selection through intra-method Consensus Partition Consistency
#'
#' @param data A dataframe, where columns are features and rows are data points
#' @param method.upper.k The number of clusters, k, up to which the average
#' agreements will be calculated
#' @param number.of.comparisons The number of comparisons to average over per k
#'
#' @return A list containing a dataframe of partition agreement scores for a
#' set of random parameter
#' clustering runs across different methods and the corresponding plot
#'
#' @export
#'
#' @examples
#' clusteringMethodSelection(toy_genes, method.upper.k = 3,
#' number.of.comparisons = 4)
#' clusteringMethodSelection(toy_genes, method.upper.k = 2,
#' number.of.comparisons = 2)

clusteringMethodSelection <- function(data, method.upper.k = 5,
                                      number.of.comparisons = 3) {

  print("Comparing methods...")

  number.of.pairs <- number.of.comparisons*2

  hierarchical.algs <- c("average", "ward.D", "ward.D2", "single",
                         "complete", "mcquitty", "median", "centroid")

  hierarchical.measures <- c("euclidean", "manhattan", "canberra",
                             "minkowski", "maximum")

  kmeans.kernels <- c("Hartigan-Wong", "Lloyd", "Forgy",
                      "MacQueen")

  spectral.kernels <- c("rbfdot", "polydot", "vanilladot", "tanhdot",
                        "laplacedot", "anovadot", "splinedot")

  hier.cobs <- expand.grid(hierarchical.measures, hierarchical.algs)
  hier.cobs <- hier.cobs[sample(nrow(hier.cobs), number.of.pairs),]

  spectral.cobs <- expand.grid(spectral.kernels, spectral.kernels)
  spectral.cobs <- spectral.cobs[spectral.cobs$Var1 !=spectral.cobs$Var2 ,]
  spectral.cobs <- spectral.cobs[sample(nrow(spectral.cobs),number.of.pairs),]

  kmeans.cobs <- expand.grid(kmeans.kernels, kmeans.kernels)
  kmeans.cobs <- kmeans.cobs[kmeans.cobs$Var1 !=kmeans.cobs$Var2 ,]
  kmeans.cobs <- kmeans.cobs[sample(nrow(kmeans.cobs),number.of.pairs),]

  df.h <- data.frame(matrix(
    ncol=method.upper.k, nrow=0,
    dimnames=list(NULL, paste0("p",seq(1, method.upper.k)))))

  df.s <- data.frame(matrix(
    ncol=method.upper.k, nrow=0,
    dimnames=list(NULL, paste0("p",seq(1, method.upper.k)))))

  df.k <- data.frame(matrix(
    ncol=method.upper.k, nrow=0,
    dimnames=list(NULL, paste0("p",seq(1, method.upper.k)))))

  for(i in seq(1, number.of.pairs, by = 2)) {

    p1h <- hier.cobs[i,]
    p2h <- hier.cobs[i+1,]

    p1h.1 <- as.character(p1h[[1]])
    p1h.2 <- as.character(p1h[[2]])
    p2h.1 <- as.character(p2h[[1]])
    p2h.2 <- as.character(p2h[[2]])

    cur.h <- partitionAgreement(data, algorithm.1 = "hierarchical",
                                measure.1 = p1h.1,
                                hier.agglo.algorithm.1 = p1h.2,
                                algorithm.2 = "hierarchical",
                                measure.2 = p2h.1,
                                hier.agglo.algorithm.2 = p2h.2,
                                method.upper.k)

    df.h[nrow(df.h) + 1, ] <- cur.h
  }

  for(i in seq(1, number.of.comparisons)) {

    p1s <- spectral.cobs[i,]
    cur.s <- partitionAgreement(data, algorithm.1 = "spectral",
                                measure.1 = as.character(p1s[[1]]),
                                algorithm.2 = "spectral",
                                measure.2 = as.character(p1s[[2]]),
                                method.upper.k)

    df.s[nrow(df.s) + 1, ] <- cur.s
  }

  for(i in seq(1, number.of.comparisons)) {

    p1k <- kmeans.cobs[i,]
    cur.k <- partitionAgreement(data, algorithm.1 = "kmeans",
                                measure.1 = as.character(p1k[[1]]),
                                algorithm.2 = "kmeans",
                                measure.2 = as.character(p1k[[2]]),
                                method.upper.k)

    df.k[nrow(df.k) + 1, ] <- cur.k
  }


  df.final <- data.frame(hierarchical = colMeans(df.h),
                         spectral = colMeans(df.s),
                         kmeans = colMeans(df.k),
                         clusters = seq(1, method.upper.k))

  h.mean <- mean(df.final$hierarchical)
  s.mean <- mean(df.final$spectral)
  k.mean <- mean(df.final$kmeans)

  df.plot <- melt(df.final, id=c("clusters"))
  colnames(df.plot) <- c("clusters", "methods", "value")

  agreements.plot <- ggplot(df.plot, aes(x = clusters, y = value)) +
    geom_line(aes(color = methods)) +
    geom_hline(aes(yintercept=h.mean), linetype="dashed") +
    geom_hline(aes(yintercept=s.mean), linetype="dashed") +
    geom_hline(aes(yintercept=k.mean), linetype="dashed") +
    geom_text(aes(0,h.mean,label = paste0("h.mean: ", format(round(h.mean, 2),
                                                             nsmall = 2)),
                  vjust = 1, hjust = -0.25)) +
    geom_text(aes(0,s.mean,label = paste0("s.mean: ", format(round(s.mean, 2),
                                                             nsmall = 2)),
                  vjust = 1, hjust = -0.25)) +
    geom_text(aes(0,k.mean,label = paste0("k.mean: ", format(round(k.mean, 2),
                                                             nsmall = 2)),
                  vjust = 1, hjust = -0.25)) +
    ylab("Partition Agreement (ARI)")

  return(list(df.final,agreements.plot))
}
