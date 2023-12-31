#' Partition Agreement calculation between two clustering runs
#'
#' Calculate the agreement (0,1) between two partitioning generated by two
#' clustering runs using the adjust Rand Index. We can use three clustering
#' algorithms (spectral, kmeans and hierarchical) along with the following
#' parameters for each:
#'
#' Spectral
#'  kernels: rbfdot, polydot, vanilladot, tanhdot, laplacedot, besseldot,
#'  anovadot, splinedot
#'
#'  K-means
#'  kernels: "Hartigan-Wong", Lloyd, Forgy, MacQueen
#'
#' Hierarchical
#'  Agglomeration methods: average, ward.D, ward.D2, single, complete, mcquitty,
#'  median, centroid
#'
#'  Distance measures: euclidean, manhattan, canberra, minkowski, maximum
#'
#' @param data A dataframe, where columns are features and rows are data points
#' @param measure.1 Concerns the first algorithm to be used and represents a
#' kernel for Spectral/kmeans or a distance measure for hierarchical clustering
#' @param hier.agglo.algorithm.1 Concerns the first algorithm to be used and
#' represents the agglomerative method for hierarchical clustering (not used in
#' spectral/kmeans clustering)
#' @param algorithm.2 First algorithm to be used (spectral/kmeans/hierarchical)
#' @param measure.2 Concerns the second algorithm to be used and represents a
#' kernel for Spectral/kmeans or a distance measure for hierarchical clustering
#' @param hier.agglo.algorithm.2 Concerns the second algorithm to be used and
#' represents the agglomerative method for hierarchical clustering (not used in
#' spectral/kmeans clustering)
#' @param number.of.clusters The upper limit of clusters to form starting from 2
#' @param algorithm.1 Second algorithm to be used (spectral/kmeans/hierarchical)
#'
#' @return An object of class "partitionAgreement" containing agreements
#' (Rand Indexes) from 1 cluster (ARI=0) up to the number of clusters requested
#'
#' @examples
#' partitionAgreement(toy_genes, algorithm.1 = "hierarchical",
#' measure.1 = "canberra",hier.agglo.algorithm.1 = "average",
#' algorithm.2 = "hierarchical",measure.2 = "manhattan",
#' hier.agglo.algorithm.2 = "average",number.of.clusters = 3)
#'
#' partitionAgreement(toy_genes, algorithm.1 = "spectral", measure.1 = "rbfdot",
#' algorithm.2 = "kmeans",measure.2 = "Lloyd", number.of.clusters = 5)
#' @export
#'
#' @importFrom stats cutree
#' @importFrom pdfCluster adj.rand.index

partitionAgreement <- function(data, algorithm.1 = "hierarchical",
                               measure.1 = "canberra",
                               hier.agglo.algorithm.1 = "average",
                               algorithm.2 = "hierarchical",
                               measure.2 = "manhattan",
                               hier.agglo.algorithm.2 = "average",
                               number.of.clusters = 5) {

  # Convert to matrix
  dataset <- data.matrix(data, rownames.force = NA)

  # cr1
  cr1 <- data.frame(matrix(ncol = 0, nrow = dim(data)[1]))
  for(i in 2:number.of.clusters) {
    #Spectral clustering
    if(algorithm.1 == "spectral") {
      cl <- kernlab::specc(dataset, centers=i, kernel = measure.1)
      temp <- data.frame(cl@.Data)
      cr1 <- cbind(cr1, temp)
    }

    #Hierarchical
    else if(algorithm.1 == "hierarchical") {
      dist_mat <- stats::dist(dataset, method = measure.1)
      cl <- stats::hclust(dist_mat, method = hier.agglo.algorithm.1)
      temp <- data.frame(cutree(cl, k = i))
      cr1 <- cbind(cr1, temp)
    }

    # #k-means
    else if(algorithm.1 == "kmeans") {
      cl <- stats::kmeans(dataset, i, algorithm = measure.1)
      temp <- data.frame(cl$cluster)
      cr1 <- cbind(cr1, temp)
    }

  }

  # cr2
  cr2 <- data.frame(matrix(ncol = 0, nrow = dim(data)[1]))
  for(i in 2:number.of.clusters) {
    #Spectral clustering
    if(algorithm.2 == "spectral") {
      cl <- kernlab::specc(dataset, centers=i, kernel = measure.2)
      temp <- data.frame(cl@.Data)
      cr2 <- cbind(cr2, temp)
    }

    #Hierarchical
    else if(algorithm.2 == "hierarchical") {
      dist_mat <- stats::dist(dataset, method = measure.2)
      cl <- stats::hclust(dist_mat, method = hier.agglo.algorithm.2)
      temp <- data.frame(cutree(cl, k = i))
      cr2 <- cbind(cr2, temp)
    }

    #k-means
    else if(algorithm.2 == "kmeans") {
      cl <- stats::kmeans(dataset, i, algorithm = measure.2)
      temp <- data.frame(cl$cluster)
      cr2 <- cbind(cr2, temp)
    }
  }

  Ks1 <- rep(1, dim(dataset)[1])

  colnames(cr1) <- paste0("Ks", 2:number.of.clusters)
  clusters.1 <- cbind(dataset, Ks1, cr1)

  colnames(cr2) <- paste0("Ks", 2:number.of.clusters)
  clusters.2 <- cbind(dataset, Ks1, cr2)

  # Convert to dataframe
  clusters.1 <- as.data.frame(clusters.1)
  clusters.2 <- as.data.frame(clusters.2)

  # Calculate Adjusted Rand Index vector
  ari.vector <- c(0)

  for (k in seq(2, number.of.clusters)) {
    k.name <- paste0("Ks", k)
    ari.vector <- append(ari.vector,
                         adj.rand.index(clusters.1[[k.name]],
                                        clusters.2[[k.name]]))
  }

  names(ari.vector) <- c(paste0("K_", seq(1, number.of.clusters)))

  partitionAgreement <- function(ari.scores=ari.vector){

    pa <- list(ari.scores = ari.scores)

    ## Set the name for the class
    class(pa) <- "partitionAgreement"

    return(pa)
  }

  partition.agreement <- partitionAgreement()

  return(partition.agreement)
}
