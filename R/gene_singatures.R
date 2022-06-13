#' Generating the feature/gene signature per cluster
#'
#' @param data A dataframe, where columns are features and rows are data points.
#'
#' @param memberships A dataframe with column "id" (same samples ids as above)
#' and column "membership" containing the cluster membership of each sample.
#' The memberships must be strings
#'
#' @return An object of class "geneSignature" containing a list of LASSO
#' (regression analysis) coefficients of each gene and a plot of the highest
#' 30% of coefficients per cluster.
#'
#' @export
#'
#' @examples
#' geneSignatures(toy_genes, toy_gene_memberships)
#'
#' @import ggplot2
#' @importFrom dplyr across filter %>% left_join


geneSignatures <- function(data, memberships) {

    # utils::globalVariables("where", add=FALSE)

    data <- as.data.frame(data)
    rnames <- row.names(data)
    data$id <- rnames

    # if(!("id" %in% colnames(data)))
    # {
    #     id <- paste0("s", 1:dim(data)[1])
    #     data <- cbind(id,data)
    # }

    # Composite data
    data <- left_join(memberships, data)
    data$id <- NULL
    row.names(data) <- rnames

    # Cluster names
    clusters <- unique(memberships$membership)

    print("Generating feature signatures...")

    # Running cross-validation Lasso to find optimal lambda value
    data.matrix <- as.matrix(data[,2:dim(data)[2]])
    cv_model <- glmnet::cv.glmnet(data.matrix(data.matrix), data$membership,
                                  family = "multinomial", alpha = 1)

    # Optimal lambda value (minimizing test MSE)
    optimal_lambda <- cv_model$lambda.min

    # Running optimal lasso model
    optimal_lasso <- glmnet::glmnet(data.matrix(data.matrix), data$membership,
                            family = "multinomial",
                            alpha = 1, lambda = optimal_lambda)

    # Extract coefficients for minimized test MSE)
    Coefficients <- stats::coef(optimal_lasso, s = "min")

    # Formatting coefficient dataframe per cluster
    ns <- names(Coefficients)
    ni <- 1
    coef.dataset <- data.frame(matrix(ncol= 0, nrow=dim(data)[2]-1))

    for(i in Coefficients) {
        temp <- as.data.frame(as.matrix(i)) %>% `colnames<-`(ns[ni])
        temp$Cluster <- ns[ni]
        temp <- temp[-1,]
        temp$Cluster <- NULL
        coef.dataset <- cbind(coef.dataset, temp)
        ni <- ni + 1
    }

    # Calculating mean coefficient per feature across clusters
    coef.dataset <- filter(coef.dataset,
                           rowSums(abs(across(where(is.numeric))))!=0)
    coef.dataset$means <- rowMeans(coef.dataset)
    coef.dataset <- coef.dataset[with(coef.dataset, order(abs(means),
                                                          decreasing = TRUE)),]
    coef.dataset$features <- rownames(coef.dataset)
    coef.dataset$means <- NULL #addition

    # retain top 30%
    coef.dataset <- coef.dataset[1:round(dim(coef.dataset)[1]*0.3, digits = 0),]
    coef.data.melt <- reshape::melt(coef.dataset)

    coef.30perc <- ggplot2::ggplot(data = coef.data.melt,
                                   aes(x = features, y = value,
                                                     fill = variable)) +
        geom_bar(stat = "identity") +
        theme(axis.title.x=element_blank(),
              axis.text.x = element_text(angle=45, vjust = 1, hjust = 1,
                                         size = 12),
              plot.title = element_text(hjust = 0.5),
              axis.title.y = element_text(size = 15),
              legend.position = "none") +
        geom_hline(yintercept=0, linetype="dashed", color = "red") +
        labs(title = "Coefficients") +
        facet_grid(variable~.)

    geneSignature <-
        function(coefficient.dataset = coef.dataset,
                 top30percent.coefficients = coef.30perc){

            gs <- list(coefficient.dataset = coefficient.dataset,
                       top30percent.coefficients = top30percent.coefficients)

            ## Set the name for the class
            class(gs) <- "geneSignature"

            return(gs)
        }

    gene.signature <- geneSignature()

    return(gene.signature)
}

# Getters
#' @export
get_coefficient_dataset <- function(object) {
    UseMethod("get_coefficient_dataset")
}
#' @export
get_coefficient_dataset.geneSignature <- function(object) {
    object$coefficient.dataset
}

#' @export
get_top30percent_coefficients <- function(object) {
    UseMethod("get_top30percent_coefficients")
}
#' @export
get_top30percent_coefficients.geneSignature <- function(object) {
    object$top30percent.coefficients
}
