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
#'
#' @importFrom diceR prepare_data
#' @import ggplot2

clusterVoting <- function(data ,min.k ,max.k, algorithm) {
    
    data <- as.matrix(data)
    # data <- prepare_data(data)
    
    # Metrics' sizes
    k_array <- sprintf("k%s",min.k:max.k)
    k_length <- length(k_array)
    
    # Initializing metric scores table
    scores <- matrix(, nrow = 12, ncol = k_length)
    colnames(scores) <- k_array
    rownames(scores) <- c("calinski_harabasz","dunn","silhouette","davies_bouldin","tau",
      "gamma","g_plus", "c_index", "s_dbw","mcclain_rao","Connectivity",
      "Compactness")
    
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

        # New code
        ch_score <- calinhara(data,cls,cn=2) # fpc: calinski_harabasz
        #dunn_score <- dunn(clusters = 2, Data = data, method = "euclidean") # clValid: dunn
        dunn_score <- generalised_dunn_index(data, cls, 1, 1) # clValid: dunn
        silhouette_score <- silhouette_index(data, cls) # genieclust: silhouette
        davies_bouldin <- -negated_davies_bouldin_index(data, cls) # genieclust: davies_bouldin
        
        tgg <- Index.sPlussMoins(cls, data) # native: tau, gamma, g_plus
        tau <- tgg$tau
        gamma <- tgg$gamma
        g_plus <- tgg$gplus
        c_index <- Indice.cindex(data,cls) # native: c_index
        s_dbw <- Index.SDbw(data, cls)
        mcclain_rao <- Index.15and28(cls,cls,data)$mcclain#mcclain_rao
        con <- clValid::connectivity(clusters = cls, Data = data)
        comp <- diceR::compactness(data, cls)
        criteria <- c(ch_score=ch_score,dunn_score=dunn_score,silhouette_score,
                      davies_bouldin=davies_bouldin,tau=tau,gamma=gamma,g_plus=g_plus, 
                      c_index=c_index,s_dbw=s_dbw,mcclain_rao=mcclain_rao,
                      connectivity=con, compactness=comp)
        
        criteria <- array(as.numeric(unlist(criteria)))
        
        scores[, counter] <- criteria
        clusters[, counter] <- cls
        counter <- counter + 1
    }
    
    scores[is.na(scores)] <- 0
    
    votes <- data.frame(result=integer(), metric=character(),
                        stringsAsFactors=FALSE)
    b.counter <- 1
    
    # Determine which k has the best score
    for(metric in seq_len(nrow(scores))) {
        
        if(metric %in% c(1,2,3,5,6,10,11)) {
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


Indice.cindex <- function (d, cl) 
{
    d <- as.matrix(dist(d, method = 'manhattan'))
    d <- data.matrix(d)
    DU <- 0
    r <- 0
    v_max <- array(1, max(cl))
    v_min <- array(1, max(cl))
    for (i in 1:max(cl)) {
        n <- sum(cl == i)
        if (n > 1) {
            t <- d[cl == i, cl == i]
            DU = DU + sum(t)/2
            v_max[i] = max(t)
            if (sum(t == 0) == n) 
                v_min[i] <- min(t[t != 0])
            else v_min[i] <- 0
            r <- r + n * (n - 1)/2
        }
    }
    Dmin = min(v_min)
    Dmax = max(v_max)
    if (Dmin == Dmax) 
        result <- NA
    else result <- (DU - r * Dmin)/(Dmax * r - Dmin * r)
    result
}


Index.sPlussMoins <- function (cl1,md)
{
    md <- as.matrix(dist(md, method = 'euclidean'))
    cn1 <- max(cl1)
    n1 <- length(cl1)
    dmat <- as.matrix(md)
    average.distance <- median.distance <- separation <- cluster.size <- within.dist1 <- between.dist1 <- numeric(0)
    separation.matrix <- matrix(0, ncol = cn1, nrow = cn1)
    di <- list()
    for (u in 1:cn1) {
        cluster.size[u] <- sum(cl1 == u)
        du <- as.dist(dmat[cl1 == u, cl1 == u])
        within.dist1 <- c(within.dist1, du)
        average.distance[u] <- mean(du)
        median.distance[u] <- median(du)
        bv <- numeric(0)
        for (v in 1:cn1) {
            if (v != u) {
                suv <- dmat[cl1 == u, cl1 == v]
                bv <- c(bv, suv)
                if (u < v) {
                    separation.matrix[u, v] <- separation.matrix[v,u] <- min(suv)
                    between.dist1 <- c(between.dist1, suv)
                }
            }
        }
    }
    
    nwithin1 <- length(within.dist1)
    nbetween1 <- length(between.dist1)
    meanwithin1 <- mean(within.dist1)
    meanbetween1 <- mean(between.dist1)
    
    s.plus <- s.moins <- 0 
    #s.moins<-sum(rank(c(within.dist1,between.dist1),ties="first")[1:nwithin1]-rank(within.dist1,ties="first"))
    #s.plus  <-sum(rank(c(-within.dist1,-between.dist1),ties="first")[1:nwithin1]-rank(-within.dist1,ties="first"))
    for (k in 1: nwithin1)
    {
        s.plus <- s.plus+(colSums(outer(between.dist1,within.dist1[k], ">")))
        s.moins <- s.moins+(colSums(outer(between.dist1,within.dist1[k], "<")))
    }    
    
    Index.Gamma <- (s.plus-s.moins)/(s.plus+s.moins)
    Index.Gplus <- (2*s.moins)/(n1*(n1-1))
    t.tau  <- (nwithin1*nbetween1)-(s.plus+s.moins)
    Index.Tau <- (s.plus-s.moins)/(((n1*(n1-1)/2-t.tau)*(n1*(n1-1)/2))^(1/2))
    
    results <- list(gamma=Index.Gamma, gplus=Index.Gplus, tau=Index.Tau)
    return(results)
}

Index.SDbw<-function(x, cl)
{
    x <- as.matrix(x)
    Scatt<-Average.scattering(cl,x)$scatt
    Dens.bw<-density.bw(cl,x)
    SDbw<-Scatt+Dens.bw
    return(SDbw)
} 

density.clusters<-function(cl, x)
{
    x <- as.matrix(x)
    k <- max(cl)
    n <- length(cl)
    
    distance <- matrix(0, ncol = 1, nrow = n)
    density <-  matrix(0, ncol = 1, nrow = k)
    centers.matrix<-centers(cl,x)
    stdev<-Average.scattering(cl,x)$stdev 
    for(i in 1:n) 
    {        
        u=1
        while(cl[i] != u )
            u<-u+1
        for (j in 1:ncol(x))   
        {               
            distance[i]<- distance[i]+(x[i,j]-centers.matrix[u,j])^2 
        }     
        distance[i]<-sqrt(distance[i])            
        if (distance[i] <= stdev)
            density[u]= density[u]+1                      
    }  
    dens<-list(distance=distance, density=density)    
    return(dens)          
    
}

density.bw<-function(cl, x)
{
    x <- as.matrix(x)
    k <- max(cl)
    n <- length(cl)   
    centers.matrix<-centers(cl,x)
    stdev<-Average.scattering(cl,x)$stdev 
    density.bw<- matrix(0, ncol = k, nrow = k)
    u<-1
    
    for(u in 1:k)
    {
        for(v in 1:k)
        {
            if(v!=u)
            {  
                distance<- matrix(0, ncol = 1, nrow = n)
                moy<-(centers.matrix[u,]+centers.matrix[v,])/2
                for(i in 1:n)
                {
                    if((cl[i]==u)||(cl[i]==v))
                    {
                        for (j in 1:ncol(x))   
                        {               
                            distance[i]<- distance[i]+(x[i,j]-moy[j])^2 
                        }   
                        distance[i]<- sqrt(distance[i])
                        if(distance[i]<= stdev)
                        {
                            density.bw[u,v]<-density.bw[u,v]+1                  
                        }  
                    }           
                }
            }       
        }
    }
    density.clust<-density.clusters(cl,x)$density 
    S<-0
    for(u in 1:k)
        for(v in 1:k)
        {  
            if(max(density.clust[u], density.clust[v])!=0)
                S=S+ (density.bw[u,v]/max(density.clust[u], density.clust[v]))
        }   
    density.bw<-S/(k*(k-1))
    return(density.bw) 
    
}      

centers<-function(cl,x)
{
    x <- as.matrix(x)
    n <- length(cl)
    k <- max(cl)
    centers <- matrix(nrow = k, ncol = ncol(x))
    {
        for (i in 1:k) 
        {
            for (j in 1:ncol(x)) 
            {
                centers[i, j] <- mean(x[cl == i, j])
            }
        }
    }
    return(centers)
}  

Average.scattering <- function (cl, x)
{
    x <- as.matrix(x)
    n <- length(cl)
    k <- max(cl)
    centers.matrix <- centers(cl,x)
    
    cluster.size <- numeric(0)  
    variance.clusters <- matrix(0, ncol = ncol(x), nrow = k)
    var <- matrix(0, ncol = ncol(x), nrow = k)
    
    for (u in 1:k) 
        cluster.size[u] <- sum(cl == u)
    
    for (u in 1:k) 
    {  
        for (j in 1:ncol(x)) 
        { 
            for(i in 1:n) 
            {     				   
                if(cl[i]==u)                   
                    variance.clusters[u,j]<- variance.clusters[u,j]+(x[i, j]-centers.matrix[u,j])^2 
            }
        }            
    }
    
    for (u in 1:k) 
    {    
        for (j in 1:ncol(x)) 
            variance.clusters[u,j]= variance.clusters[u,j]/ cluster.size[u]   
    }
    
    
    variance.matrix <- numeric(0)
    for(j in 1:ncol(x)) 
        variance.matrix[j]=var(x[,j])*(n-1)/n
    
    
    Somme.variance.clusters<-0
    for (u in 1:k) 
        Somme.variance.clusters<-Somme.variance.clusters+sqrt((variance.clusters[u,]%*%(variance.clusters[u,])))
    
    
    # Standard deviation
    stdev<-(1/k)*sqrt(Somme.variance.clusters)
    
    #Average scattering for clusters  
    scat<- (1/k)* (Somme.variance.clusters /sqrt(variance.matrix %*% variance.matrix))
    
    scat <- list(stdev=stdev, centers=centers.matrix, variance.intraclusters= variance.clusters, scatt=scat)
    return(scat)
}


Index.15and28  <- function (cl1,cl2,md)
{
    md <- as.matrix(dist(md, method = 'euclidean'))
    cn1 <- max(cl1)
    n1 <- length(cl1)
    dmat <- as.matrix(md)
    average.distance <- median.distance <- separation <- cluster.size <- within.dist1 <- between.dist1 <- numeric(0)
    separation.matrix <- matrix(0, ncol = cn1, nrow = cn1)
    di <- list()
    for (u in 1:cn1) 
    {
        cluster.size[u] <- sum(cl1 == u)
        du <- as.dist(dmat[cl1 == u, cl1 == u])
        within.dist1 <- c(within.dist1, du)
        #average.distance[u] <- mean(du)
        #median.distance[u] <- median(du)
        #bv <- numeric(0)
        for (v in 1:cn1) {
            if (v != u) {
                suv <- dmat[cl1 == u, cl1 == v]
                #bv <- c(bv, suv)
                if (u < v) {
                    separation.matrix[u, v] <- separation.matrix[v,u] <- min(suv)
                    between.dist1 <- c(between.dist1, suv)
                }
            }
        }
    }
    cn2 <- max(cl2)
    n2 <- length(cl2)
    dmat <- as.matrix(md)
    average.distance <- median.distance <- separation <- cluster.size <- within.dist2 <- between.dist2 <- numeric(0)
    separation.matrix <- matrix(0, ncol = cn2, nrow = cn2)
    di <- list()
    for (w in 1:cn2) {
        cluster.size[w] <- sum(cl2 == w)
        dw <- as.dist(dmat[cl2 == w, cl2 == w])
        within.dist2 <- c(within.dist2, dw)
        #average.distance[w] <- mean(dw)
        #median.distance[w] <- median(dw)
        bx <- numeric(0)
        for (x in 1:cn2) {
            if (x != w) {
                swx <- dmat[cl2 == w, cl2 == x]
                bx <- c(bx, swx)
                if (w < x) {
                    separation.matrix[w, x] <- separation.matrix[x,w] <- min(swx)
                    between.dist2 <- c(between.dist2, swx)
                }
            }
        }
    }
    nwithin1 <- length(within.dist1)
    nbetween1 <- length(between.dist1)
    meanwithin1 <- mean(within.dist1)
    meanbetween1 <- mean(between.dist1)
    meanwithin2 <- mean(within.dist2)
    meanbetween2 <- mean(between.dist2)
    Index.15 <- (meanbetween2-meanbetween1)/(meanwithin2-meanwithin1)
    Index.28 <- (meanwithin1/nwithin1)/(meanbetween1/nbetween1)
    
    results <- list(frey=Index.15,mcclain=Index.28)
    return(results)
}