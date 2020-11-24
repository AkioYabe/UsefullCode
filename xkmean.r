xkmeans <- function(data, num_list) {
  BIC <- sapply(num_list,function(i){
    fit <- kmeans(data,i,iter.max = 5)
    
    m <- ncol(fit$centers)
    n <- length(fit$cluster)
    k <- nrow(fit$centers)
    D <- fit$tot.withinss
    
    return(D + log(n)*m*k)
    #AIC = D + 2*m*k
    #BIC = D + log(n)*m*k
  })
  
  vec <- -diff(BIC)
  nclus <- which.max(vec[-length(vec)] / vec[-1])+1
  
  return(nclus)
}

sapply(1:100, function(x){xkmeans(iris[1:4],1:10)}) %>% table()
