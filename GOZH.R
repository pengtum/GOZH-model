
GOZH <- function(formula, data, location,complexity){
  
  library(GD)
  library(rpart)
  library(IDSA)

  y <- all.vars(formula)[1]
  vars <- all.vars(formula)[-1]

  data.y <- data[, y]
  data.y <- as.vector(as.matrix(data.y))
  data.x <- data[, vars]
  data.loc <- data[, location]

  nx <- length(vars)
  

  
print('start')
  # combinations of x
  cox <- list()
  for (i in 1:nx){
    coxi <- combn(1:nx, i)
    coxl <- split(coxi, rep(1:ncol(coxi), each = nrow(coxi)))
    cox <- c(cox, coxl)
  }
  names(cox) <- 1:length(cox)
  #

  var <- unlist(lapply(1:length(cox), function(u)
    paste(vars[cox[[u]]], collapse = "_")))
  n.var <- sapply(cox, length)
  result <- data.frame("var" = var, "n.var" = n.var,
                       "q.variance" = rep(NA, length(cox)))
  # "q.entropy" = rep(NA, length(cox)),
  # "q.gini" = rep(NA, length(cox)),
  # "q.dependence" = rep(NA, length(cox))

  for (i in 1:length(cox)){
    if(i%%1000==0)
    {
      out=paste('the',i,'in',length(cox))
      print(out)
    }
      
    m <- cox[[i]]
    g <- as.formula(paste(y, paste(vars[m], collapse = "+"), sep = "~"))

    git <- rpart(g, method='anova', data=data,cp=complexity) # ??€??ç?“æ?œæ˜¯0.0001

    
    zones <- as.character(git$where)
    
    di <- data.frame(data.y, zones)
    gdi <- gd(data.y ~ zones, di)
    result[i, 3] <- gdi$Factor$qv
    result[i, 4] <- length(unique(zones))
  }
  
  # if (method == "variance") {
  #   k <- which(result[, 3] == max(result[, 3]))[1]
  # }
  # if (method == "entropy") {
  #   k <- which(result[, 4] == max(result[, 4]))[1]
  # }
  # if (method == "gini") {
  #   k <- which(result[, 5] == max(result[, 5]))[1]
  # }
  # if (method == "dependence") {
  #   k <- which(result[, 6] == max(result[, 6]))[1]
  # }
  
  k <- which(result[, 3] == max(result[, 3]))[1]
  
  var.k <- vars[cox[[k]]]
  
  f.k <- as.formula(paste(y, paste(var.k, collapse = "+"), sep = "~"))
  z <- list("best.vars" = var.k, "best.formula" = f.k,
            "all.q" = result)
  
  
  
  return(z)

  

  
}


