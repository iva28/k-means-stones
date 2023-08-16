get.ordinal <- function(position) {
  if (position == "No") {
    return(position)
  }
  # Remove leading zeros if they exist
  position <- gsub("^0+", "", position)
  
  last.two.digits <- as.numeric(substr(position, nchar(position) - 1, nchar(position)))
  
  if (is.na(last.two.digits)) {
    return(position)
  }
  
  if (length(last.two.digits) == 1) {
    if (last.two.digits == 1) {
      ordinal <- paste(position, "st", sep = "")
    } else if (last.two.digits == 2) {
      ordinal <- paste(position, "nd", sep = "")
    } else if (last.two.digits == 3) {
      ordinal <- paste(position, "rd", sep = "")
    } else {
      ordinal <- paste(position, "th", sep = "")
    }
  } else {
    if (last.two.digits >= 11 && last.two.digits <= 13) {
      ordinal <- paste(position, "th", sep = "")
    } else {
      last.digit <- as.numeric(substr(position, nchar(position), nchar(position)))
      if (last.digit == 1) {
        ordinal <- paste(position, "st", sep = "")
      } else if (last.digit == 2) {
        ordinal <- paste(position, "nd", sep = "")
      } else if (last.digit == 3) {
        ordinal <- paste(position, "rd", sep = "")
      } else {
        ordinal <- paste(position, "th", sep = "")
      }
    }
  }
  return(ordinal)
}

transormed.dataframe <- function(dataset) {
  dataset$`Record Label` <- as.factor(dataset$`Record Label`)
  dataset$`Album type` <- as.factor(dataset$`Album type`)
  dataset$`Lead vocal s ` <- as.factor(dataset$`Lead vocal s `)
  dataset$`British charts` <- as.factor(dataset$`British charts`)
  dataset$Certification <- as.factor(dataset$Certification)
  dataset$`Album name` <- as.factor(dataset$`Album name`)
  
  for(i in which(colnames(dataset) == 'UK Peak Pos'):which(colnames(dataset) == 'POL')) {
    variable <- dataset[,i]
    ordinal.positions.i <- sapply(variable, get.ordinal)
    # Get unique positions from the dataframe
    unique.positions.i <- unique(ordinal.positions.i)
    numeric.vector.i <- sort(as.numeric(gsub("[^0-9]", "", unique.positions.i)))
    numeric.vector.i <- sapply(numeric.vector.i, get.ordinal)
    numeric.vector.i <- c("No",numeric.vector.i)
    
    #creating factor variable
    variable.factor <- factor(ordinal.positions.i, levels = numeric.vector.i)
    #adding factor variable 
    dataset[,i] <- variable.factor
  }
  return(dataset)
}

# function for computing evaluation measures
compute.eval.metrics <- function(cmatrix) {
  TP <- cmatrix[1,1] # true positive
  TN <- cmatrix[2,2] # true negative
  FP <- cmatrix[2,1] # false positive
  FN <- cmatrix[1,2] # false negative
  acc <- sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- 2*precision*recall / (precision + recall)
  c(accuracy = acc, precision = precision, recall = recall, F1 = F1)
}

# function for normalization
normalize.var <- function(x) {
  if (sum(x,na.rm = T) == 0) x
  else ((x - min(x, na.rm = T)) / (max(x,na.rm = T) - min(x, na.rm = T)))
}
# function that computes the difference between two subsequent values
compute.difference <- function(values) {
  dif <- vector(mode = "numeric", length = length(values))
  dif[1] <- NA
  for(i in 1:(length(values)-1)) {
    dif[i+1] <- abs(values[i+1] - values[i])
  }
  dif
}

# function that provides summary statistics about clusters
summary.stats <- function(feature.set, clusters, cl.num) {
  sum.stats <- aggregate(x = feature.set, 
                         by = list(clusters), 
                         FUN = function(x) { 
                           m <- mean(x, na.rm = T)
                           sd <- sqrt(var(x, na.rm = T))
                           paste(round(m, digits = 2), " (", 
                                 round(sd, digits = 2), ")", sep = "")
                         })
  sum.stat.df <- data.frame(cluster = sum.stats[,1], 
                            freq = as.vector(table(clusters)),
                            sum.stats[,-1])
  
  sum.stats.transpose <- t( as.matrix(sum.stat.df) )
  sum.stats.transpose <- as.data.frame(sum.stats.transpose)
  attributes <- rownames(sum.stats.transpose)
  sum.stats.transpose <- as.data.frame( cbind(attributes, sum.stats.transpose) )
  colnames(sum.stats.transpose) <- c( "attributes", rep("Mean (SD)", cl.num) )
  rownames(sum.stats.transpose) <- NULL
  sum.stats.transpose
}

create.attr.boxplot <- function(df, attribute, clust_var) {
  ggplot(data = df,
         mapping = aes(x=.data[[clust_var]], 
                       y=.data[[attribute]], 
                       fill=.data[[clust_var]])) +
    geom_boxplot() +
    labs(y = attribute, x = "") +
    theme_classic()
}


create.comparison.plots <- function(df, clust) {
  library(dplyr)
  library(ggpubr)
  df_clust <- df
  df_clust[['cluster']] <- clust
  boxplots <- lapply(colnames(df), 
                     function(x) create.attr.boxplot(df_clust, x, 'cluster'))
  
  ggarrange(plotlist = boxplots,
            ncol = 3, nrow = 4,
            common.legend = TRUE, legend = "bottom",
            vjust = 1, hjust = -1, font.label = list(size=12))
}