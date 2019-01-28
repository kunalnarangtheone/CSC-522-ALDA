###########
# HW1
# Mention your team details here
# Kunal Narang (knarang)
# Samuel Henderson (snhender)
# Anders Liman (aliman)
############

# You may use the following libraries:
# If you get an error when running these lines, 
# make sure to install the respective libraries
require(ggplot2)
require(reshape2)
require(data.table)
require(philentropy)
require(plyr)
require(utils)

# read data matrix
read_data <- function(path = './hw1_word_frequency.csv') {
  # Note 1: DO NOT change the function arguments
  # Input: path: type: string, output: a matrix containing data from hw1_word_frequency.csv
  # Write code here to read the csv file as a matrix and return it.
  df = read.csv(file = path, header=TRUE, sep=",")
  mat = as.matrix(df)
  return(mat)
}

calculate_matrix <- function(data_matrix, method_name){
  # NOTE: This function has already been implemented for you.
  # DO NOT modifiy this function.
  # Input: data_matrix: type: matrix n_sentences x sentence_length,
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # Input: method_name: type: string, can be one of the following values: ('calculate_euclidean', 'calculate_cosine', 'calculate_manhattan', 'calculate_chebyshev')
  # output: a 155 x 155 matrix of type double, containing the distance/similarity calculation using method_name between 
  # every pair of sentences 
  # This function has already been implemented for you. It takes the data matrix and method name, outputs the distance
  # matrix based on the method name.
  # Programming logic for selecting every pair wise rows from the data matrix has already been provided for you
  # for euclidean, cosine and manhattan. You are only required to write the logic to calculate the actual distances for a pair of vectors p and q
  # in the corresponding functions listed above.
  # for chebyshev, you have been tasked to use the library. You will not need to use loops here, since the 
  # library is already optimized. Read the documentation and figure out how to compute the distance matrix
  # without loops for chebyshev
  distance_matrix = matrix(0L, nrow = nrow(data_matrix), ncol = nrow(data_matrix))
  if(method_name %in% c("calculate_euclidean", "calculate_cosine", "calculate_manhattan")){
    # the looping logic for pairwise distances is already provided for you
    for(i in seq(1, nrow(data_matrix))){
      for(j in seq(i, nrow(data_matrix))){
        distance_matrix[i,j] <- do.call(method_name, list(unlist(data_matrix[i,]), unlist(data_matrix[j,])))
        distance_matrix[j,i] <- distance_matrix[i,j]
      }
    }
  }else if(method_name == "calculate_chebyshev"){
    # for chebyshev, you have been tasked to use the library. You will not need to use loops here, since the 
    # library is already optimized. Read the documentation and figure out how to compute the distance matrix
    # without loops for chebyshev
    distance_matrix <- calculate_chebyshev(data_matrix)
  }
  return(distance_matrix)
}

calculate_euclidean <- function(p, q) {
  # Input: p, q are vectors of size 1 x 200, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the euclidean distance between the vectors p and q
  # Write code here to calculate the euclidean distance between pair of vectors p and q
  s <- 0
  for (i in seq(1, length(p))) {
    s <- s + (p[i] - q[i])^2
  }
  
  euclidean <- sqrt(s)
  return(euclidean)
}

calculate_cosine <- function(p, q) {
  # Input: p, q are vectors of size 1 x 200, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the cosine distance between the vectors p and q
  # Write code here to calculate the cosine distance between pair of vectors p and q
  numerator <- 0
  denominator <- 0 
  p.square <- 0
  q.square <- 0
  
  
  for (i in seq(1, length(p))) 
  {
    numerator <- numerator + p[i]*q[i]
    p.square <- p.square + p[i]*p[i]
    q.square <- q.square + q[i]*q[i]
  }
  
  mod.p <- sqrt(p.square)
  mod.q <- sqrt(q.square)
  denominator <- mod.p*mod.q
  
  cosine = numerator/denominator
  return(cosine)
  
}

calculate_manhattan <- function(p, q) {
  # Input: p, q are vectors of size 1 x 200, each representing a row (i.e., a sentence) from the original dataset.
  # output: a single value of type double, containing the manhattan distance between the vectors p and q
  # Write code here to calculate the manhattan distance between pair of vectors p and q
  manhattan <- 0
  
  for (i in seq(1, length(p)))
  {
    manhattan <- manhattan + abs(p[i]-q[i]) 
    
  }
  return(manhattan)
  
}

calculate_chebyshev <- function(data_matrix){
  # Input: data_matrix: type: matrix n_sentences x sentence_length, 
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # output: a 155 x 155 matrix of type double, containing the chebyshev distance between every pair of sentences
  # Write code here to calculate chebyshev distance given an original data matrix of size 155 x 200
  require(philentropy)
  chebyshev <- distance(data_matrix, method = "chebyshev")
  return(chebyshev)
  
}

normalize_data <- function(data_matrix){
  # Input: data_matrix: type: matrix n_sentences x sentence_length, 
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # output: a 155 x 200 matrix of type double, containing the normalized values in [0, 1] range per row.
  # Write code here to normalize data_matrix
  for (i in seq(1, nrow(data_matrix))) {
  for (j in seq(1, ncol(data_matrix))) {
    data_matrix[i,j] <- (data_matrix[i,j] - min(data_matrix[i, ]))/(max(data_matrix[i, ]) - min(data_matrix[i, ]))
  }
  }
  
  return(data_matrix)
  
}

analyze_normalization <- function(data_matrix, normalized_data_matrix){
  # Input: data_matrix, normalized_data_matrix: type: matrix n_sentences x sentence_length, 
  # where n_sentences is total # sentences (155 in the dataset supplied to you) and
  # sentence_length is the total length of each sentence (200 in the dataset supplied to you).
  # data_matrix refers to the original data_matrix, and normalized_data_matrix refers to the normalized version (i.e., output from normalize_data)
  # Output: a 155 x 155 matrix of type double containing the updated euclidean distance using the normalized_data_matrix
  # Also generate the plot(s) that were requested in the question and save them to the pdf.
  # Write code here to generate the output requested as well as any plots/analyses requested.
  
  avg_row_original <- c()
  avg_row_normalized <- c()
  
  
  distance_matrix = matrix(0L, nrow = nrow(data_matrix), ncol = nrow(data_matrix))
  # the looping logic for pairwise distances is already provided for you
  for(i in seq(1, nrow(data_matrix))){
    for(j in seq(i, nrow(data_matrix))){
      distance_matrix[i,j] <- do.call("calculate_euclidean", list(unlist(data_matrix[i,]), unlist(data_matrix[j,])))
      distance_matrix[j,i] <- distance_matrix[i,j]
    }
    avg_row_original[i] <- mean(distance_matrix[i, ])
  }
  
  distance_matrix2 = matrix(0L, nrow = nrow(normalized_data_matrix), ncol = nrow(normalized_data_matrix))
    # the looping logic for pairwise distances is already provided for you
    for(i in seq(1, nrow(normalized_data_matrix))){
      for(j in seq(i, nrow(normalized_data_matrix))){
        distance_matrix2[i,j] <- do.call("calculate_euclidean", list(unlist(normalized_data_matrix[i,]), unlist(normalized_data_matrix[j,])))
        distance_matrix2[j,i] <- distance_matrix2[i,j]
      }
      avg_row_normalized[i] <- mean(distance_matrix2[i, ])
    }

  plot1 <- plot_distance_matrix(distance_matrix)
  plot2 <- plot_distance_matrix(distance_matrix2)
  
  df.avg_row_original <- as.data.frame(avg_row_original)
  df.avg_row_normalized <- as.data.frame(avg_row_normalized)
  
  # print(df.avg_row_original)
  # print(df.avg_row_normalized)

  # plot3 <- qplot(df.avg_row_original$avg_row_original, geom="histogram") 
  plot3 <- ggplot(df.avg_row_original, aes(x=df.avg_row_original$avg_row_original)) + geom_histogram(binwidth = 2) + xlab("Mean") + ylab("Frequency") + labs(title = "Histogram of mean of each row of the original data matrix")
  plot4 <- ggplot(df.avg_row_normalized, aes(x=df.avg_row_normalized$avg_row_normalized)) + geom_histogram(binwidth = 0.5)+ xlab("Mean") + ylab("Frequency") + labs(title = "Histogram of mean of each row of the normalized data matrix")

  
  print("Statistic 1: Comparison of means")
  mean1 <- mean(distance_matrix)
  mean2 <- mean(distance_matrix2)
  print(paste("Mean of data_matrix = ", mean1))
  print(paste("Mean of normalized_data_matrix = ", mean2))
  # 
  # print("Statistic 2: Comparison of standard deviations")
  # sd1 <- sd(distance_matrix)
  # sd2 <- sd(distance_matrix2)
  # print(paste("Standard deviation of data_matrix = ", sd1))
  # print(paste("Standard deviation of normalized_data_matrix = ", sd2))
  # 
  # print("Statistic 3: Comparison of variances")
  # var1 <- var(distance_matrix)
  # var2 <- var(distance_matrix2)
  # print(paste("Variance of data_matrix = ", var1))
  # print(paste("Variance of normalized_data_matrix = ", var2))
  
  #add plots as given in the question
  ggsave("Original distance matrix.pdf", plot1)
  ggsave("Normalized distance matrix.pdf", plot2)
  ggsave("Histogram Row Average Original Data Matrix.pdf", plot3)
  ggsave("Histogram Row Average Normalized Data Matrix.pdf", plot4)


  return(distance_matrix2)
  
}

# This function visualizes a distance matrix, with color indicating distance
plot_distance_matrix <- function(distance_matrix) {
  ggplot(data = melt(distance_matrix), aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() +
    scale_x_discrete(name="Row Number") + scale_y_discrete(name="Row Number") +
    scale_fill_continuous(name="Distance")
}


