#######################
# ALDA: hw0.R
# Instructor: Dr. Thomas Price
# Mention your team details here
#
#
#
#
#########################
require(ggplot2)

# no install.packages or rm(list=ls(all=T)) in this function


intro_to_r <- function(num_values){
  # input: num_values type: integer, specifies the sample size of the random vector you are to generate.
  # output: a  list:  [generated vector (type: double), mean of vector (type: double), median of the vector (type: double), max value of vector (type: double), min value of the vector (type: double)]
  
  
}

#######################
# ALDA: hw0.R
# Instructor: Dr. Thomas Price
# Mention your team details here
#
#
#
#
#########################
require(ggplot2)
set.seed(123)
# no install.packages or rm(list=ls(all=T)) in this function


intro_to_r <- function(num_values){
  # input: num_values type: integer, specifies the sample size of the random vector you are to generate.
  # output: a  list:  [generated vector (type: double), mean of vector (type: double), median of the vector (type: double), max value of vector (type: double), min value of the vector (type: double)]
  
  
}

set.seed(123)
require(ggplot2)
intro_to_plotting <- function(num_values){
  # input: num_values type: integer, specifies the sample size of the random vector you are to generate.
  # output: two plots (saved to disk, no return value), descriptions for which have been provided in the hw0 document. 
  new_vector = sample(1:num_values*2, num_values, replace = FALSE)
  new_vector2 = new_vector^2
  plot1 = qplot(new_vector, new_vector)
  plot2 = qplot(new_vector, new_vector2)
  
  ggsave("plot1.pdf", plot1)
  ggsave("plot2.pdf", plot2)
}

# do not call either function in this script 
# this script is only for function definitions
# if you wish to test your script, call your functions separately in a separate script.

# do not call either function in this script 
# this script is only for function definitions
# if you wish to test your script, call your functions separately in a separate script.