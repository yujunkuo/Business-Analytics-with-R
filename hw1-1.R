#solution 1
data(iris)
data <- iris[,-5]
dis_matrix <- matrix(data = 0,nrow = 150,ncol = 150)

  for(i in 2:150){
    for(j in 1:(i-1)){
      dis_matrix[i,j] <- sum((data[i,]-data[j,])^2)^0.5
      dis_matrix[j,i] <- dis_matrix[i,j]
    }
  }

#solution 2
dis_matrix2 <- matrix(data = NA,nrow = 150,ncol = 150)

com_dis <- function(i,j){
  x <- (iris[i,1] - iris[j,1])^2 + (iris[i,2] - iris[j,2])^2 + (iris[i,3] - iris[j,3])^2 + (iris[i,4] - iris[j,4])^2
  return(sqrt(x))
}

for(i in 1:150){
  for(j in 1:150){
    dis_matrix2[i,j] <- com_dis(i,j)
  }
}
