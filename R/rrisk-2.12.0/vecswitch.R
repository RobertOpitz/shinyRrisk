vecswitch <- function(input, alternatives) {
  input_dims <- dim(input)
  input <- as.vector(input)
  
  if (length(alternatives[[1]]) == length(input))
    result <- mapply(function(i, j) alternatives[[i]][j,1],
                     as.vector(input), seq_along(input))
  else
    result <- sapply(input, function(i) alternatives[[i]])
  
  dim(result) <- input_dims
  result
}