`%paste%`<-function(mat1,mat2) {
  mat1 = as.matrix(mat1)
  mat2 = as.matrix(mat2)
  t(apply(mat1, 1, function(l){
    apply(matrix(paste(l, mat2, sep = "->"), nrow = nrow(mat2)), 2,
          function(v) {paste(v, collapse = ";")})
  }))
}
