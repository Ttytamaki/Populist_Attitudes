###
# Conferindo os possíveis valores da escala de atitudes populistas

a<- c(0, 0.25, 0.5, 0.75, 1)
b<- c(0, 0.25, 0.5, 0.75, 1)
c <- c(0, 0.25, 0.5, 0.75, 1)

mat <- matrix(nrow = 1, ncol=2)
for(i in a){
  for(j in b){
    for(k in c){
      comb <- paste(i,j,k, sep=', ')
      prod <- i*j*k
      mat <- rbind(mat,c(comb,prod))
    }
  }
}
mat<-mat[-1,]

mat


mat <- matrix(NA, nrow = 1, ncol=2)
for(i in a){
  for(j in b){
    for(k in c){
      if((i * j * k) > 0){
        comb <- paste(i, j, k, sep = ", ")
        prod <- i * j * k
        mat <- rbind(mat, c(comb, prod))
      }
    }
  }
}


table(e19$pop)


mat[mat[,2] == 0.421875,]
