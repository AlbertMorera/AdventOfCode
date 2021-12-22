imput <- advent_input(7, 2021)

x <- as.numeric(stringr::str_split(imput$x, ",")[[1]])

x <- c(16,1,2,0,4,2,7,1,2,14)
#---------------------------------
# Part 1
#---------------------------------
comb <- c(NULL)
for(i in 0:max(x)){
  comb <- c(comb, sum(abs(x-i)))
}

min(comb)

#---------------------------------
# Part 2
#---------------------------------
comb <- c(NULL)
for(i in 0:max(x)){
  combustible <- lapply(abs(x-i), function(x){
    co = 0 
    for(count in 1:x){
      co = count + co
    }
    return(co)
  })
  comb <- c(comb, sum(unlist(combustible)))
}

min(comb)
