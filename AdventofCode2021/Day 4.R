imput <- advent_input(4, 2021)

#---------------------------------
# Part 1
#---------------------------------
drawn <- as.numeric(stringr::str_split(imput$x[1], ",")[[1]])

boards <- 
  imput %>%
  slice(-1)

boards <- boards$x[boards$x != ""]
num.lines <- length(boards)
num.boars <- num.lines / 5
boards.data <- array(rep(NA, 5*5*num.boars), c(5,5, num.boars))

for(b in 1:num.boars){
  for(l in 1:5){
    new <- stringr::str_split(boards[(b-1)*5 + (l)], " ")[[1]]
    new <- new[new != ""]
    boards.data[l,,b] <- new
  }
}

winner <- NA
for(d in 1:length(drawn)){
  for(b in 1:num.boars){
    position <- match(as.character(drawn[d]), boards.data[,,b])
    boards.data[,,b][position] <- NA
  }
  
  for(b in 1:num.boars){
    if(any(rowSums(is.na(boards.data[,,b])) == ncol(boards.data[,,b])) | 
       any(colSums(is.na(boards.data[,,b])) == ncol(boards.data[,,b]))){
      
      winner <- sum(as.numeric(boards.data[,,b]), na.rm = T)
      break()
    }
  }
  if(!is.na(winner)) break()
}

winner * drawn[d]


b.w <- array(NA, num.boars)



b.w[20] <- 20

isFALSE(any(is.na(b.w)))

looser <- b




