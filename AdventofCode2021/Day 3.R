imput <- advent_input(3, 2021)

#---------------------------------
# Part 1
#---------------------------------
m <- sapply(imput$x, function(x) substring(x, first=1:nchar(imput$x[1]), last=1:nchar(imput$x[1])))
class(m) <- "numeric"

m_summary <- apply(m, 1, FUN=table)

gamma.rate.bin <- rownames(m_summary)[apply(m_summary, 2, function(x) match(sort(x, T)[1], x))]
epsilon.rate.bin <- rownames(m_summary)[apply(m_summary, 2, function(x) match(sort(x, F)[1], x))]

gamma.rate.dec <- strtoi(paste(gamma.rate.bin, collapse = ''), base = 2)
epsilon.rate.dec <- strtoi(paste(epsilon.rate.bin, collapse = ''), base = 2)

# Resoult (Power consumption)
gamma.rate.dec * epsilon.rate.dec


#---------------------------------
# Part 2
#---------------------------------
m <- sapply(imput$x, function(x) substring(x, first=1:nchar(imput$x[1]), last=1:nchar(imput$x[1])))

O2 <- m
for(i in 1:nchar(imput$x[1])){
  if(length(unique(O2[i, ])) != 1)
    if(table(O2[i, ])[1]==table(O2[i, ])[2])
      sel <- "1"
    else
      sel <- names(sort(table(O2[i, ]), T))[1]
    O2 <- O2[, O2[i,] == sel]
    if(is.null(ncol(O2))) break()
}

CO2 <- m
for(i in 1:nchar(imput$x[1])){
  if(length(unique(CO2[i, ])) != 1)
    if(table(CO2[i, ])[1]==table(CO2[i, ])[2])
      sel <- "0"
    else
      sel <- names(sort(table(CO2[i, ]), F))[1]
    CO2 <- CO2[, CO2[i,] == sel]
    if(is.null(ncol(CO2))) break()
}



strtoi(paste(O2, collapse = ''), base = 2) * strtoi(paste(CO2, collapse = ''), base = 2)



