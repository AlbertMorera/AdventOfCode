imput <- adventdrob::advent_input(1, 2021)


## Part 1
class(imput$x) <- "numeric"

sum(diff(imput$x)>0)


# Part 2
class(imput$x) <- "numeric"

x <- c(NULL)
for(i in 1:nrow(imput)){
  x <- c(x, sum(imput[i:(i+2), ]))
}

sum(diff(x[!is.na(x)])>0)



