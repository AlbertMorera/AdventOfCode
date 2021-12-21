imput <- advent_input(2, 2021)

x <- 
  imput %>% 
  tidyr::separate(x, c("cmd", "value"), " ", convert=T)

#---------------------------------
# Part 1
#---------------------------------
panel <- data.frame(
  dir = c("down", "up", "forward"),
  x = c(0,0,1),
  y = c(1,-1,0))

xy <- c(0, 0)
for(i in 1:nrow(x)){
  xy <- xy + x$value[i] * panel[match(x$cmd, panel$dir)[i], -1]
}

xy[1] * xy[2]


#---------------------------------
# Part 2
#---------------------------------
panel <- data.frame(
  dir = c("down", "up", "forward"),
  x = c(0,0,1),
  y = c(0,0,0),
  aim = c(1, -1, 0))

xy <- c(0, 0, 0)
for(i in 1:nrow(x)){
  xy <- xy + x$value[i] * as.numeric(panel[match(x$cmd, panel$dir)[i], -1])
  if(x$cmd[i] == "forward")  xy <- xy + c(0, x$value[i] * xy[3], 0)
}

xy[1] * xy[2]

