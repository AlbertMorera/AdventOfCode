x <- data.frame(cmd = c("forward", "down", "forward", "up", "down", "forward"), 
                value = c(5,5,8,3,8,2))

panel <- data.frame(
  dir = c("down", "up", "forward"),
  x = c(0,0,1),
  y = c(1,-1,0))

# Part 1
xy <- c(0, 0)
for(i in 1:nrow(x)){
  xy <- xy + x$value[i] * panel[match(x$cmd, panel$dir)[i], -1]
}
xy[1] * xy[2]
