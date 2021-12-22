imput <- advent_input(5, 2021)

#---------------------------------
# Part 1 (I)
#---------------------------------
values <- unlist(stringr::str_split(imput$x, " -> "))
values <- unlist(stringr::str_split(values, ","))

df <- t(array(rep(values, 4*nrow(imput)), c(4, nrow(imput))))

class(df) <- "numeric"
df <- as.data.frame(df)

if.vh <- function(x){
  if(x[1]==x[3] | x[2]==x[4]){
    return("Y")
  }else{
    return("N")
  }
}

df$vh <- unlist(apply(df, 1, if.vh))
df.vh <- subset(df, vh=="Y")[,-5]

surface <- array(rep(0, 1000*1000), c(1000, 1000))
for(i in 1:nrow(df.vh)){
  if(df.vh[i,1] == df.vh[i,3]){
    for(y in purrr::map2(df.vh[i,2],df.vh[i,4], seq)){
      surface[df.vh[i,1], y] <- surface[df.vh[i,1], y] + 1
    }
  }else{
    for(x in purrr::map2(df.vh[i,1],df.vh[i,3], seq)){
      surface[x, df.vh[i,2]] <- surface[x, df.vh[i,2]] + 1
    }
  }
}

table(surface>1)


#---------------------------------
# Part 1 (II)
#---------------------------------
imput %>%
  tidyr::extract(x, c("x1", "y1", "x2", "y2"), "(\\d+),(\\d+) -> (\\d+),(\\d+)", convert=T) %>%
  mutate(x=purrr::map2(x1,x2, seq)) %>%
  mutate(y=purrr::map2(y1,y2, seq)) %>%
  filter(x1==x2 | y1==y2) %>%
  tidyr::unnest(c(x, y)) %>%
  count(x,y) %>%
  summarize(sum(n>1))


#---------------------------------
# Part 2
#---------------------------------
imput %>%
  tidyr::extract(x, c("x1", "y1", "x2", "y2"), "(\\d+),(\\d+) -> (\\d+),(\\d+)", convert=T) %>%
  mutate(x=purrr::map2(x1,x2, seq)) %>%
  mutate(y=purrr::map2(y1,y2, seq)) %>%
  tidyr::unnest(c(x, y)) %>%
  count(x,y) %>%
  summarize(sum(n>1))
















