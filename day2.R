library(stringr)

# Part 1 --------------------------------------------------------------------

bag <- c(red = 12, green = 13, blue = 14)

impossible = c()

# read input file
input = readLines("dat/input2.txt")

impossible <- sapply(1:length(input), function(i) {
  # pre-processing 
  game = unlist(strsplit(input[i], ";"))
  id = as.numeric(str_remove(unlist(str_split(game[1], ":"))[1], "Game"))
  game[1] = unlist(str_split(game[1], ":"))[-1]
  
  # sort cubes in each game by cubes in the bag
  for(j in seq_along(game)) {
    cubes = as.numeric(unlist(str_extract_all(game[j], "[0-9.]+")))
    colors = str_squish(unlist(str_split(str_remove_all(game[j], "[0-9]"), ",")))
    idx = match(names(bag), colors)
    
    # check if possible 
    if(sum(cubes[idx] > bag, na.rm = T) > 0) {
      impossible = c(impossible, id)
      }
    
    if(id %in% impossible) {
      break
    }
    
  }
  impossible
  
}, USE.NAMES = F)

possible = seq_along(input)[!seq_along(input) %in% unlist(impossible)]
res <- sum(possible)
res


# Part 2 --------------------------------------------------------------------

# read input file
input = readLines("~/Desktop/input2.txt")

power = sapply(1:length(input), function(i) {
  # pre-processing 
  game = unlist(strsplit(input[i], ";"))
  id = as.numeric(str_remove(unlist(str_split(game[1], ":"))[1], "Game"))
  game[1] = unlist(str_split(game[1], ":"))[-1]
  
  blue = c()
  red = c()
  green = c()
  
  # get number of cubes per color 
  for(j in 1:length(game)) {
    cubes = as.numeric(unlist(str_extract_all(game[j], "[0-9.]+")))
    colors = str_squish(unlist(str_split(str_remove_all(game[j], "[0-9]"), ",")))
    idx = match(c("blue", "red", "green"), colors)
    
    blue = c(blue, cubes[idx][1])
    red = c(red, cubes[idx][2])
    green = c(green, cubes[idx][3])
    
    
  }
  
  # fewest number of cubes of each color
  power = c(max(blue, na.rm = T)*max(red, na.rm = T)*max(green, na.rm = T))
  
}, USE.NAMES = F)


res <- sum(power)
