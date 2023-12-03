library(stringr)



# Part 1 --------------------------------------------------------------------

bag <- c(red = 12, green = 13, blue = 14)

impossible = c()

# read input file
input = readLines("~/Desktop/day2.txt")
input = readLines("~/Desktop/input2.txt")

impossible <- sapply(1:length(input), function(i) {
  game = unlist(strsplit(input[i], ";"))
  id = as.numeric(str_remove(unlist(str_split(game[1], ":"))[1], "Game"))
  game[1] = unlist(str_split(game[1], ":"))[-1]
  
  for(j in seq_along(game)) {
    cubes = as.numeric(unlist(str_extract_all(game[j], "[0-9.]+")))
    colors = str_squish(unlist(str_split(str_remove_all(game[j], "[0-9]"), ",")))
    idx = match(names(bag), colors)
    
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







# Manual ----------------------------------------------
# read line 3
game = unlist(strsplit(readLines("~/Desktop/day2.txt")[3], ";"))





# remove "Game x" from the 1st element 

id = as.numeric(str_remove(unlist(str_split(game[1], ":"))[1], "Game"))
game[1] = unlist(str_split(game[1], ":"))[-1]

length(game)
# loop over the elemtents 
i = 1
cubes = as.numeric(unlist(str_extract_all(game[i], "[0-9.]+")))
colors = str_squish(unlist(str_split(str_remove_all(game[i], "[0-9]"), ",")))
idx = match(names(bag), colors)
cubes[idx]

cubes[idx] > bag
if(sum(cubes[idx] > bag)) {impossible = c(impossible, id)}
if(id %in% impossible) {break}



for(i in seq_along(game)) {
  cubes = as.numeric(unlist(str_extract_all(game[i], "[0-9.]+")))
  colors = str_squish(unlist(str_split(str_remove_all(game[i], "[0-9]"), ",")))
  idx = match(names(bag), colors)
  
  if(sum(cubes[idx] > bag)) {impossible = c(impossible, id)}
  if(id %in% impossible) {
    break
    }
  
}

impossible






bag # ordenar en red - green - blue 

