library(stringr)

# Part 1 --------------------------------------------------------------

# read input file
input = readLines("dat/input4.txt")

points = sapply(seq(input), function(i) {
  card = input[i]
  card = unlist(str_split(unlist(str_split(card, "\\|")), ":"))
  id = card[i]
  winning = as.numeric(unlist(str_split(str_squish(card[2]), " ")))
  have = as.numeric(unlist(str_split(str_squish(card[3]), " ")))
  if(sum(winning %in% have) ==0) {
    points = 0
  } else {points = 2**(sum(winning %in% have)-1) }
  
  
}, USE.NAMES = F)

sum(points)



# Part 2 --------------------------------------------------------------

input = readLines("dat/input4.txt")
#input = readLines("dat/ex4.txt")

# Get matching numbers per each card
copycards = sapply(seq(input), function(i) { 
  
  card = input[i]
  card = unlist(str_split(unlist(str_split(card, "\\|")), ":"))
  id = card[i]
  winning = as.numeric(unlist(str_split(str_squish(card[2]), " ")))
  have = as.numeric(unlist(str_split(str_squish(card[3]), " ")))
  copycards = sum(winning %in% have)
}, USE.NAMES = F)


# Initialize a container vector for each card instance
scractchcards = seq(input)

# Make a list of winning cards per card 
my_list = list()
my_list = lapply(seq(scractchcards), function(i) {
  tmp = scractchcards[i]:sum(scractchcards[i] + copycards[i])
  my_list[[i]] = tmp[-1]
  
})

my_list
# Update the card instances with the winning cards
tmp = unlist(my_list)[unlist(my_list) <= length(input)]
scractchcards = c(scractchcards, tmp)

# loop over the elements my_list
instances <- function(number) {
  my_list[[number]]
}

while(length(tmp > 0)) {
  
  #Make a list of winning cards per card 
  tmp = sapply(tmp, function(i) instances(i))
  # Update the card instances with the winning cards
  tmp = unlist(tmp)[unlist(tmp) <= length(input)]
  scractchcards = c(scractchcards, tmp)
}

res = length(scractchcards)