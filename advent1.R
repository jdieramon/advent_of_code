library(stringr)

# Read input 
adv1 = readLines("input/1_2023")

# Loop over each line of text 
key = c()
key = sapply(adv1, function(i){
  
  # convert to lowercase
  word = str_to_lower(i)
  
  # unlist to extract characters as a vector
  chars <- unlist(str_split(word, ""))
  
  # keep only numbers 
  nums <- chars[!chars %in% letters]
  
  # double to a single-digit (if necessary) 
  if(length(nums) == 1) {
    nums = as.numeric(str_c(nums, nums))
    key = c(key, nums)} else {
      # extract (combine) the 1st and last numbers
      key <- c(key, as.numeric(str_c(nums[1],tail(nums, n = 1))))
      
    }
  
}, USE.NAMES = F)

# Sum each element 
sum(key)


# Part 2 -----------------------------------
num_byletters <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

word = "two1nine"

# convert to lowercase
word = str_to_lower(word)

# remove NA
tmp = str_extract(word, num_byletters)[!is.na(str_extract(word, num_byletters))]


key = c()

# antes de la 1a palabra
k = head(tmp, n= 1)
chars = str_split(str_split(word, k)[[1]][1],"")[[1]][1]

if(nchar(chars) == 0 | is.na(chars)) {
  nums_tmp = k
} else {nums_tmp = chars}

# convertir a numero 
print(nums_tmp)

if(nchar(nums_tmp) == 1){nums_tmp = as.numeric(nums_tmp) }else{nums_tmp = match(nums_tmp, num_byletters)}

key_tmp = nums_tmp






# despues de ultima palabra
k = tail(tmp, n= 1)
# si hay numero : lo retengo 
chars <-unlist(str_split(str_split(word, k)[[1]][2], "")[[1]])
nums <- chars[!chars %in% letters]

if(length(nums) == 0) {nums_tmp = k} else {nums_tmp = tail(nums, n = 1)}

# convertir a numero 
print(nums_tmp)
if(nchar(nums_tmp) == 1){nums_tmp = as.numeric(nums_tmp) }else{nums_tmp = match(nums_tmp, num_byletters)}

nums_tmp

key_tmp = as.numeric(str_c(key_tmp, nums_tmp))

key = c(key, key_tmp)
key





abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
