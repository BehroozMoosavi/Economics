class("r")
class("Ryan")
#Whitespaces count as characters and can be included in strings:
  
#" " for space
#"\n" for newline
#"\t" for tab

str = "Dear Mr. Carnegie,\n\nThanks for the great school!\n\nSincerely, Ryan"

cat(str)
str.vec = c("Statistical", "Computing", "isn't that bad") # Collect 3 strings
str.vec # All elements of the vector
str.vec[3] # The 3rd element
str.vec[-(1:2)] # All but the 1st and 2nd

str.mat = matrix("", 2, 3) # Build an empty 2 x 3 matrix
str.mat[1,] = str.vec # Fill the 1st row with str.vec
str.mat[2,1:2] = str.vec[1:2] # Fill the 2nd row, only entries 1 and 2, with
# those of str.vec
str.mat[2,3] = "isn't a fad" # Fill the 2nd row, 3rd entry, with a new string
str.mat # All elements of the matrix
t(str.mat) # Transpose of the matrix
#Converting other data types to strings
#Easy! Make things into strings with as.character()
as.character(0.8)
as.character(TRUE)
as.logical("TRU")
as.logical("True")
#Number of characters
nchar("coffee")
####################
#Getting a substring
phrase = "Give me a break"
substr(phrase, 1, 4)
substr(phrase, nchar(phrase)+1, nchar(phrase)+10)
presidents = c("Clinton", "Bush", "Reagan", "Carter", "Ford")
substr(presidents, 1, 2) # Grab the first 2 letters from each
substr(phrase, 1, 1) = "L"
phrase # "G" changed to "L"

substr(phrase, 1000, 1001) = "R"
phrase # Nothing happened
substr(phrase, 1, 4) = "Show"
phrase # "Live" changed to "Show"
###### Spliting the string
ingredients = "chickpeas, tahini, olive oil, garlic, salt"
split.obj = strsplit(ingredients, split=",")
split.obj
class(split.obj)
length(split.obj)
great.profs = "Nugent, Genovese, Greenhouse, Seltman, Shalizi, Ventura"
favorite.cats = "tiger, leopard, jaguar, lion"
split.list = strsplit(c(ingredients, great.profs, favorite.cats), split=",")
split.list
############

#Splitting character-by-character
#Finest splitting you can do is character-by-character: use strsplit() with split=""

split.chars = strsplit(ingredients, split="")[[1]]
split.chars
### Combine
paste("Spider", "Man") # Default is to separate by " "
paste("Spider", "Man", sep="-")
paste(presidents, c("D", "R", "R", "D", "R"))
paste(presidents, " (", 42:38, ")", sep="")
######Condensing
paste(presidents, collapse="; ")
paste(presidents, " (", 42:38, ")", sep="", collapse="; ")
paste(presidents, collapse=NULL) # No condensing, the default
#####
king.lines = readLines("http://www.stat.cmu.edu/~ryantibs/statcomp/data/king.txt")
class(king.lines) # We have a character vector
length(king.lines) # Many lines (elements)!
king.lines[1:3] 
###### REading from local file
# We don’t need to use the web; readLines() can be used on a local file. The following code would read in a text file from Professor Tibs’ computer:



### Reconstitution
#Fancy word, but all it means: make one long string, then split the words

king.text = paste(king.lines, collapse=" ")
king.words = strsplit(king.text, split=" ")[[1]]
king.words

# Sanity check
substr(king.text, 1, 150)

king.words[1:20]
##### counting words
king.wordtab = table(king.words)
class(king.wordtab)
length(king.wordtab)
king.wordtab[1:10]
### What did we get? Alphabetically sorted unique words, and their counts = number of appearances
king.wordtab[1:5]
names(king.wordtab)[2] == "-"
king.wordtab["freedom"]
### Most frequent words
#Let’s sort in decreasing order, to get the most frequent words
king.wordtab.sorted = sort(king.wordtab, decreasing=TRUE)
length(king.wordtab.sorted)
head(king.wordtab.sorted, 20) # First 20
tail(king.wordtab.sorted, 20) # Last 20
### Visualizing Frequency
nw = length(king.wordtab.sorted)
plot(1:nw, as.numeric(king.wordtab.sorted), type="l",
     xlab="Rank", ylab="Frequency")

C = 100; a = 0.65
king.wordtab.zipf = C*(1/1:nw)^a
cbind(king.wordtab.sorted[1:8], king.wordtab.zipf[1:8])

plot(1:nw, as.numeric(king.wordtab.sorted), type="l",
     xlab="Rank", ylab="Frequency")
curve(C*(1/x)^a, from=1, to=nw, col="red", add=TRUE)