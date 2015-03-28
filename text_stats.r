# By: Kristian Bjoerke

source("code_format.r")

args = commandArgs(trailingOnly = TRUE)

filename = args[1]
#filename = "test_pg2600.txt"

# Read the text file of reference textr:
reference_text_original = readLines(filename)

output = code_format(reference_text_original)

reference_text = output[[1]]
character_list = output[[2]]


# Create an empty transition probability matrix for the character list:
transprob_matrix = matrix(0, nrow=length(character_list),
                          ncol=length(character_list),
                          dimnames=list(character_list, character_list))

# Create an empty vector to be used for character probabilities:
char_prob = array(data=0, dim=length(character_list),
                  dimnames=list(character_list))



num_char = length(reference_text)

# Initialization of loop:
current_char = reference_text[1]

char_prob[current_char] = char_prob[current_char] + 1

# Loop over characters in reference text:
for( i in 2:num_char )
{
    # Counter:
    if( i %% floor(num_char/10) == 0 )
    {
        print(sprintf("%d %s", ceiling(100*(i/num_char)), "%"))
    }

    next_char = reference_text[i]

    # Contribution to transition probability matrix:
    transprob_matrix[current_char, next_char] = 
                        transprob_matrix[current_char, next_char] + 1

    current_char = next_char
    
    # Contribution to characeter probabilities:
    char_prob[current_char] = char_prob[current_char] + 1
}


# Normalization loop (normalize each row) for transition probability matrix:
for( char in character_list )
{
    norm_factor = sum(transprob_matrix[char, ])
    if( norm_factor )
    {
        transprob_matrix[char, ] = transprob_matrix[char, ]/norm_factor
    }
}

# Normalize character probabilities:
char_prob = char_prob/num_char


# Write the content of the transition matrix to a .txt file:
write.table(transprob_matrix, 
            file=sprintf("transprob_matrix-%s-.txt", 
                         gsub(".txt", "", filename)))

# Write the character probabilites to a .txt file:
write.table(char_prob,
            file=sprintf("char_prob-%s-.txt", 
                         gsub(".txt", "", filename)),
            col.names=FALSE)


setEPS()
# Create .eps figure of transition probability matrix:
postscript(sprintf("transprob_matrix-%s-.eps", gsub(".txt", "", filename)))

library(lattice)
levelplot(t(apply(transprob_matrix, 2, rev)), 
          col.regions = gray(1-0:255/255),
          main=sprintf('Transition probablity matrix for "%s"', filename),
          xlab='Probability of Second Character', 
          ylab='Condition on First Character')

# Create .eps figure of character probabilities:
postscript(sprintf("char_prob-%s-.eps", gsub(".txt", "", filename)))

barplot(char_prob, las=2,
        xlab="Characters", ylab="Probability (relative frequency)",
        main=sprintf('Character probabilities for "%s"', filename))


