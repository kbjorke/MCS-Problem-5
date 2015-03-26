# By: Kristian Bjoerke

args = commandArgs(trailingOnly = TRUE)

filename = args[1]

# Read the text file of reference textr:
reference_text_original = readLines(filename)

# Combine into one continous vector:
reference_text = paste(reference_text_original, collapse=" ")

# Change all lower case characters to upper case characters: 
reference_text = toupper(reference_text)

# Replace all punctuation characters with white space:
reference_text = gsub("[[:punct:]]", " ", reference_text)

# Remove all digits:
reference_text = gsub("[[:digit:]]", "", reference_text)

# Create list of special characters and A-Z replacements:
special_characters = list('Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 
                          'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 
                          'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E', 'Ê'='E', 
                          'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 
                          'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 
                          'Ö'='O', 'Ø'='O', 'Ù'='U', 'Ú'='U', 'Û'='U', 
                          'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 
                          'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 
                          'æ'='a', 'ç'='c', 'è'='e', 'é'='e', 'ê'='e', 
                          'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 
                          'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 
                          'õ'='o', 'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 
                          'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')

# Repleace special characters with A-Z/a-z characters:
reference_text = chartr(paste(names(special_characters), collapse=''),
                        paste(special_characters, collapse=''),
                        reference_text)

# Remove all unnecessary white spaces:
reference_text = gsub("[[:space:]]+", " ", reference_text)

# Split the list into an array of the characters:
# (Makes the loop faster, because not using substr() function)
reference_text = strsplit(reference_text, '')[[1]]


# Create an list of the character pluss emtpty space:
character_list = c(' ', toupper(letters))


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
          xlab='Probability of Second Letter', 
          ylab='Condition on First Letter')

# Create .eps figure of character probabilities:
postscript(sprintf("char_prob-%s-.eps", gsub(".txt", "", filename)))

barplot(char_prob, las=2,
        xlab="Characters", ylab="Probability (relative frequency)",
        main=sprintf('Character probabilities for "%s"', filename))
