# By: Kristian Bjoerke

#args = commandArgs(trailingOnly = TRUE)

#filename = args[1]
filename = "example_encrypted_text.txt"

text_stats_source = "pg2600.txt"

encrypted_text = readLines(filename)

### Make includable function of given section, for use in both programs ###

# Combine into one continous vector:
formated_encrypted_text = paste(encrypted_text, collapse=" ")

# Change all lower case characters to upper case characters: 
formated_encrypted_text = toupper(formated_encrypted_text)

# Replace all punctuation characters with white space:
formated_encrypted_text = gsub("[[:punct:]]", " ", formated_encrypted_text)

# Remove all digits:
formated_encrypted_text = gsub("[[:digit:]]", "", formated_encrypted_text)

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
formated_encrypted_text = chartr(paste(names(special_characters), collapse=''),
                        paste(special_characters, collapse=''),
                        formated_encrypted_text)

# Remove all unnecessary white spaces:
formated_encrypted_text = gsub("[[:space:]]+", " ", formated_encrypted_text)

# Split the list into an array of the characters:
# (Makes the loop faster, because not using substr() function)
formated_encrypted_text = strsplit(formated_encrypted_text, '')[[1]]


# Create an list of the character pluss emtpty space:
character_list = c(' ', toupper(letters))

#########



# Read out transition matrix, to be used in encryption script:
transprob_matrix = read.table(sprintf("transprob_matrix-%s-.txt", 
                                      gsub(".txt", "", text_stats_source)))

transprob_matrix = data.matrix(transprob_matrix)
#transprob_matrix = matrix(transprob_matrix, nrow=length(character_list),
#                          ncol=length(character_list),
#                          dimnames=list(character_list, character_list))

# Read out character probabilities, to be used in encryption script:
char_prob = read.table(sprintf("char_prob-%s-.txt",
                               gsub(".txt", "", text_stats_source)))

# Format the char_prob array correctly
char_prob = array(data=char_prob[[2]], dimnames=list(char_prob[[1]]))




# Create an empty vector to be used for character probabilities:
initial_rel_char_freq = array(data=0, dim=length(character_list),
                              dimnames=list(character_list))



num_char = length(formated_encrypted_text)

# Initialization of loop:

# Loop over characters in reference text:
for( i in 1:num_char )
{
    current_char = formated_encrypted_text[i]
    
    # Contribution to characeter probabilities:
    initial_rel_char_freq[current_char] = 
                initial_rel_char_freq[current_char] + 1
}


# Normalize character probabilities:
initial_rel_char_freq = array(initial_rel_char_freq/num_char,
                              dimnames=list(character_list))


sorted_char_freq_source = dimnames(sort(char_prob, decreasing=TRUE))
sorted_char_freq_enctext = dimnames(sort(initial_rel_char_freq, 
                                         decreasing=TRUE))

initial_key = array(data=NA, dim=length(character_list),
                    dimnames=list(character_list))

print(sorted_char_freq_source)
print(sorted_char_freq_enctext)

for( i in 1:length(character_list) ) 
{
    initial_key[sorted_char_freq_enctext[[1]][i]] = 
                                    sorted_char_freq_source[[1]][i]
}

decrypted_text = array(data=NA, dim=length(encrypted_text))

for( i in 1:length(encrypted_text) )
{
    print(encrypted_text[i])
}


