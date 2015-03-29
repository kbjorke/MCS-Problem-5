# By: Kristian Bjoerke

source("code_format.r")

#args = commandArgs(trailingOnly = TRUE)

#filename = args[1]
filename = "example_encrypted_text.txt"

# Number of Monte Carlo samples:
N = 10
p = 1

text_stats_source = "pg2600.txt"

encrypted_text = readLines(filename)

output = code_format(encrypted_text)

formated_encrypted_text = output[[1]]
character_list = output[[2]]

# Read out transition matrix, to be used in encryption script:
transprob_matrix = read.table(sprintf("transprob_matrix-%s-.txt", 
                                      gsub(".txt", "", text_stats_source)))

transprob_matrix = data.matrix(transprob_matrix)
transprob_matrix = matrix(transprob_matrix, nrow=length(character_list),
                          ncol=length(character_list),
                          dimnames=list(character_list, character_list))


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


for( i in 1:length(character_list) ) 
{
    initial_key[sorted_char_freq_enctext[[1]][i]] = 
                                    sorted_char_freq_source[[1]][i]
}

code_key = initial_key

score_function = function(formated_text, key, transprob_mat)
{
    num_char =  length(formated_text)

    score_func = 0
    for( i in 1:(num_char-1) )
    {
        first_character = key[formated_text[i]]
        second_character = key[formated_text[i+1]] 

        score_func = score_func + transprob_mat[first_character,
                                                second_character]
    }

    return(score_func)
}
    
char_swap1 = c("S", "N")
char_swap2 = c("S", "R")
char_swap3 = c("L", "W")

print(code_key)

code_key = chartr(paste(char_swap1, collapse=''), 
                  paste(rev(char_swap1), collapse=''), 
                  code_key)
code_key = chartr(paste(char_swap2, collapse=''), 
                  paste(rev(char_swap2), collapse=''), 
                  code_key)
code_key = chartr(paste(char_swap3, collapse=''), 
                  paste(rev(char_swap3), collapse=''), 
                  code_key)

#print(code_key)


score_func = score_function(formated_encrypted_text, code_key, transprob_matrix)



for( i in 1:N )
{
    # Counter:
    if( i %% floor(N/10) == 0 )
    {
        print(sprintf("%d %s", ceiling(100*(i/N)), "%"))
        print(score_func)
        
        print(chartr(paste(names(code_key), collapse=''),
                        paste(code_key, collapse=''),
                        encrypted_text[1]))
    }


    char_swap = sample(character_list,2)

    new_code_key = chartr(paste(char_swap, collapse=''), 
                          paste(rev(char_swap), collapse=''), 
                          initial_key)

    new_score_func = score_function(formated_encrypted_text, new_code_key,
                                    transprob_matrix)

    if( new_score_func >= score_func )
    {
        code_key = new_code_key
        score_func = new_score_func
    }
    else
    {
        alpha = new_score_func/score_func

        r = runif(1)

        if( alpha^p >= r )
        {
            code_key = new_code_key
            score_func = new_score_func
        }
    }
    #print(score_func)
}


decrypted_text = array(data=NA, dim=length(encrypted_text))

#print(encrypted_text)
#for( i in 1:length(encrypted_text) )
#{
#    # Alternative way of solving, may be slower/faster
##    decrypted_text[i] = gsub(sorted_char_freq_enctext[[1]],
##                          sorted_char_freq_source[[1]], encrypted_text[i])
#    decrypted_text[i] = chartr(paste(names(code_key), collapse=''),
#                        paste(code_key, collapse=''),
#                        encrypted_text[i])
#}
#print(decrypted_text)


