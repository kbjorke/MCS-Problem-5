# By: Kristian Bjoerke

source("code_format.r")

time = as.POSIXlt(Sys.time(), "GMT")

#args = commandArgs(trailingOnly = TRUE)

#filename = args[1]
filename = "example_encrypted_text.txt"

# Number of Trial:
trials = 2

# Number of Monte Carlo samples:
N = 100

# Scaling paramter:
p = 3000

# Score function paramters:
lambda1 = 1
lambda2 = 0

# Source text for transition and character probabilities:
text_stats_source = "pg2600.txt"

ciphertext = readLines(filename)

output = code_format(ciphertext)

formated_ciphertext = output[[1]]
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



num_char = length(formated_ciphertext)

# Initialization of loop:

# Loop over characters in ciphertext:
for( i in 1:num_char )
{
    current_char = formated_ciphertext[i]
    
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

        score_func = score_func + 
                     lambda1*transprob_mat[first_character,second_character] + 
                     lambda2*char_prob[[second_character]]
    }

    return(score_func)
}
    

score_func = score_function(formated_ciphertext, code_key, transprob_matrix)

acceptance = 0

best_score_func = 0



folder_name = sprintf("output-%s/", gsub(".txt", "", filename))

dir.create(file.path(folder_name), showWarning=FALSE)

output_file = file(sprintf("%soutput_%02d-%02d_%02d-%02d.txt", folder_name,
                                   (time$mon+1), time$mday,
                                   time$hour, time$min), "w")

write(ciphertext, file=output_file, append=TRUE)

write("", file=output_file, append=TRUE)



for( t in 1:trials )
{

    for( i in 1:N )
    {
        # Counter:
        if( i %% floor(N/10) == 0 )
        {
            print(sprintf("%d %s", ceiling(100*(i/N)), "%"))
            print(score_func)
            
            print(chartr(paste(names(code_key), collapse=''),
                            paste(code_key, collapse=''),
                            ciphertext[1]))
        }


        # Swap two non-" " characters:
        char_swap = sample(character_list[2:length(character_list)],2)
        #char_swap = sample(character_list,2)

        new_code_key = chartr(paste(char_swap, collapse=''), 
                              paste(rev(char_swap), collapse=''), 
                              code_key)


        new_score_func = score_function(formated_ciphertext, new_code_key,
                                        transprob_matrix)

        if( new_score_func >= score_func )
        {
            code_key = new_code_key
            score_func = new_score_func
            
            acceptance = acceptance + 1
        }
        else
        {
            alpha = new_score_func/score_func

            r = runif(1)

            if( alpha^p >= r )
            {
                code_key = new_code_key
                score_func = new_score_func

                acceptance = acceptance + 1
            }
        }
        #print(score_func)

        if( score_func > best_score_func )
        {
            best_score_func = score_func
            best_code_key = code_key
        }
    }

print(acceptance/N)


plaintext = array(data=NA, dim=length(ciphertext))

#print(ciphertext)
print(best_score_func)
print(best_code_key)
for( i in 1:length(ciphertext) )
{
    # Alternative way of solving, may be slower/faster
#    plaintext[i] = gsub(sorted_char_freq_enctext[[1]],
#                          sorted_char_freq_source[[1]], ciphertext[i])
    plaintext[i] = chartr(paste(names(best_code_key), collapse=''),
                        paste(best_code_key, collapse=''),
                        ciphertext[i])
}
print("")
print(plaintext)
write(plaintext, file=output_file, append=TRUE)
write("", file=output_file, append=TRUE)
}

close(output_file)


