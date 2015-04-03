# By: Kristian Bjoerke

source("code_format.r")

time = as.POSIXlt(Sys.time(), "GMT")

filename = "encrypted_text.txt"

# Source text for transition and character probabilities:
text_stats_source = "pg2600.txt"

# Number of Trial:
trials = 1

# Number of Monte Carlo samples:
N = 1000

# Scaling paramter:
p = 1500

# Score function paramters:
lambda1 = 0.15
lambda2 = 0.85

# Score funcrtion:
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
                     lambda2*char_prob[[first_character]]
    }
    score_func = score_func + lambda2*char_prob[[key[formated_text[num_char]]]]

    return(score_func)
}


ciphertext = readLines(filename)

input = code_format(ciphertext)

formated_ciphertext = input[[1]]
character_list = input[[2]]

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

    

score_func = score_function(formated_ciphertext, code_key, transprob_matrix)




folder_name = sprintf("output-%s/%d-%02d-%02d/", gsub(".txt", "", filename),
                      (time$year+1900),(time$mon+1), time$mday)

dir.create(file.path(folder_name), showWarning=FALSE)

output_file = file(sprintf("%soutput_%02d-%02d_%02d-%02d.txt", folder_name,
                                   (time$mon+1), time$mday,
                                   time$hour, time$min), "w")

# Write file header:
write(sprintf("Time: %s", time), file=output_file, append=TRUE)
write(sprintf("File of encrypted text: %s", filename), 
      file=output_file, append=TRUE)
write("\nDecryption parameters:", file=output_file, append=TRUE)
write(sprintf("N = %d \nScaling parameter = %d", N, p), 
      file=output_file, append=TRUE)
write(sprintf("lambda_1 = %.2f \nlambda_2 = %.2f", lambda1, lambda2), 
      file=output_file, append=TRUE)
write("\nCiphertext:\n", file=output_file, append=TRUE)
write(ciphertext, file=output_file, append=TRUE)
write(sprintf("\nNumber of trials: %d", trials), 
      file=output_file, append=TRUE)


for( t in 1:trials )
{
    acceptance = 0

    best_score_func = 0

    print("")
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
    for( i in 1:length(ciphertext) )
    {
        plaintext[i] = chartr(paste(names(best_code_key), collapse=''),
                            paste(best_code_key, collapse=''),
                            ciphertext[i])
    }
    print("")
    print(plaintext)

    len = length(best_code_key)

    best_code_key_output = sprintf("{%s} = %s", names(best_code_key),
                                         best_code_key)
    best_code_key_output = sprintf("%s    %s    %s",
                                   best_code_key_output[
                                        seq(1,floor(len/3))],
                                   best_code_key_output[
                                        seq(1+floor(len/3),2*floor(len/3))],
                                   best_code_key_output[
                                        seq(1+2*floor(len/3),3*floor(len/3))])


    
    write(sprintf("\n\nTrial number: %d", t), file=output_file, append=TRUE)
    write(sprintf("Acceptance rate: %.4f", acceptance/N), 
          file=output_file, append=TRUE)
    write(sprintf("Score function: %.3f", best_score_func), 
          file=output_file, append=TRUE)
    write("\nPlaintext (best guess):\n", file=output_file, append=TRUE)
    write(plaintext, file=output_file, append=TRUE)
    write("\nDecryption key:\n", file=output_file, append=TRUE)
    write(best_code_key_output, file=output_file, append=TRUE)
}

close(output_file)


