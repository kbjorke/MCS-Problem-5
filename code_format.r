# By: Kristian Bjoerke

code_format = function(text)
{
    # Create an list of the characters for the code:
    character_list = c(' ', toupper(letters))


    # Format the text so that only characters expected in code are present:

    # Combine into one continous vector:
    text = paste(text, collapse=" ")

    # Change all lower case characters to upper case characters: 
    text = toupper(text)

    # Replace all punctuation characters with white space:
    text = gsub("[[:punct:]]", " ", text)

    # Remove all digits:
    text = gsub("[[:digit:]]", "", text)

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
    text = chartr(paste(names(special_characters), collapse=''),
                            paste(special_characters, collapse=''),
                            text)

    # Remove all unnecessary white spaces:
    text = gsub("[[:space:]]+", " ", text)

    # Split the list into an array of the characters:
    # (Makes the loop faster, because not using substr() function)
    text = strsplit(text, '')[[1]]

    return(list(text, character_list))
}
