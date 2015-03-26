# By: Kristian Bjoerke

#args = commandArgs(trailingOnly = TRUE)

#filename = args[1]

# Read out transition matrix, to be used in encryption script:
transprob_matrix = read.table("transprob_matrix-pg2600-.txt", header=TRUE)
#transprob_matrix = data.matrix(transprob_matrix)

# Read out character probabilities, to be used in encryption script:
char_prob = read.table("char_prob-pg2600-.txt")
#char_prob = data.matrix(char_prob)

print(transprob_matrix)
print(char_prob)
