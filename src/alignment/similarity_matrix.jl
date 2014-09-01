type CharacterSimilarityMatrix
    char_index::Dict{Char,Int}
    m::SparseMatrixCSC{Int,Int}
end

CharacterSimilarityMatrix(filename::String) = CharacterSimilarityMatrix(load_similarity_matrix(filename)...)

char_index(simMat::CharacterSimilarityMatrix, c::Char) = simMat.char_index[c]
lookup(simMat::CharacterSimilarityMatrix, i::Int, j::Int) = (j > i) ? lookup(simMat,j,i) : simMat.m[i,j]

################
# Input parser #
################
function load_similarity_matrix(filename::String)
    f = open(filename, "r")
    char_index = (Char=>Int)[]
    I = Int[]
    J = Int[]
    V = Int[]
	
    lineNum::Int = 0
    for line in eachline(f)
        if beginswith(line, '#')
            continue
        end
        tokens = split(line)
        if lineNum == 0
            for (idx,charToken) in enumerate(tokens)
                char_index[charToken[1]] = idx
            end
        else
            for (idx,s) in enumerate(tokens)
                push!(I,lineNum)
                push!(J,idx)
                push!(V,parseint(s,10))
            end
        end

        lineNum += 1
    end
    close(f)

    return (char_index, sparse(I,J,V))
end
