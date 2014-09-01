# The universal GAP character for this assignment.

const GAP = '-'

######################
## Scoring schemes. ##
######################
abstract Scoring
score(s::Scoring, x::Char, y::Char) = (x == GAP || y == GAP) ? s.gap : match_score(s,x,y)

immutable StandardScoring <: Scoring
    match::Int
    mismatch::Int
    gap::Int
end

match_score(s::StandardScoring, x::Char, y::Char) = (x == y) ? s.match : s.mismatch

type CharacterMatrixScoring <: Scoring
    matrix::CharacterSimilarityMatrix
    gap::Int
end

match_score(s::CharacterMatrixScoring, x::Char, y::Char) = lookup(s.matrix, char_index(s.matrix,x), char_index(s.matrix,y))

# Provide an option to load from file.
CharacterMatrixScoring(matrixFile::String, gap::Int) = CharacterMatrixScoring(CharacterSimilarityMatrix(matrixFile), gap)
