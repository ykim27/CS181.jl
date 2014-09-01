module Alignment

	export
	    GAP,
	    Scoring, 
	    StandardScoring, 
	    CharacterMatrixScoring,
	    score


    # Julia files
    include("similarity_matrix.jl")
    include("alignment_base.jl")

end
