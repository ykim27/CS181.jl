#################################################
#
#   MultiGraph
#   contributors: Matthew Reyna, Youn Kim
#
#   Adaptations of graphs.jl and dot.jl from Graphs.jl
#
#################################################
type MultiGraph{V} <: AbstractGraph{V, Vector{V}}
    is_directed :: Bool
    dictionary :: Dict{V,Int}
    adjacency :: Array{Int,2}
    
    MultiGraph(is_directed :: Bool, dictionary :: Dict) = new(is_directed, dictionary, Array(Int, (0,0)))
    MultiGraph(is_directed) = new(is_directed, Dict(), Array(Int, (0,0)))
end

@graph_implements MultiGraph vertex_list vertex_map adjacency_list

## required interfaces

is_directed(g::MultiGraph) = g.is_directed

num_vertices(g::MultiGraph) = length(g.dictionary)

vertices(g::MultiGraph) = [v for v in keys(g.dictionary)]

vertex_index{V}(v::V, g::MultiGraph{V}) = g.dictionary[v]

function num_edges(g::MultiGraph)
    n = sum(g.adjacency);
    if is_directed(g)
        return n
    else
        return n / 2
    end
end

out_degree{V}(v::V, g::MultiGraph{V}) = sum(g.adjacency[g.dictionary[v],:])

function out_neighbors{V}(v::V, g::MultiGraph{V})

    i = g.dictionary[v]
    adjacent_vertices = {}

    for w in keys(g.dictionary)
        if g.adjacency[i,g.dictionary[w]]>0
            push!(adjacent_vertices,w)
        end
    end

    return adjacent_vertices

end

## mutation

function add_vertex!{V}(g::MultiGraph{V}, v::V)

    if !haskey(g.dictionary,v)

        n = length(g.dictionary)
        m = size(g.adjacency,1)

        g.adjacency = [g.adjacency zeros(m,1);
                    zeros(1,m)  zeros(1,1)]
        g.dictionary[v] = n+1

    end

    return g

end

function remove_vertex!{V}(g::MultiGraph, v::V)

    if haskey(g.dictionary,v)

        p = g.dictionary[v]
        n = length(g.dictionary)
        m = size(g.adjacency,1)

        g.adjacency = [g.adjacency[1:p-1,1:p-1] g.adjacency[1:p-1,p+1:m];
                    g.adjacency[p+1:m,1:p-1] g.adjacency[p+1:m,p+1:m]]

        delete!(g.dictionary,v)
        for w in keys(g.dictionary)
            if g.dictionary[w]>p
                g.dictionary[w] -=1
            end
        end

    end

    return g

end

function add_edge!{V}(g::MultiGraph{V}, u::V, v::V)

    if !haskey(g.dictionary,u)
        add_vertex!(g,u)
    end
    if !haskey(g.dictionary,v)
        add_vertex!(g,v)
    end

    p = g.dictionary[u]
    q = g.dictionary[v]

    if g.is_directed
        g.adjacency[p,q] += 1
    else
        g.adjacency[p,q] += 1
        g.adjacency[q,p] += 1
    end

    return g

end

function remove_edge!{V}(g::MultiGraph, u::V, v::V)
    if haskey(g.dictionary,u) && haskey(g.dictionary,v)

        p = g.dictionary[u]
        q = g.dictionary[v]

        if g.adjacency[p,q] > 0
            if g.is_directed
                g.adjacency[p,q] -= 1
            else
                g.adjacency[p,q] -= 1
                g.adjacency[q,p] -= 1
            end
        end
    end

    return g

end
