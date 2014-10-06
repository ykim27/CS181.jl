#################################################
#
#   LabeledGraph
#   contributors: Matthew Reyna, Youn Kim
#
#################################################

type LabeledGraph{V} <: AbstractGraph{V, Vector{V}}
    is_directed :: Bool
    dictionary :: Dict{V,Int}
    adjacency :: Array{Real,2}
	
	LabeledGraph(is_directed :: Bool, dictionary :: Dict) = new(is_directed, dictionary, Array(Real, (0,0)))
	LabeledGraph(is_directed) = new(is_directed, Dict(), Array(Real, (0,0)))
end

@graph_implements LabeledGraph vertex_list vertex_map adjacency_list

function labeled_graph{V}(vs::Vector{V}, es::Vector{Vector{V}}; is_directed::Bool=true)

    g = labeled_graph{V}(is_directed)

    for v in vs
        add_vertex!(g,v)
    end
    for e in es
        add_edge!(g,e[1],e[2])
    end

    return g

end

## required interfaces

is_directed(g::LabeledGraph) = g.is_directed

num_vertices(g::LabeledGraph) = length(g.dictionary)

vertices(g::LabeledGraph) = [v for v in keys(g.dictionary)]

vertex_index{V}(v::V, g::LabeledGraph{V}) = g.dictionary[v]

num_edges(g::LabeledGraph) = countnz(g.adjacency)

out_degree{V}(v::V, g::LabeledGraph{V}) = countnz(g.adjacency[g.dictionary[v],:])

function out_neighbors{V}(v::V, g::LabeledGraph{V})

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

function add_vertex!{V}(g::LabeledGraph{V}, v::V)

    if !haskey(g.dictionary,v)

        n = length(g.dictionary)
        m = size(g.adjacency,1)

        g.adjacency = [g.adjacency zeros(m,1);
                       zeros(1,m)  zeros(1,1)]
        g.dictionary[v] = n+1

    end

    return g

end

function remove_vertex!{V}(g::LabeledGraph, v::V)

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

function add_edge!{V}(g::LabeledGraph, u::V, v::V)

    if !haskey(g.dictionary,u)
        add_vertex!(g,u)
    end
    if !haskey(g.dictionary,v)
        add_vertex!(g,v)
    end

    p = g.dictionary[u]
    q = g.dictionary[v]

    if g.is_directed
        g.adjacency[p,q] = 1
    else
        g.adjacency[p,q] = 1
        g.adjacency[q,p] = 1
    end

    return g

end

function remove_edge!{V}(g::LabeledGraph, u::V, v::V)

    if haskey(g.dictionary,u) && haskey(g.dictionary,v)

        p = g.dictionary[u]
        q = g.dictionary[v]

        if g.is_directed
            g.adjacency[p,q] = 0
        else
            g.adjacency[p,q] = 0
            g.adjacency[q,p] = 0
        end

    end

    return g

end

function set_edge!{V}(g::LabeledGraph, u::V, v::V, weight::Real)

    if !haskey(g.dictionary,u)
        add_vertex!(g,u)
    end
    if !haskey(g.dictionary,v)
        add_vertex!(g,v)
    end

    p = g.dictionary[u]
    q = g.dictionary[v]

    if g.is_directed
        g.adjacency[p,q] = e
    else
        g.adjacency[p,q] = e
        g.adjacency[q,p] = e
    end

    return g

end