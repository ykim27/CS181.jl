#################################################
#
#   LabeledGraph
#   contributors: Matthew Reyna, Youn Kim
#
#   Adaptations of graphs.jl and dot.jl from Graphs.jl
#
#################################################
type LabeledGraph{V} <: AbstractGraph{V, Vector{V}}
    is_directed :: Bool
    dictionary :: Dict{V,Int}
    adjacency :: Array{Int,2}
    edge_properties :: Dict{String, Array}
    
    LabeledGraph(is_directed :: Bool, dictionary :: Dict) = new(is_directed, dictionary, Array(Int, (0,0)), Dict())
    LabeledGraph(is_directed :: Bool) = new(is_directed, Dict(), Array(Int, (0,0)), Dict())
end

@graph_implements LabeledGraph vertex_list vertex_map adjacency_list

function labeled_graph{V}(vs::Vector{V}, es::Vector{Vector{V}}; is_directed::Bool=true)

    g = labeled_graph{V}(is_directed,is_weighted)

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

vertex_index(v, g::LabeledGraph) = g.dictionary[v]

num_edges(g::LabeledGraph) = countnz(g.adjacency) / 2

out_degree(v, g::LabeledGraph) = countnz(g.adjacency[g.dictionary[v],:])

function out_neighbors(v, g::LabeledGraph)

    i = g.dictionary[v]
    adjacent_vertices = {}

    for w in keys(g.dictionary)
        if g.adjacency[i,g.dictionary[w]]>0
            push!(adjacent_vertices,w)
        end
    end

    return adjacent_vertices

end

## more methods

function add_vertex!(g::LabeledGraph, v)

    if !haskey(g.dictionary,v)

        n = length(g.dictionary)
        m = size(g.adjacency,1)

        g.adjacency = [g.adjacency zeros(m,1);
                    zeros(1,m)  zeros(1,1)]
        for key in keys(g.edge_properties)
            prop = g.edge_properties[key]
            g.edge_properties[key] = [prop zeros(m,1);
                                    zeros(1,m)  zeros(1,1)]
        end
        g.dictionary[v] = n+1

    end

    return g

end

function remove_vertex!(g::LabeledGraph, v)

    if haskey(g.dictionary,v)

        p = g.dictionary[v]
        n = length(g.dictionary)
        m = size(g.adjacency,1)

        g.adjacency = [g.adjacency[1:p-1,1:p-1] g.adjacency[1:p-1,p+1:m];
                    g.adjacency[p+1:m,1:p-1] g.adjacency[p+1:m,p+1:m]]
        for key in keys(g.edge_properties)
            prop = g.edge_properties[key]
            g.edge_properties[key] = [prop[1:p-1,1:p-1] prop[1:p-1,p+1:m];
                                    prop[p+1:m,1:p-1] prop[p+1:m,p+1:m]]
        end
        delete!(g.dictionary,v)
        for w in keys(g.dictionary)
            if g.dictionary[w]>p
                g.dictionary[w] -=1
            end
        end

    end

    return g

end

function add_edge!(g::LabeledGraph, u, v)

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

function remove_edge!(g::LabeledGraph, u, v)

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

function add_edge_property!{T}(g::LabeledGraph, prop_name::String, value_type::Type{T})
    sz = size(g.adjacency);
    g.edge_properties[prop_name] = zeros(value_type, sz[1], sz[2])
end

function get_edge_property(g::LabeledGraph, x, y, key::String)
    if haskey(g.edge_properties, key)
        g.edge_properties[key][g.dictionary[x], g.dictionary[y]]
    else
        throw(ArgumentError("Edge property \"$(key)\" does not exist."))
    end
end

function set_edge_property!(g::LabeledGraph, u, v, prop_name::String, val)

    p = g.dictionary[u]
    q = g.dictionary[v]
    
    if haskey(g.edge_properties, prop_name)
        prop = g.edge_properties[prop_name]
    else
        throw(ArgumentError("Edge property \"$(key)\" does not exist."))
    end
    
    if g.is_directed
        prop[p,q] = val
    else
        prop[p,q] = val
        prop[q,p] = val
    end

    return g

end
