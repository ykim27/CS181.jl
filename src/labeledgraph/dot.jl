
# Write the dot representation of a graph to a stream.
# Temporary solution for CS181. TODO: clean this up.
function to_dot(graph::LabeledGraph, stream::IO, with_weight::String="")
    has_vertex_attrs = method_exists(attributes, (vertex_type(graph), LabeledGraph))
    has_edge_attrs = method_exists(attributes, (edge_type(graph), LabeledGraph))
    
    write(stream, "$(graph_type_string(graph)) graphname {\n")
    if implements_edge_list(graph)
        for edge in edges(graph)
            write(stream,"$(vertex_index(source(edge), graph)) $(edge_op(graph)) $(vertex_index(target(edge), graph))\n")
        end
    elseif implements_vertex_list(graph) && (implements_incidence_list(graph) || implements_adjacency_list(graph))
        for vertex in vertices(graph)
	    if out_degree(vertex,graph) == 0
	        write(stream,"$(vertex);\n")
	    end
            for n in out_neighbors(vertex, graph)
                if is_directed(graph) || vertex_index(n, graph) > vertex_index(vertex, graph)
                    write(stream,"$(vertex) $(edge_op(graph)) $(n)")
                    if with_weight != ""
                        weight = get_edge_property(graph,vertex,n,with_weight)
                        write(stream," [ label=\"$(weight)\" ]")
                    end
                    write(stream, ";\n")
                end
            end
        end
    else
        throw(ArgumentError("More graph Concepts needed: dot serialization requires iteration over edges or iteration over vertices and neighbors."))
    end
    write(stream, "}\n")
    stream
end

function graph_type_string(graph::AbstractGraph)
    is_directed(graph) ? "digraph" : "graph"
end

function edge_op(graph::AbstractGraph)
    is_directed(graph) ? "->" : "--"
end

# Parse a LabeledGraph from a DOT stream.
function from_dot(stream::IO, label_type:DataType=String)
    graph = LabeledGraph{String}(false);
    add_edge_property!(graph,"WEIGHT",Float64);

    in_graph = false;
    for line in eachline(stream)
        line = strip(line);
        if(length(line) == 0)
            continue;
        end
        if(!in_graph)
            in_graph = true;
            continue;
        end
        if(line[1] == '}')
            continue;
        end
	m=match(r"(\w+)?\s+?--?\s+?(\w+)\s*\[\s*label\s*=\s*\"(.*?)\"\s*\]",line)
	# old regex
        # m=match(r"(\w+)?\s+?--?\s+?(\w+)",line)
        from = m.captures[1];
        to = m.captures[2];
	weight = m.captures[3];
	if label_type <: Number
	   weight = parse(weight);
	end
        add_edge!(graph,from,to);
	set_edge_property!(graph,from,to,"WEIGHT",weight);
    end

    return graph;
end