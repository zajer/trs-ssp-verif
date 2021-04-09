module IntSet = Set.Make(Int)
let is_function_injective f = 
    let codom = Bigraph.Fun.codom f 
    and fun_as_list = Bigraph.Fun.to_list f in
    if Bigraph.IntSet.cardinal codom = List.length fun_as_list then
        true
    else
        false
let update_mapping_with_new_objs ui_to_update residue output_state new_ui = 
    let dom_of_residue = Bigraph.Fun.dom residue in
    let nodes_unmapped_by_residue,num_of_unmapped_nodes = 
        Bigraph.Nodes.fold 
        (fun i _ (unmapped_nodes,num_of_unmapped_nodes) -> 
            if not (Bigraph.IntSet.mem i dom_of_residue) then (i::unmapped_nodes),num_of_unmapped_nodes+1 else unmapped_nodes,num_of_unmapped_nodes 
        ) 
        output_state 
        ([],0) in
    let result = List.mapi (fun i unmapped_node_id -> new_ui+i, unmapped_node_id) nodes_unmapped_by_residue in
    let mapping_of_new_elems = Ui.make_map_of_list result in
    Ui.union ~base:ui_to_update ~extension:mapping_of_new_elems,num_of_unmapped_nodes+new_ui