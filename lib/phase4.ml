open Tracking_bigraph
type mapped_reacts = (int,Tracking_bigraph.TTS.trans_exported) Hashtbl.t
let is_update_with_trans_possible ~ui2state_map trans ~ui2par_map =
    let codom_mapping = Ui.make_map_of_list (Bigraph.Iso.to_list trans.TTS.participants) in
    let ui2state_map_from_par = Ui.transform_codom false ~transformed_mapping:ui2par_map ~codom_mapping in
    Ui.is_subset ~subset:ui2state_map_from_par ~target:ui2state_map
type mapped_trans = ((int*string),TTS.trans_exported) Hashtbl.t
let rec _find_trans_allowing_for_update ~ui2state_map ~ui2par_map trans_with_same_key =
    match trans_with_same_key with 
    | [] -> false, None
    | h::t -> 
        if is_update_with_trans_possible ~ui2state_map ~ui2par_map h then
            true,Some h
        else
            _find_trans_allowing_for_update ~ui2state_map ~ui2par_map t
let is_update_possible state ~ui2state_map ~ui2par_map trans_to_be_applied all_trans = 
    let key = state.TTS.index,trans_to_be_applied.TTS.react_label in
    let known_similar_transitions = Hashtbl.find_all all_trans key in
    _find_trans_allowing_for_update ~ui2state_map ~ui2par_map known_similar_transitions
(*let _is_function_injective f = 
    let codom = Bigraph.Fun.codom f 
    and fun_as_list = Bigraph.Fun.to_list f in
    if Bigraph.IntSet.cardinal codom = List.length fun_as_list then
        true
    else
        false
let _update_mapping_with_new_objs ui_to_update residue output_state new_ui = 
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
    Ui.union ~base:ui_to_update ~extension:mapping_of_new_elems,num_of_unmapped_nodes+new_ui*)
let update ui2state_map trans all_states first_new_ui = 
    match Common.is_function_injective trans.TTS.residue with
    | false -> raise (Invalid_argument "Residue is not an injective function")
    | true -> 
        let residue_inverted = Bigraph.Fun.to_list trans.residue |> List.map (fun (i1,i2)-> i2,i1) |> Ui.make_map_of_list in
        let ui2state_map_updated_by_residue = Ui.transform_codom false ~transformed_mapping:ui2state_map ~codom_mapping:residue_inverted 
        and result_state = {TTS.bigraph=(Hashtbl.find all_states trans.out_state_idx);index=trans.out_state_idx} in
        let result_ui2state_map,new_first_ui_val = Common.update_mapping_with_new_objs ui2state_map_updated_by_residue trans.residue result_state.bigraph.n first_new_ui in
        {TTS.bigraph=(Hashtbl.find all_states trans.out_state_idx);index=trans.out_state_idx},result_ui2state_map,new_first_ui_val
exception Not_updateable of string
let perform_phase ~current_state ~current_state_mapping ~previous_state ~previous_state_mapping ~mapping_on_redex trans_on_walk first_new_ui all_states all_trans =
    let cond1,_ = is_update_possible previous_state ~ui2state_map:previous_state_mapping ~ui2par_map:mapping_on_redex trans_on_walk all_trans in
    match cond1 with
    | false -> raise (Not_updateable "Previous state is not updateable")
    | true -> 
        let cond2,trans_to_apply_opt = is_update_possible current_state ~ui2state_map:current_state_mapping ~ui2par_map:mapping_on_redex trans_on_walk all_trans in
        match cond2 with
        | false -> raise (Not_updateable "Currently constructed state is not updateable")
        | true -> 
            let trans_to_apply = Option.get trans_to_apply_opt in
            update current_state_mapping trans_to_apply all_states first_new_ui
