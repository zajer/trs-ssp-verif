open Tracking_bigraph
let gen_ui2redex_map participants_iso ui2state =
    let participants_converted = Bigraph.Iso.to_list participants_iso |> Ui.make_map_of_list in
    let part_inv = Ui.inverse participants_converted in
    let result = Ui.transform_codom false ~transformed_mapping:ui2state ~codom_mapping:part_inv in
    result
let transform_ui_map trans ui2state_map first_new_ui all_states =
    match Common.is_function_injective trans.TTS.residue with
    | false -> raise (Invalid_argument "Residue is not an injective function")
    | true ->
        let residue_inverted = Bigraph.Fun.to_list trans.residue |> List.map (fun (i1,i2)-> i2,i1) |> Ui.make_map_of_list in
        let ui2state_map_updated_by_residue = Ui.transform_codom false ~transformed_mapping:ui2state_map ~codom_mapping:residue_inverted 
        and result_state = Hashtbl.find all_states trans.out_state_idx in
        let result_ui2state_map,new_first_ui_val = Common.update_mapping_with_new_objs ui2state_map_updated_by_residue trans.residue result_state.Bigraph.Big.n first_new_ui
        and result_ui2redex_map = gen_ui2redex_map trans.participants ui2state_map in
        result_ui2redex_map,result_ui2state_map,new_first_ui_val
let extract_time_change current_state trans_fun_raw = 
    let result_set, result_time_shift_opt = List.fold_left 
        (
            fun (res_is,res_ts_opt) (relative_aid,ts) -> 
                if ts <> 0 then
                    let ts_to_add = 
                        match res_ts_opt with
                        | None -> ts
                        | Some res_ts -> if res_ts = ts then res_ts else raise (Invalid_argument "Time shift for all agents must be the same")
                    and aid,_ = Array.get current_state (relative_aid-1)  in
                    Common.IntSet.add aid res_is,Some ts_to_add
                else
                    res_is,res_ts_opt
        )
        (Common.IntSet.empty,None) 
        trans_fun_raw.Ssp.State.permutation_with_time_shift in
        match result_time_shift_opt with
        | None -> raise (Invalid_argument "Transition function must change agents' time")
        | Some result_time_shift -> result_set,result_time_shift
let rec _transform_raw_walk raw_walk result_extended_walk all_states_by_idx all_trans_by_idx current_ui_map current_state first_new_ui =
    match raw_walk with
    | [] -> result_extended_walk |> List.rev
    | h::t ->
        let parsed_trans = Ssp.Template_state.parse_trans_fun h
        and time_change = extract_time_change current_state h in
        let ui2redex,new_current_ui_map,new_first_new_ui = transform_ui_map (Hashtbl.find all_trans_by_idx parsed_trans.transition_idx) current_ui_map first_new_ui all_states_by_idx in
        let new_extended_walk_elem = {Phase3.trans_fun=parsed_trans;ui2redex;ui2state=new_current_ui_map;first_new_ui=new_first_new_ui;time_change} in
            _transform_raw_walk t (new_extended_walk_elem::result_extended_walk) all_states_by_idx all_trans_by_idx new_current_ui_map (parsed_trans.func current_state) new_first_new_ui
let perform_phase raw_walk initial_ui_map num_of_agents first_new_ui all_states all_trans =
    let initial_state = Array.init num_of_agents (fun i -> i+1,0) in
    _transform_raw_walk
        raw_walk
        []
        all_states
        all_trans
        initial_ui_map
        initial_state
        first_new_ui