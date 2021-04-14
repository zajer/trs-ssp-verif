type result = {is_successful:bool; value:Phase3.constructed_state list * Phase3.time_info list; error_message:string option}
type _construciton_params = 
    {
        current_state:Phase3.constructed_state;
        usable_ewalk:Phase3.extended_walk;
        current_time:int;
        number_of_agents:int;
        all_states:(int,Bigraph.Big.t) Hashtbl.t;
        all_trans_by_idx:Phase3.mapped_trans;
        all_trans_by_key:Phase4.mapped_trans;
    }
let _update_result_value res new_val = 
    {is_successful=res.is_successful;value=new_val;error_message=res.error_message}
let _append_partial_result current_result new_state new_time_info =
    match current_result.value with
    | (vsl,vtil) -> (new_state::vsl, List.rev_append new_time_info vtil) |> _update_result_value current_result
let _update_params current_params state ewalk =
    {
        current_state=state;
        usable_ewalk=ewalk;
        current_time=current_params.current_time+1;
        number_of_agents=current_params.number_of_agents;
        all_states=current_params.all_states;
        all_trans_by_idx=current_params.all_trans_by_idx;
        all_trans_by_key=current_params.all_trans_by_key
    }
let rec _proceed_with_construction_of_result current_result params operation = 
    match current_result.is_successful with
    | false -> current_result
    | true -> 
        match params.usable_ewalk with
        | [] -> current_result
        | _ ->
            try
                let new_state,rest_of_ewalk,new_time_infos = 
                    Phase3.perform_phase
                        params.current_state
                        ~constructed_time_moment:(params.current_time+1)
                        ~num_of_agents:params.number_of_agents
                        params.usable_ewalk
                        params.all_states
                        params.all_trans_by_idx
                        params.all_trans_by_key in
                let new_current_result = _append_partial_result current_result new_state new_time_infos 
                and new_params = _update_params params new_state rest_of_ewalk  in
                    _proceed_with_construction_of_result (operation (new_state,new_time_infos) new_current_result) new_params operation  
            with
            | Phase4.Not_updateable s -> {is_successful=false;value=current_result.value;error_message=Some (s^" while constructing a state at the moment:"^(string_of_int (params.current_time+1)))}
let _initial_result init_state = {is_successful=true; value=([init_state],[]); error_message=None}
let _initial_ui_map_on_nodes ~num_of_nodes =
    List.init num_of_nodes (fun i -> i+1,i ) |> Ui.make_map_of_list
let _initial_sat_config ~num_of_agents = 
    Array.init num_of_agents (fun i-> i+1,0)
let _initial_state ~first_raw_walk_element ~num_of_agents all_states_by_idx =
    let state_idx = first_raw_walk_element.Ssp.State.from_idx in
    let state_big = Hashtbl.find all_states_by_idx state_idx in
    let state_ui_map = _initial_ui_map_on_nodes ~num_of_nodes:(Bigraph.Nodes.size state_big.Bigraph.Big.n) in
    let initial_sat_config = _initial_sat_config ~num_of_agents in
    {
        Phase3.state={Tracking_bigraph.TTS.bigraph=state_big;index=state_idx};
        ui_map=state_ui_map;
        sat_config=initial_sat_config;
        time=0
    }
let _construct_result ~initial_state ~num_of_agents ~whole_ewalk all_states_by_idx all_trans_by_idx all_trans_by_key operation =
    let initial_result = _initial_result 
    and initial_params = 
        {
            current_state=initial_state;
            usable_ewalk=whole_ewalk;
            current_time=0;
            number_of_agents=num_of_agents;
            all_states=all_states_by_idx;
            all_trans_by_idx;
            all_trans_by_key
        } in
    _proceed_with_construction_of_result (initial_result initial_state) initial_params operation
let _invert_order_of_constructed_states res =
    match res.value with
    | (vsl,vtil) -> ((List.rev vsl), vtil) |> _update_result_value res
let perform_phase raw_walk ~num_of_agents all_states_by_idx all_trans_by_idx all_trans_by_key operation =
    assert ( (List.compare_length_with raw_walk 0) > 0 );
    Ssp.Template_state._number_of_agents:=num_of_agents;
    let initial_state = _initial_state ~first_raw_walk_element:(List.hd raw_walk) ~num_of_agents all_states_by_idx in
    let ewalk = Phase2.perform_phase 
        raw_walk 
        (initial_state.ui_map) 
        ~num_of_agents 
        ~first_new_ui:(Ui.IntMappingsSet.max_elt initial_state.ui_map |> fun (i,_)->i+1 ) 
        all_states_by_idx 
        all_trans_by_idx in
    _construct_result 
        ~initial_state 
        ~num_of_agents
        ~whole_ewalk:ewalk
        all_states_by_idx all_trans_by_idx all_trans_by_key operation
    |> _invert_order_of_constructed_states

