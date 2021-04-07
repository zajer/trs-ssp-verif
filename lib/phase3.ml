module IntSet = Set.Make(Int)
type extended_walk_element = {trans_fun:State_space.trans_fun;ui2redex:Ui.map;ui2state:Ui.map;first_new_ui:int;time_change:(IntSet.t*int)}
type extended_walk = extended_walk_element list
(*let first ewalk = 
    match ewalk with
    | [] -> raise Not_found
    | h::t -> h,t*)
type mapped_trans = (int,Tracking_bigraph.TTS.trans_exported) Hashtbl.t
let corr_trans all_trans trans_fun =
    let trans_id = trans_fun.Ssp.Template_state.transition_idx in
    Hashtbl.find all_trans trans_id
let agents_update sat_config time_change =
    State_space.update_sat sat_config time_change
let agents_in_future state current_moment = 
    State_space.elements_in_future state current_moment
type time_info = {participants:int list; react_label:string; transition_idx:int; new_objs: int list; term_objs:int list; start_time:int; end_time:int}
let extract_time_info ~ui2state_before_transition ~ui2state_after_transition ~ui2redex ~constructed_moment_of_time ~duration_of_transition ~transition_idx react_label =
    let new_objs = Ui.IntMappingsSet.diff ui2state_after_transition ui2state_before_transition |> Ui.domain
    and term_objs = Ui.IntMappingsSet.diff ui2state_before_transition ui2state_after_transition |> Ui.domain in
    {
        participants=Ui.domain ui2redex;
        react_label;
        transition_idx;
        start_time=constructed_moment_of_time;
        end_time=constructed_moment_of_time+duration_of_transition;
        new_objs;
        term_objs
    }
let rec _construct_state 
    ~previous_state 
    ~ui2previous_state 
    ~current_state 
    ~ui2current_state 
    ~usable_ewalk 
    ~unused_ewalk 
    ommited_agents 
    ~previous_sat_config 
    ~current_sat_config 
    time_moment 
    num_of_agents 
    all_states 
    all_trans_idx 
    all_trans_by_keys 
    time_flow =
    match usable_ewalk with 
    | [] -> List.rev unused_ewalk,current_state,ui2current_state,current_sat_config,time_flow
    | h::t -> 
        if IntSet.cardinal ommited_agents = num_of_agents then 
            List.rev_append unused_ewalk usable_ewalk,current_state,ui2current_state,current_sat_config,time_flow
        else
            let involved_agents,time_needed = h.time_change in
            if not (IntSet.inter involved_agents ommited_agents |> IntSet.is_empty) then
                _construct_state 
                    ~previous_state 
                    ~ui2previous_state 
                    ~current_state 
                    ~ui2current_state 
                    ~usable_ewalk:t 
                    ~unused_ewalk:(h::unused_ewalk) 
                    ommited_agents 
                    ~previous_sat_config 
                    ~current_sat_config 
                    time_moment 
                    num_of_agents
                    all_states 
                    all_trans_idx 
                    all_trans_by_keys 
                    time_flow
            else
                let current_sat_config_after_transition = agents_update current_sat_config h.time_change in
                let set_of_agents_in_future = agents_in_future current_sat_config_after_transition time_moment in
                let num_of_agents_in_future = IntSet.cardinal set_of_agents_in_future in
                if not (num_of_agents_in_future = 0) then
                    _construct_state 
                        ~previous_state 
                        ~ui2previous_state 
                        ~current_state 
                        ~ui2current_state 
                        ~usable_ewalk:t 
                        ~unused_ewalk:(h::unused_ewalk) 
                        (IntSet.union ommited_agents set_of_agents_in_future) 
                        ~previous_sat_config 
                        ~current_sat_config 
                        time_moment 
                        num_of_agents
                        all_states 
                        all_trans_idx 
                        all_trans_by_keys 
                        time_flow
                else
                    let trans_to_be_applied = corr_trans all_trans_idx h.trans_fun in
                    let new_constructed_state,new_constructed_state_mapping,_ = 
                        Phase4.perform_phase 
                            ~current_state
                            ~current_state_mapping:ui2current_state 
                            ~previous_state 
                            ~previous_state_mapping:ui2previous_state 
                            ~mapping_on_redex:h.ui2redex 
                            trans_to_be_applied 
                            h.first_new_ui
                            all_states
                            all_trans_by_keys in
                    let tinfo = extract_time_info 
                                    ~ui2state_before_transition:ui2current_state 
                                    ~ui2state_after_transition:new_constructed_state_mapping 
                                    ~ui2redex:h.ui2redex 
                                    ~constructed_moment_of_time:time_moment 
                                    ~duration_of_transition:time_needed
                                    ~transition_idx:h.trans_fun.transition_idx
                                    trans_to_be_applied.react_label in
                    _construct_state 
                        ~previous_state 
                        ~ui2previous_state 
                        ~current_state:new_constructed_state
                        ~ui2current_state:new_constructed_state_mapping
                        ~usable_ewalk:t 
                        ~unused_ewalk
                        ommited_agents
                        ~previous_sat_config 
                        ~current_sat_config:current_sat_config_after_transition
                        time_moment 
                        num_of_agents
                        all_states 
                        all_trans_idx 
                        all_trans_by_keys 
                        (tinfo::time_flow)
let perform_phase 
        previous_state 
        ui2previous_state 
        previous_sat_config 
        ewalk
        constructed_time_moment 
        num_of_agents 
        all_states 
        all_trans_by_idx 
        all_trans_by_keys =
            _construct_state 
                ~previous_state 
                ~ui2previous_state 
                ~current_state:previous_state
                ~ui2current_state:ui2previous_state
                ~usable_ewalk:ewalk
                ~unused_ewalk:[]
                IntSet.empty
                ~previous_sat_config 
                ~current_sat_config:previous_sat_config
                constructed_time_moment 
                num_of_agents 
                all_states 
                all_trans_by_idx 
                all_trans_by_keys 
                []