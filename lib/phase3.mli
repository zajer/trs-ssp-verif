type extended_walk_element = {trans_fun:State_space.trans_fun;ui2redex:Ui.map;ui2state:Ui.map;first_new_ui:int;time_change:(Common.IntSet.t*int)}
type extended_walk = extended_walk_element list
type trans_mapped_by_idx = (int,Tracking_bigraph.TTS.trans_exported) Hashtbl.t
type time_info = {participants:int list; react_label:string; transition_idx:int; new_objs: int list; term_objs:int list; start_time:int; end_time:int}
type constructed_state = {state:Tracking_bigraph.TTS.state;ui_map:Ui.map;sat_config:Ssp.Template_state.t;time:int}
val corr_trans : trans_mapped_by_idx -> State_space.trans_fun -> Tracking_bigraph.TTS.trans_exported
val agents_update : State_space.sat -> Common.IntSet.t * int -> State_space.sat
val agents_in_future : State_space.sat -> int -> Common.IntSet.t
val extract_time_info : 
    ui2state_before_transition:Ui.map ->
    ui2state_after_transition:Ui.map ->
    ui2redex:Ui.map ->
    start_time:int ->
    duration_of_transition:int ->
    transition_idx:int ->
    string ->
    time_info
val state2semi_state : constructed_state -> Phase4.constructed_semi_state
val semi_state2state : Phase4.constructed_semi_state -> State_space.sat -> int -> constructed_state
val perform_phase :
        constructed_state ->
        extended_walk ->
        constructed_time_moment:int ->
        num_of_agents:int ->
        Phase4.states_mapped_by_idx ->
        trans_mapped_by_idx ->
        Phase4.trans_mapped_by_key ->
        constructed_state * extended_walk * time_info list 