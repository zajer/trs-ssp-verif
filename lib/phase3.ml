module IntSet = Set.Make(Int)
type extended_walk_element = {trans_fun:State_space.trans_fun;ui2redex:Ui.map;ui2input:Ui.map;first_new_ui:int;time_change:(IntSet.t*int)}
type extended_walk = extended_walk_element list
let first ewalk = 
    match ewalk with
    | [] -> raise Not_found
    | h::t -> h,t
type mapped_trans = (int,Tracking_bigraph.TTS.trans_exported) Hashtbl.t
let corr_trans all_trans trans_fun =
    let trans_id = trans_fun.State_space.transition_idx in
    Hashtbl.find all_trans trans_id
let agents_update state time_change =
    State_space.update_agents_times state time_change
let agents_in_future state current_moment = 
    State_space.agents_in_future state current_moment
type time_info = {participants:int list; react_label:string; transition_idx:int; new_objs: int list; term_objs:int list; start_time:int; end_time:int}
let extract_time_info ~ui2state_before_transition ~ui2state_after_transition ~ui2redex ~constructed_moment_of_time ~duration_of_transition ~transition_idx react_label =
    let new_objs = Ui.IntMappingsSet.inter ui2state_after_transition ui2state_before_transition |> Ui.domain
    and term_objs = Ui.IntMappingsSet.inter ui2state_before_transition ui2state_after_transition |> Ui.domain in
    {
        participants=Ui.domain ui2redex;
        react_label;
        transition_idx;
        start_time=constructed_moment_of_time;
        end_time=constructed_moment_of_time+duration_of_transition;
        new_objs;
        term_objs
    }