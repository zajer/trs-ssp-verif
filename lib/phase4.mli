type trans_mapped_by_key = ((int*string),Tracking_bigraph.TTS.trans_exported) Hashtbl.t
type states_mapped_by_idx = (int,Bigraph.Big.t) Hashtbl.t
type constructed_semi_state = {state:Tracking_bigraph.TTS.state;ui_map:Ui.map}
type transition_with_ui = {transition:Tracking_bigraph.TTS.trans_exported;ui_map_on_redex:Ui.map}
exception Not_updateable of string
val is_update_with_trans_possible : ui2state_map:Ui.map -> Tracking_bigraph.TTS.trans_exported -> ui2par_map:Ui.map -> bool
val is_update_possible : 
    Tracking_bigraph.TTS.state -> 
    ui2state_map:Ui.map -> 
    ui2par_map:Ui.map -> 
    Tracking_bigraph.TTS.trans_exported -> 
    trans_mapped_by_key -> 
    bool * Tracking_bigraph.TTS.trans_exported option
val update : 
    Ui.map -> 
    Tracking_bigraph.TTS.trans_exported -> 
    states_mapped_by_idx -> 
    int -> 
    constructed_semi_state * int
val perform_phase : 
    current_state:constructed_semi_state -> 
    previous_state:constructed_semi_state -> 
    transition_with_ui -> 
    int -> 
    states_mapped_by_idx -> 
    trans_mapped_by_key -> 
    constructed_semi_state * int