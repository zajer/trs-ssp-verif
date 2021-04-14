type result = {is_successful:bool; value:Phase3.constructed_state list * Phase3.time_info list; error_message:string option}
val perform_phase : 
    Ssp.State.trans_fun_raw list ->
    num_of_agents:int ->
    Phase4.states_mapped_by_idx ->
    Phase3.trans_mapped_by_idx ->
    Phase4.trans_mapped_by_key ->
    (Phase3.constructed_state * Phase3.time_info list -> result -> result ) ->
    result