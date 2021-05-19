type result = {is_successful:bool; value:Phase3.constructed_state list * Phase3.time_info list; error_message:string option}
type output = {name:string;mutable is_scenario_valid:bool;mutable message:string option;directory:string;timeline_filename:string; groups_filename:string; states_regex:string} [@@deriving yojson_of]
val output_filename : string -> string
val output_files_regex : unit -> string
module ResultTransformer : sig
    type t = (Phase3.constructed_state * Phase3.time_info list) option
    type o = t -> result -> result
    
    val stack : o -> o -> o
    val apply : t -> result -> o -> result
end
module BasicTransformers : sig
    val disqualify_results_if_pattern_detected : Ssp_bridge.Patterns.pattern list -> ResultTransformer.t -> result -> result
    val disqualify_results_if_scenario_takes_too_long : int -> ResultTransformer.t -> result -> result
    val save_or_update_output : output -> string -> ResultTransformer.t -> result -> result
end
val perform_phase : 
    Ssp.State.trans_fun_raw list ->
    num_of_agents:int ->
    Phase4.states_mapped_by_idx ->
    Phase3.trans_mapped_by_idx ->
    Phase4.trans_mapped_by_key ->
    ResultTransformer.o ->
    result
