val map_states_to_idx : Tracking_bigraph.TTS.state list -> Phase4.states_mapped_by_idx
val map_trans_to_idx : Tracking_bigraph.TTS.trans_exported list -> Phase3.trans_mapped_by_idx 
val map_trans_to_key : Tracking_bigraph.TTS.trans_exported list -> Phase4.trans_mapped_by_key 
type config_raw = {scenario_description:string; controls_2_color:(string*string) list} [@@deriving of_yojson]
type config = {directory:string; description:string; short_name:string; control2color:(string,string) Hashtbl.t}
val convert : config_raw -> string -> config
val make_visjs_timeline_config : config -> Visjs.timeline_config
val make_visjs_serialization_config : config -> Visjs.state_serialization_config
val make_output_info : config -> Phase1.output