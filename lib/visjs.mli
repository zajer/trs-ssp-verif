type state_serialization_config = {directory:string;file_prefix:string; control2shape:(string,string)Hashtbl.t; control2color:(string,string) Hashtbl.t}
type node = {id:int;label:string;shape:string;color:string}
[@@deriving yojson, show]
type edge = { from: int; to_:int[@key "to"]; arrows: string }
[@@deriving yojson, show]
type network_data = { nodes : node list; edges:edge list}
[@@deriving yojson, show]
val hyperedge_node_tag : string
val bigraph_2_network : state_serialization_config -> Bigraph.Big.t -> network_data
type state_serialized_raw = {sat_config:Ssp.Template_state.t;network:network_data;ui_map:Ui.map}
type state_serialized = {sat_config:(int*int)array;network_data:string[@key "network_data_file"];ui_map:string}
[@@deriving yojson, show]
val network_filename : state_serialization_config -> int -> string
val state_serialized_filename : state_serialization_config -> int -> string
val transformer_save_state : state_serialization_config -> Phase1.ResultTransformer.t -> Phase1.result -> Phase1.result 
type timeline_item = { start_time: int[@key "start"]; end_time: int[@key "end"]; content: string; style:string; group:int}
[@@deriving yojson, show]
type timeline = timeline_item list
[@@deriving yojson, show]
type timeline_config = {directory:string;name:string;mutable known_objects:Common.IntSet.t;mutable current_timeline:timeline;}
type group = {id:int;content:string}
[@@deriving yojson, show]
val timeline_filename : timeline_config -> string
val set_of_participants_2_groups : Common.IntSet.t -> group list
val groups_filename : timeline_config -> string
val time_infos_2_timeline : Phase3.time_info list -> timeline
val transformer_save_timeline : timeline_config -> Phase1.ResultTransformer.t -> Phase1.result -> Phase1.result