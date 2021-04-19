type state_serialization_config = {directory:string;file_prefix:string; control2shape:(string,string)Hashtbl.t; control2color:(string,string) Hashtbl.t}
type node = {id:int;label:string;shape:string;color:string}
[@@deriving yojson, show]
type edge = { from: int; to_:int[@key "to"]; arrows: string }
[@@deriving yojson, show]
type network_data = { nodes : node list; edges:edge list}
[@@deriving yojson, show]
let _default_string_fun = fun str_opt -> match str_opt with | None -> "" | Some s -> s
let _big_node_2_node config index (ctrl:Bigraph.Ctrl.t) =
    let ctrl_str = Bigraph.Ctrl.name ctrl in
    let color = Hashtbl.find_opt config.control2color ctrl_str |> _default_string_fun
    and shape = Hashtbl.find_opt config.control2shape ctrl_str |> _default_string_fun in
    {id=index;label=(string_of_int index)^":"^ctrl_str;color;shape}
let _state_2_nodes config bigraph = 
    Bigraph.Nodes.fold (fun idx ctrl res -> let part_res = _big_node_2_node config idx ctrl in part_res::res ) (bigraph.Bigraph.Big.n) []
let _big_link_2_edge ~from_idx ~to_idx arrows =
    { from=from_idx; to_=to_idx; arrows }
let _place_graph_2_edges bigraph = 
    Bigraph.Sparse.fold (fun from_idx to_idx res -> _big_link_2_edge ~from_idx ~to_idx "to" :: res) bigraph.Bigraph.Big.p.nn []
let _link_graph_edge_2_edge edge =
    Bigraph.Link.Ports.fold 
        (
            fun from_idx to_idx res -> _big_link_2_edge ~from_idx ~to_idx "" :: res 
        ) 
        edge.Bigraph.Link.p 
        []
let hyperedge_node_tag = "!@#HYPEREDGE!@#"
let _hyper_edge_2_node config node_id =
    let color = Hashtbl.find_opt config.control2color hyperedge_node_tag |> _default_string_fun
    and shape = Hashtbl.find_opt config.control2shape hyperedge_node_tag |> _default_string_fun in
    {id=node_id;label="";color;shape}
let _link_graph_edge_2_edges_and_node config edge ~max_node_id =
    let num_of_connections = Bigraph.Link.Ports.cardinal edge.Bigraph.Link.p in
    let res_edgs =
        if num_of_connections > 2 then (* many-many *)
            Bigraph.Link.Ports.fold 
            (fun from_idx _ res_edgs -> (_big_link_2_edge ~from_idx ~to_idx:(max_node_id+1) "")::res_edgs) 
            edge.Bigraph.Link.p
            []
            else (* 1-1 connection *)
                let nodes_to_connect = 
                    Bigraph.Link.Ports.fold 
                    (fun from_idx _ res -> from_idx::res )
                    edge.Bigraph.Link.p
                    [] in
                [_big_link_2_edge ~from_idx:(List.nth nodes_to_connect 0) ~to_idx:(List.nth nodes_to_connect 1) ""]
    in
    if num_of_connections > 2 then
        res_edgs,Some (_hyper_edge_2_node config (max_node_id+1))
    else
        res_edgs,None
let _link_graph_2_edges bigraph =
    Bigraph.Link.Lg.fold (fun edge res -> _link_graph_edge_2_edge edge :: res ) bigraph.Bigraph.Big.l [] |> List.flatten
let _link_graph_2_nodes_and_edges config bigraph max_node_id =
    let current_max_node_id = ref max_node_id in
    let res_edges,res_nodes = 
        Bigraph.Link.Lg.fold 
            (
                fun edge (res_edgs,res_nds) -> 
                    let edgs,node_opt = _link_graph_edge_2_edges_and_node config edge ~max_node_id:!current_max_node_id in
                    match node_opt with
                    | None -> edgs::res_edgs,res_nds
                    | Some node -> 
                        current_max_node_id := !current_max_node_id+1;
                        edgs::res_edgs,node::res_nds            
            ) 
            bigraph.Bigraph.Big.l 
            ([],[])
    in
    (List.flatten res_edges),res_nodes 
let _state_2_edges bigraph =
    let edges_from_place_graph = _place_graph_2_edges bigraph
    and edges_from_link_graph = _link_graph_2_edges bigraph in
    List.append edges_from_place_graph edges_from_link_graph
let _state_2_edges_and_nodes config bigraph max_node_id =
    let edges_from_place_graph = _place_graph_2_edges bigraph
    and edges_from_link_graph,nodes_from_link_graph = _link_graph_2_nodes_and_edges config bigraph max_node_id in
    List.append edges_from_place_graph edges_from_link_graph,nodes_from_link_graph
let bigraph_2_network config bigraph = 
    let nodes = _state_2_nodes config bigraph in
    let edges,nodes2 = _state_2_edges_and_nodes config bigraph ((List.length nodes)-1) in
    {nodes=nodes@nodes2;edges}
let _network_to_json = [%yojson_of: network_data]
type state_serialized_raw = {sat_config:Ssp.Template_state.t;network:network_data;ui_map:Ui.map}
type state_serialized = {sat_config:(int*int)array;network_data:string[@key "network_data_file"];ui_map:string}
[@@deriving yojson, show]
let network_filename config time_moment =
    config.directory^"\\"^config.file_prefix^"-network-T:"^(string_of_int time_moment)^".json"
let state_serialized_filename config time_moment = 
    config.directory^"\\"^config.file_prefix^"-state-T:"^(string_of_int time_moment)^".json"
let _raw_2_exported (ssr:state_serialized_raw) (config:state_serialization_config) time_moment =
    let network_filename = network_filename config time_moment in
        {sat_config = ssr.sat_config;network_data=network_filename;ui_map=Ui.map_to_string ssr.ui_map}
let _make_ssr sat net uim =
    {network=net;sat_config=sat;ui_map=uim}
let transformer_save_state config (part_res_cs,_) current_result = 
    let filename = state_serialized_filename config part_res_cs.Phase3.time 
    and network_filename = network_filename config part_res_cs.Phase3.time 
    and network = bigraph_2_network config part_res_cs.state.bigraph in
    let content_raw = _make_ssr part_res_cs.sat_config network part_res_cs.ui_map in
    let network_json = [%yojson_of: network_data] network 
    and content_json = (_raw_2_exported content_raw config part_res_cs.time) |> [%yojson_of: state_serialized]  in
    Yojson.Safe.to_file filename content_json;
    Yojson.Safe.to_file network_filename network_json ;
    current_result
let tj = [%yojson_of: network_data] 