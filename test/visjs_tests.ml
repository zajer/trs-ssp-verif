open OUnit2
open Ssp_verification

let test_constructed_state_2_network_1 _ =
    let bigraph = "{(0, X:0),(1, Y:0),(2, Z:0)}\n0 3 0\n000\n000\n000" |> Bigraph.Big.of_string
    and config = {Visjs.directory="";file_prefix="test_state"; control2shape=Hashtbl.create 0; control2color= Hashtbl.create 0} in
    let result = Visjs.bigraph_2_network config bigraph
    and expected_result = 
        {
            Visjs.nodes=
                [
                    {Visjs.id=2;label="2:Z";color="";shape=""};
                    {Visjs.id=1;label="1:Y";color="";shape=""};
                    {Visjs.id=0;label="0:X";color="";shape=""}
                ];
            edges=[]
        } in
    assert_equal
        ~msg:"Constructed network is not equal to expected"
        ~printer:[%derive.show: Visjs.network_data]
        expected_result
        result
let test_constructed_state_2_network_2 _ =
    let bigraph = "{(0, X:0),(1, Y:0),(2, Z:0)}\n0 3 0\n010\n001\n000" |> Bigraph.Big.of_string
    and config = {Visjs.directory="";file_prefix="test_state"; control2shape=Hashtbl.create 0; control2color= Hashtbl.create 0} in
    let result = Visjs.bigraph_2_network config bigraph
    and expected_result = 
        {
            Visjs.nodes=
                [
                    {Visjs.id=2;label="2:Z";color="";shape=""};
                    {Visjs.id=1;label="1:Y";color="";shape=""};
                    {Visjs.id=0;label="0:X";color="";shape=""}
                ];
            edges=
                [
                    {Visjs.from=1; to_=2;arrows="to"};
                    {Visjs.from=0; to_=1;arrows="to"}
                ]
        } in
    assert_equal
        ~msg:"Constructed network is not equal to expected"
        ~printer:[%derive.show: Visjs.network_data]
        expected_result
        result
let test_constructed_state_2_network_3 _ =
    let bigraph = "{(0, X:1),(1, Y:1),(2, Z:1),(3, W:1)}\n0 4 0\n0100\n0010\n0000\n0000\n({}, {}, {(0, 1), (1, 1), (2, 1)})\n({}, {}, {(2, 1), (3, 1)})" |> Bigraph.Big.of_string
    and config = {Visjs.directory="";file_prefix="test_state"; control2shape=Hashtbl.create 0; control2color= Hashtbl.create 0} in
    let result = Visjs.bigraph_2_network config bigraph
    and expected_result = 
        {
            Visjs.nodes=
                [
                    {Visjs.id=3;label="3:W";color="";shape=""};
                    {Visjs.id=2;label="2:Z";color="";shape=""};
                    {Visjs.id=1;label="1:Y";color="";shape=""};
                    {Visjs.id=0;label="0:X";color="";shape=""};
                    {Visjs.id=4;label="";color="";shape=""};
                ];
            edges=
                [
                    {Visjs.from=1; to_=2;arrows="to"};
                    {Visjs.from=0; to_=1;arrows="to"};
                    {Visjs.from=3; to_=2;arrows=""};
                    {Visjs.from=2; to_=4;arrows=""};
                    {Visjs.from=1; to_=4;arrows=""};
                    {Visjs.from=0; to_=4;arrows=""}
                ]
        } in
    assert_equal
        ~msg:"Constructed network is not equal to expected"
        ~printer:[%derive.show: Visjs.network_data]
        expected_result
        result
let test_save_state_as_network ctx =
    let tmp_dir_name = bracket_tmpdir ctx
    and ui_map_raw = [|(0,7);(1,5);(2,12)|] in
    let bigraph = "{(0, X:1),(1, Y:1),(2, Z:1),(3, W:1)}\n0 4 0\n0100\n0010\n0000\n0000\n({}, {}, {(0, 1), (1, 1), (2, 1)})\n({}, {}, {(2, 1), (3, 1)})" |> Bigraph.Big.of_string 
    and sat_config = [|(1,2);(2,3)|]
    and ui_map = Ui.make_map_of_list (Array.to_list ui_map_raw)
    and config = {Visjs.directory=tmp_dir_name; file_prefix="test_state"; control2shape=Hashtbl.create 0; control2color= Hashtbl.create 0} 
    and time_moment = 777 in 
    let network = Visjs.bigraph_2_network config bigraph 
    and input_result = {Phase1.is_successful=true;value=[],[];error_message=None} 
    and input_state = {Phase3.state={Tracking_bigraph.TTS.bigraph;index=777};ui_map;sat_config;time=time_moment} in
    let result = Visjs.transformer_save_state config (Some (input_state,[])) input_result 
    and expected_result = input_result in
    let saved_state = Yojson.Safe.from_file (Visjs.state_serialized_filename config time_moment) |> [%of_yojson: Visjs.state_serialized] 
    and saved_network = Yojson.Safe.from_file (Visjs.network_filename config time_moment) |> [%of_yojson: Visjs.network_data]  in
    assert_equal
        ~msg:"Returned result should not be altered"
        expected_result
        result;
    assert_equal
        ~msg:"Saved state not equal to expected"
        ~printer:[%show: Visjs.state_serialized]
        {Visjs.sat_config;network_data=(Visjs.network_filename config time_moment);ui_map=ui_map_raw}
        saved_state;
    assert_equal
        ~msg:"Saved network not equal to expected"
        ~printer:[%show: Visjs.network_data]
        network
        saved_network
let _cmp_timeline_items tl1i tl2i =
    tl1i.Visjs.start_time = tl2i.Visjs.start_time && tl1i.end_time = tl2i.end_time && tl1i.content = tl2i.content && tl1i.group = tl2i.group
let _cmp_groups g1 g2 =
    g1.Visjs.id = g2.Visjs.id
let test_save_timeline_1 ctx = 
    let tmp_dir_name = bracket_tmpdir ctx in
    let config = {Visjs.directory=tmp_dir_name;name="test_timeline";known_objects=Common.IntSet.empty;current_timeline=[];} in
    let time_info_1 = {Phase3.participants=[1;2;3]; react_label="r1"; transition_idx=3; new_objs=[]; term_objs=[]; start_time=3; end_time=4}
    and time_info_2 = {Phase3.participants=[2;4]; react_label="r2"; transition_idx=4; new_objs=[1]; term_objs=[5]; start_time=4; end_time=5} in
    let input_result = {Phase1.is_successful=true;value=[],[];error_message=None} 
    and input_state = {Phase3.state={Tracking_bigraph.TTS.bigraph=Bigraph.Big.zero;index=777};ui_map=Ui.empty_map;sat_config=[||];time=789} 
    and input_time_infos = [time_info_1;time_info_2] in
    let result = Visjs.transformer_save_timeline config (Some (input_state,input_time_infos)) input_result 
    and expected_result = input_result
    and expected_timeline = Visjs.time_infos_2_timeline [time_info_1;time_info_2]
    and expected_groups = Visjs.set_of_participants_2_groups (Common.IntSet.of_list [1;2;3;4]) in
    let result_timeline = Yojson.Safe.from_file (Visjs.timeline_filename config) |> [%of_yojson: Visjs.timeline]
    and result_groups = Yojson.Safe.from_file (Visjs.groups_filename config) |> [%of_yojson: Visjs.group list] in
    assert_equal
        ~msg:"Result should not be altered"
        expected_result
        result;
    assert_equal
        ~msg:"Saved timeline is not equal to expected"
        ~printer:[%show: Visjs.timeline]
        ~cmp:(fun tl1 tl2 -> List.for_all2 (fun tl1i tl2i -> _cmp_timeline_items tl1i tl2i ) tl1 tl2 )
        expected_timeline
        result_timeline;
    assert_equal
        ~msg:"Saved groups are not equal to expected"
        ~printer:[%show: Visjs.group list]
        ~cmp:(fun gs1 gs2 -> List.for_all2 (fun g1 g2 -> _cmp_groups g1 g2) gs1 gs2)
        expected_groups
        result_groups;
    assert_equal
        ~msg:"Known objects in config are not equal to expected"
        ~cmp:Common.IntSet.equal
        (Common.IntSet.of_list [1;2;3;4] )
        config.known_objects
let test_save_timeline_2 ctx = 
    let tmp_dir_name = bracket_tmpdir ctx in
    let time_info_new = {Phase3.participants=[1;2;3]; react_label="r1"; transition_idx=3; new_objs=[]; term_objs=[]; start_time=3; end_time=4}
    and time_info_old = {Phase3.participants=[4]; react_label="r2"; transition_idx=4; new_objs=[1]; term_objs=[5]; start_time=4; end_time=5} in
    let input_result = {Phase1.is_successful=true;value=[],[];error_message=None} 
    and input_state = {Phase3.state={Tracking_bigraph.TTS.bigraph=Bigraph.Big.zero;index=777};ui_map=Ui.empty_map;sat_config=[||];time=789} 
    and current_timeline = [time_info_old] |> Visjs.time_infos_2_timeline
    and input_time_infos = [time_info_new] in
    let config = {Visjs.directory=tmp_dir_name;name="test_timeline";known_objects=Common.IntSet.of_list [1;2;3;4];current_timeline;} in
    let result = Visjs.transformer_save_timeline config (Some (input_state,input_time_infos)) input_result 
    and expected_result = input_result
    and expected_timeline = Visjs.time_infos_2_timeline [time_info_new;time_info_old] in
    let result_timeline = Yojson.Safe.from_file (Visjs.timeline_filename config) |> [%of_yojson: Visjs.timeline] in
    assert_equal
        ~msg:"Result should not be altered"
        expected_result
        result;
    assert_equal
        ~msg:"Saved timeline is not equal to expected"
        ~printer:[%show: Visjs.timeline]
        ~cmp:(fun tl1 tl2 -> List.for_all2 (fun tl1i tl2i -> _cmp_timeline_items tl1i tl2i ) tl1 tl2 )
        expected_timeline
        result_timeline;
    assert_equal
        ~msg:"Timeline kept in config is not equal to expected"
        ~printer:[%show: Visjs.timeline]
        ~cmp:(fun tl1 tl2 -> List.for_all2 (fun tl1i tl2i -> _cmp_timeline_items tl1i tl2i ) tl1 tl2 )
        expected_timeline
        config.current_timeline;
    assert_equal
        ~msg:"There should be no file with groups"
        false
        (Sys.file_exists (Visjs.groups_filename config))
let suite =
    "Visjs" >::: [
        "State conversion test 1">:: test_constructed_state_2_network_1;
        "State conversion test 2">:: test_constructed_state_2_network_2;
        "State conversion test 3">:: test_constructed_state_2_network_3;
        "Saving state">:: test_save_state_as_network;
        "Saving timeline 1">:: test_save_timeline_1;
        "Saving timeline 2 - some groups and timeline items are already saved">:: test_save_timeline_2;
    ]

let () =
  run_test_tt_main suite