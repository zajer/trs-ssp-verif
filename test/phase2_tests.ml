open OUnit2
open Ssp_verification

let test_gen_ui2redex _ =
    let part_iso = Bigraph.Iso.of_list [(0,0);(1,1)]
    and ui2state = Ui.make_map_of_list [(1,0);(3,3);(2,2);(4,1)] in
    let result = Phase2.gen_ui2redex_map part_iso ui2state
    and expected_result = Ui.make_map_of_list [(1,0);(4,1)] in
    assert_equal
        ~msg:"Result map is not equal to expected"
        ~cmp:Ui.are_equal
        expected_result
        result;;
Ssp.Template_state._number_of_agents :=4
let test_time_change_1 _ =
    let trans_fun_raw = {Ssp.State.permutation_with_time_shift=[(1,1);(3,0);(2,0);(4,1)]; react_label=""; from_idx=(-1); to_idx=(-1); transition_idx=(-1)}
    and initial_state = [| (1,0);(2,0);(3,0);(4,0) |] in
    let result_set,result_time_shift = Phase2.extract_time_change initial_state trans_fun_raw 
    and expected_set = Common.IntSet.singleton 4 |> Common.IntSet.add 1
    and expected_time_shift = 1 in 
    assert_equal
        ~msg:"Result set is not equal to expected"
        ~cmp:Common.IntSet.equal
        ~printer:(fun s -> let res = Common.IntSet.elements s |> List.map (fun i -> string_of_int i) |> String.concat "," in "{"^res^"}" )
        expected_set
        result_set;
    assert_equal
        ~msg:"Result time shift is not equal to expected"
        expected_time_shift
        result_time_shift
let test_time_change_2 _ =
    let trans_fun_raw = {Ssp.State.permutation_with_time_shift=[(1,7);(3,0);(2,0);(4,7)]; react_label=""; from_idx=(-1); to_idx=(-1); transition_idx=(-1)}
    and initial_state = [| (2,13);(4,19);(3,7);(1,5) |] in
    let result_set,result_time_shift = Phase2.extract_time_change initial_state trans_fun_raw 
    and expected_set = Common.IntSet.singleton 2 |> Common.IntSet.add 1
    and expected_time_shift = 7 in 
    assert_equal
        ~msg:"Result set is not equal to expected"
        ~cmp:Common.IntSet.equal
        ~printer:(fun s -> let res = Common.IntSet.elements s |> List.map (fun i -> string_of_int i) |> String.concat "," in "{"^res^"}" )
        expected_set
        result_set;
    assert_equal
        ~msg:"Result time shift is not equal to expected"
        expected_time_shift
        result_time_shift
let test_transform_ui_map_1 _ =
    let participants = Bigraph.Iso.of_list [(0,1);(1,0)]
    and residue = Bigraph.Fun.of_list [(0,2);(1,1);(2,0)] 
    and out_state_big = "{(0, C:0),(1, B:0),(2, A:0)}\n0 3 0\n000\n000\n000" |> Bigraph.Big.of_string in
    let trans_exported = {Tracking_bigraph.TTS.in_state_idx=0;out_state_idx=1;react_label="";participants;residue;actual_out_state=out_state_big}
    and initial_ui_map = Ui.make_map_of_list [(7,0);(9,1);(21,2)]
    and first_new_ui = 51
    and all_states = List.to_seq [(1,out_state_big)] |> Hashtbl.of_seq in
    let result_ui2redex,result_ui2state,result_new_first_ui = Phase2.transform_ui_map trans_exported initial_ui_map first_new_ui all_states
    and expected_ui2redex = Ui.make_map_of_list [(9,0);(7,1)]
    and expected_ui2state = Ui.make_map_of_list [(21,0);(9,1);(7,2)]
    and expected_first_new_ui = 51 in
    assert_equal
        ~msg:"Result map of UIs on redex is not equal to expected"
        ~printer:Ui.map_to_string
        ~cmp:Ui.are_equal
        expected_ui2redex
        result_ui2redex;
    assert_equal
        ~msg:"Result map of UIs on output state is not equal to expected"
        ~printer:Ui.map_to_string
        ~cmp:Ui.are_equal
        expected_ui2state
        result_ui2state;
    assert_equal
        ~msg:"Result map of UIs on redex is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        expected_first_new_ui
        result_new_first_ui
let test_transform_ui_map_2 _ =
    let participants = Bigraph.Iso.of_list [(0,2);(1,0);(2,1)]
    and residue = Bigraph.Fun.of_list [(0,2);(1,0)] 
    and out_state_big = "{(0, C:0),(1, A:0)}\n0 2 0\n00\n00" |> Bigraph.Big.of_string in
    let trans_exported = {Tracking_bigraph.TTS.in_state_idx=0;out_state_idx=1;react_label="";participants;residue;actual_out_state=out_state_big}
    and initial_ui_map = Ui.make_map_of_list [(7,0);(9,1);(21,2)]
    and first_new_ui = 51
    and all_states = List.to_seq [(1,out_state_big)] |> Hashtbl.of_seq in
    let result_ui2redex,result_ui2state,result_new_first_ui = Phase2.transform_ui_map trans_exported initial_ui_map first_new_ui all_states
    and expected_ui2redex = Ui.make_map_of_list [(7,1);(9,2);(21,0)]
    and expected_ui2state = Ui.make_map_of_list [(21,0);(7,1)]
    and expected_first_new_ui = 51 in
    assert_equal
        ~msg:"Result map of UIs on redex is not equal to expected"
        ~printer:Ui.map_to_string
        ~cmp:Ui.are_equal
        expected_ui2redex
        result_ui2redex;
    assert_equal
        ~msg:"Result map of UIs on output state is not equal to expected"
        ~printer:Ui.map_to_string
        ~cmp:Ui.are_equal
        expected_ui2state
        result_ui2state;
    assert_equal
        ~msg:"Result first new unique identifier is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        expected_first_new_ui
        result_new_first_ui
let test_transform_ui_map_3 _ =
    let participants = Bigraph.Iso.of_list [(0,1);(1,0)]
    and residue = Bigraph.Fun.of_list [(0,2);(1,1);(2,0)] 
    and out_state_big = "{(0, C:0),(1, B:0),(2, A:0),(3, D:0)}\n0 4 0\n0000\n0000\n0000\n0000" |> Bigraph.Big.of_string in
    let trans_exported = {Tracking_bigraph.TTS.in_state_idx=0;out_state_idx=1;react_label="";participants;residue;actual_out_state=out_state_big}
    and initial_ui_map = Ui.make_map_of_list [(7,0);(9,1);(21,2)]
    and first_new_ui = 51
    and all_states = List.to_seq [(1,out_state_big)] |> Hashtbl.of_seq in
    let result_ui2redex,result_ui2state,result_new_first_ui = Phase2.transform_ui_map trans_exported initial_ui_map first_new_ui all_states
    and expected_ui2redex = Ui.make_map_of_list [(9,0);(7,1)]
    and expected_ui2state = Ui.make_map_of_list [(21,0);(9,1);(7,2);(51,3)]
    and expected_first_new_ui = 52 in
    assert_equal
        ~msg:"Result map of UIs on redex is not equal to expected"
        ~printer:Ui.map_to_string
        ~cmp:Ui.are_equal
        expected_ui2redex
        result_ui2redex;
    assert_equal
        ~msg:"Result map of UIs on output state is not equal to expected"
        ~printer:Ui.map_to_string
        ~cmp:Ui.are_equal
        expected_ui2state
        result_ui2state;
    assert_equal
        ~msg:"Result map of UIs on redex is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        expected_first_new_ui
        result_new_first_ui
let test_transform_ui_map_4 _ =
    let participants = Bigraph.Iso.of_list [(0,1);(1,0)]
    and residue = Bigraph.Fun.of_list [(0,2);(1,0)] 
    and out_state_big = "{(0, C:0),(1, A:0),(2, D:0)}\n0 3 0\n000\n000\n000" |> Bigraph.Big.of_string in
    let trans_exported = {Tracking_bigraph.TTS.in_state_idx=0;out_state_idx=1;react_label="";participants;residue;actual_out_state=out_state_big}
    and initial_ui_map = Ui.make_map_of_list [(7,0);(9,1);(21,2)]
    and first_new_ui = 51
    and all_states = List.to_seq [(1,out_state_big)] |> Hashtbl.of_seq in
    let result_ui2redex,result_ui2state,result_new_first_ui = Phase2.transform_ui_map trans_exported initial_ui_map first_new_ui all_states
    and expected_ui2redex = Ui.make_map_of_list [(9,0);(7,1)]
    and expected_ui2state = Ui.make_map_of_list [(21,0);(7,1);(51,2)]
    and expected_first_new_ui = 52 in
    assert_equal
        ~msg:"Result map of UIs on redex is not equal to expected"
        ~printer:Ui.map_to_string
        ~cmp:Ui.are_equal
        expected_ui2redex
        result_ui2redex;
    assert_equal
        ~msg:"Result map of UIs on output state is not equal to expected"
        ~printer:Ui.map_to_string
        ~cmp:Ui.are_equal
        expected_ui2state
        result_ui2state;
    assert_equal
        ~msg:"Result map of UIs on redex is not equal to expected"
        ~printer:(fun i -> string_of_int i)
        expected_first_new_ui
        result_new_first_ui
let _assert_equality_of_extended_walk_elements (expected_ewe:Phase3.extended_walk_element) (actual_ewe:Phase3.extended_walk_element) msg =
    assert_equal
        ~msg:("The transition function in"^msg)
        ~cmp:(fun tf1 tf2 -> tf1.Ssp.Template_state.transition_idx = tf2.Ssp.Template_state.transition_idx )
        ~printer:(fun tf -> string_of_int (tf.Ssp.Template_state.transition_idx))
        expected_ewe.trans_fun
        actual_ewe.trans_fun;
    assert_equal
        ~msg:("The map of UIs on redex in "^msg)
        ~cmp:Ui.are_equal
        ~printer:(fun ui_map -> Ui.map_to_string ui_map)
        expected_ewe.ui2redex
       actual_ewe.ui2redex;
    assert_equal
        ~msg:("The map of UIs on state in "^msg)
        ~cmp:Ui.are_equal
        ~printer:(fun ui_map -> Ui.map_to_string ui_map)
        expected_ewe.ui2state
        actual_ewe.ui2state;
    assert_equal
        ~msg:("The value of first new UI in "^msg)
        ~printer:(fun i -> string_of_int i)
        expected_ewe.first_new_ui
        actual_ewe.first_new_ui;
    assert_equal
        ~msg:("The time change indicator in "^msg)
        ~cmp:(fun (ags1,ts1) (ags2,ts2) -> Common.IntSet.equal ags1 ags2 && ts1 = ts2  )
        ~printer:
            (
                fun (ags,ts) -> 
                    let set_str = Common.IntSet.elements ags |> List.map (fun i -> string_of_int i) |> String.concat "," 
                    and ts_str = string_of_int ts in
                    "set: {"^set_str^"} time shift: "^ts_str
            )
        expected_ewe.time_change
        actual_ewe.time_change;;
Ssp.Template_state._number_of_agents :=2
let test_perform_phase _ =
    let trans_fun_raw_1 = {Ssp.State.permutation_with_time_shift=[(2,1);(1,0)]; react_label="r1"; from_idx=0; to_idx=1; transition_idx=1} 
    and trans_fun_raw_2 = {Ssp.State.permutation_with_time_shift=[(1,0);(2,1)]; react_label="r1"; from_idx=1; to_idx=2; transition_idx=2} 
    and state_1_big = "{(0, A:0),(1, A:0),(2, AT1:0),(3, AT2:0)}\n0 4 0\n0000\n0000\n1000\n0100" |> Bigraph.Big.of_string
    and state_2_big = "{(0, AT2:0),(1, AT1:0),(2, A:0),(3, A:0)}\n0 4 0\n0011\n0000\n0000\n0000" |> Bigraph.Big.of_string in
    let trans_1 = 
        {Tracking_bigraph.TTS.in_state_idx=0;
        out_state_idx=1;
        react_label="r1";
        participants=Bigraph.Iso.of_list [(0,1);(1,2);(2,3)];
        residue=Bigraph.Fun.of_list [(0,0);(1,1);(2,2);(3,3)];
        actual_out_state=state_1_big}
    and trans_2 = 
        {Tracking_bigraph.TTS.in_state_idx=1;
        out_state_idx=2;
        react_label="r1";
        participants=Bigraph.Iso.of_list [(0,0);(1,2);(2,3)];
        residue=Bigraph.Fun.of_list [(0,3);(1,2);(2,1);(3,0)];
        actual_out_state=state_2_big}
    and initial_ui_map =  Ui.make_map_of_list [(1,0);(2,1);(3,2);(4,3)]
    and first_new_ui = 5
    and num_of_agents = 2 in
    let all_states = List.to_seq [(1,state_1_big);(2,state_2_big)] |> Hashtbl.of_seq
    and all_trans = List.to_seq [(1,trans_1);(2,trans_2)] |> Hashtbl.of_seq
    and raw_walk = [trans_fun_raw_1;trans_fun_raw_2] in
    let result = Phase2.perform_phase raw_walk initial_ui_map ~num_of_agents ~first_new_ui all_states all_trans 
    and expected_result = 
        [
            {
                Phase3.trans_fun=(Ssp.Template_state.parse_trans_fun trans_fun_raw_1);
                ui2redex=[(2,0);(3,1);(4,2)] |> Ui.make_map_of_list;
                ui2state=initial_ui_map;
                first_new_ui=5;
                time_change=(Common.IntSet.of_list [2],1)
            };
            {
                trans_fun=(Ssp.Template_state.parse_trans_fun trans_fun_raw_2);
                ui2redex=[(1,0);(3,1);(4,2)] |> Ui.make_map_of_list;
                ui2state=[ (1,3);(2,2);(3,1);(4,0)] |> Ui.make_map_of_list ;
                first_new_ui=5;
                time_change=(Common.IntSet.of_list [1],1)
            }
        ] in
    assert_equal
        ~msg:"The length of result extended walk is not equal to expected"
        (List.length expected_result)
        (List.length result);
    _assert_equality_of_extended_walk_elements
        (List.nth expected_result 0)
        (List.nth result 0)
        "the first element of the result extended walk is not equal to expected";
    _assert_equality_of_extended_walk_elements
        (List.nth expected_result 1)
        (List.nth result 1)
        "the second element of the result extended walk is not equal to expected"

(*przemieszać indeksy wierzchołków *)
let suite =
    "Phase 2" >::: [
        "Generating mapping of UI on redex test">:: test_gen_ui2redex;
        "Extracting time change from raw trans fun test 1">:: test_time_change_1;
        "Extracting time change from raw trans fun test 2">:: test_time_change_2;
        "Transforming map of unique identifiers test 1 - no new or deleted objects">:: test_transform_ui_map_1;
        "Transforming map of unique identifiers test 2 - deleted objects">:: test_transform_ui_map_2;
        "Transforming map of unique identifiers test 3 - new objects">:: test_transform_ui_map_3;
        "Transforming map of unique identifiers test 4 - new and deleted objects">:: test_transform_ui_map_4;
        "Performing phase test">::test_perform_phase
    ]

let () =
  run_test_tt_main suite