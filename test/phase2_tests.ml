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
        result
let test_time_change _ =
    let trans_fun_raw = {Ssp.State.permutation_with_time_shift=[(1,1);(3,0);(2,0);(4,1)]; react_label=""; from_idx=(-1); to_idx=(-1); transition_idx=(-1)} in
    let result_set,result_time_shift = Phase2.extract_time_change trans_fun_raw 
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
let suite =
    "Phase 2" >::: [
        "Generating mapping of UI on redex test">:: test_gen_ui2redex;
        "Extracting time change from raw trans fun">:: test_time_change;
        "Transforming map of unique identifiers test 1 - no new or deleted objects">:: test_transform_ui_map_1;
        "Transforming map of unique identifiers test 2 - deleted objects">:: test_transform_ui_map_2;
        "Transforming map of unique identifiers test 3 - new objects">:: test_transform_ui_map_3;
        "Transforming map of unique identifiers test 4 - new and deleted objects">:: test_transform_ui_map_4;
    ]

let () =
  run_test_tt_main suite