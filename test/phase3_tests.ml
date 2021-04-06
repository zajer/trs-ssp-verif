open OUnit2
open Ssp_verification
let _int_list2tstring il =
    "["^(List.map (fun i -> string_of_int i) il |> String.concat ";")^"]"
let _tinfo2string tinfo = 
    let participants_str = _int_list2tstring tinfo.Phase3.participants 
    and transition_idx_str = string_of_int tinfo.transition_idx
    and new_objs_str = _int_list2tstring tinfo.new_objs
    and term_objs_str = _int_list2tstring tinfo.term_objs
    and start_time_str = string_of_int tinfo.start_time
    and end_time_str = string_of_int tinfo.end_time in
    let result = ["participants:"^participants_str;"react_label:"^tinfo.react_label;"transition_idx:"^transition_idx_str;"new_objs:"^new_objs_str;"term_objs:"^term_objs_str;"start_time:"^start_time_str;"end_time:"^end_time_str] |> String.concat ";" in
    "{"^result^"}"
let test_extract_time_info_1 _ = 
    let ui2state_before_transition = [(0,0);(1,1);(2,2)] |> Ui.make_map_of_list
    and ui2state_after_transition = [(0,2);(1,0);(2,1)] |> Ui.make_map_of_list
    and ui2redex = [(0,0);(2,2)] |> Ui.make_map_of_list
    and constructed_moment_of_time = 777
    and duration_of_transition = 21
    and transition_idx = 7
    and react_label = "my react" in
    let result = Phase3.extract_time_info ~ui2state_before_transition ~ui2state_after_transition ~ui2redex ~constructed_moment_of_time ~duration_of_transition ~transition_idx react_label 
    and expected_result = {Phase3.participants=[0;2] ; react_label; transition_idx; new_objs=[]; term_objs=[]; start_time=777; end_time=798} in
    assert_equal
        ~msg:"Result time info is not equal to expected"
        ~printer:_tinfo2string
        expected_result
        result
let test_extract_time_info_2 _ = 
    let ui2state_before_transition = [(0,0);(1,1);(2,2)] |> Ui.make_map_of_list
    and ui2state_after_transition = [(0,2);(1,0);(2,3);(3,1)] |> Ui.make_map_of_list
    and ui2redex = [(0,0);(2,2)] |> Ui.make_map_of_list
    and constructed_moment_of_time = 777
    and duration_of_transition = 21
    and transition_idx = 7
    and react_label = "my react" in
    let result = Phase3.extract_time_info ~ui2state_before_transition ~ui2state_after_transition ~ui2redex ~constructed_moment_of_time ~duration_of_transition ~transition_idx react_label 
    and expected_result = {Phase3.participants=[0;2] ; react_label; transition_idx; new_objs=[3]; term_objs=[]; start_time=777; end_time=798} in
    assert_equal
        ~msg:"Result time info is not equal to expected"
        ~printer:_tinfo2string
        expected_result
        result
let test_extract_time_info_3 _ = 
    let ui2state_before_transition = [(0,0);(1,1);(2,2)] |> Ui.make_map_of_list
    and ui2state_after_transition = [(0,1);(2,0)] |> Ui.make_map_of_list
    and ui2redex = [(0,0);(2,2)] |> Ui.make_map_of_list
    and constructed_moment_of_time = 777
    and duration_of_transition = 21
    and transition_idx = 7
    and react_label = "my react" in
    let result = Phase3.extract_time_info ~ui2state_before_transition ~ui2state_after_transition ~ui2redex ~constructed_moment_of_time ~duration_of_transition ~transition_idx react_label 
    and expected_result = {Phase3.participants=[0;2] ; react_label; transition_idx; new_objs=[]; term_objs=[1]; start_time=777; end_time=798} in
    assert_equal
        ~msg:"Result time info is not equal to expected"
        ~printer:_tinfo2string
        expected_result
        result
let test_extract_time_info_4 _ = 
    let ui2state_before_transition = [(0,0);(1,1);(2,2)] |> Ui.make_map_of_list
    and ui2state_after_transition = [(0,1);(3,0);(2,2)] |> Ui.make_map_of_list
    and ui2redex = [(0,0);(2,2)] |> Ui.make_map_of_list
    and constructed_moment_of_time = 777
    and duration_of_transition = 21
    and transition_idx = 7
    and react_label = "my react" in
    let result = Phase3.extract_time_info ~ui2state_before_transition ~ui2state_after_transition ~ui2redex ~constructed_moment_of_time ~duration_of_transition ~transition_idx react_label 
    and expected_result = {Phase3.participants=[0;2] ; react_label; transition_idx; new_objs=[3]; term_objs=[1]; start_time=777; end_time=798} in
    assert_equal
        ~msg:"Result time info is not equal to expected"
        ~printer:_tinfo2string
        expected_result
        result

let suite =
    "Phase 3" >::: [
        "Extracting time info test 1 - no new or deleted objects">:: test_extract_time_info_1;
        "Extracting time info test 2 - new obj, no deleted objects">:: test_extract_time_info_2;
        "Extracting time info test 3 - no new, deleted objects">:: test_extract_time_info_3;
        "Extracting time info test 4 - new and deleted objects">:: test_extract_time_info_4
    ]

let () =
  run_test_tt_main suite