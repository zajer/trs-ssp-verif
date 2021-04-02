open OUnit2
open Ssp_verification
let test_is_update_possible_1 _ =
  let ui2state_map = Ui.make_map_of_list [(7,0);(21,1);(777,2)]
  and ui2par_map = Ui.make_map_of_list [(777,0);(7,1)]
  and transition = 
    {
        Tracking_bigraph.TTS.in_state_idx=(-1);
        out_state_idx=(-1);
        react_label="my react";
        participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
        residue=Bigraph.Fun.empty;
        actual_out_state=Bigraph.Big.one
    }
  in
  let result = Phase4.is_update_with_trans_possible ~ui2state_map transition ~ui2par_map
  and expected_result = true in
  assert_equal
    ~msg:"Result not equal to expected"
    ~printer:(fun b -> string_of_bool b)
    expected_result
    result
let test_is_update_possible_2 _ =
  let ui2state_map = Ui.make_map_of_list [(7,0);(21,1);(777,2)]
  and ui2par_map = Ui.make_map_of_list [(777,1);(7,0)]
  and transition = 
    {
        Tracking_bigraph.TTS.in_state_idx=(-1);
        out_state_idx=(-1);
        react_label="my react";
        participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
        residue=Bigraph.Fun.empty;
        actual_out_state=Bigraph.Big.one
    }
  in
  let result = Phase4.is_update_with_trans_possible ~ui2state_map transition ~ui2par_map
  and expected_result = false in
  assert_equal
    ~msg:"Result not equal to expected"
    ~printer:(fun b -> string_of_bool b)
    expected_result
    result
let test_is_update_possible_3 _ =
  let ui2state_map = Ui.make_map_of_list [(7,0);(21,1);(777,2)]
  and ui2par_map = Ui.make_map_of_list [(5,0);(7,1)]
  and transition = 
    {
        Tracking_bigraph.TTS.in_state_idx=(-1);
        out_state_idx=(-1);
        react_label="my react";
        participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
        residue=Bigraph.Fun.empty;
        actual_out_state=Bigraph.Big.one
    }
  in
  let result = Phase4.is_update_with_trans_possible ~ui2state_map transition ~ui2par_map
  and expected_result = false in
  assert_equal
    ~msg:"Result not equal to expected"
    ~printer:(fun b -> string_of_bool b)
    expected_result
    result
let suite =
    "Phase 4" >::: [
        "Is update possible test 1">:: test_is_update_possible_1;
        "Is update possible test 2">:: test_is_update_possible_2;
        "Is update possible test 3">:: test_is_update_possible_3;
    ]

let () =
  run_test_tt_main suite