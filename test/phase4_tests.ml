open OUnit2
open Ssp_verification
let test_is_update_with_trans_possible_1 _ =
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
let test_is_update_with_trans_possible_2 _ =
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
let test_is_update_with_trans_possible_3 _ =
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
let test_is_update_possible_1 _ =
  let ui2state_map = Ui.make_map_of_list [(7,0);(21,1);(777,2)]
  and ui2par_map = Ui.make_map_of_list [(777,0);(7,1)] 
  and applied_transition = 
    {
      Tracking_bigraph.TTS.in_state_idx=(-1);
      out_state_idx=(-1);
      react_label="my react";
      participants=(Bigraph.Iso.of_list [(0,1);(1,2)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and known_transition_1 = 
    {
      Tracking_bigraph.TTS.in_state_idx=(-1);
      out_state_idx=(-1);
      react_label="my react";
      participants=(Bigraph.Iso.of_list [(0,1);(1,2)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and known_transition_2 =
    {
      Tracking_bigraph.TTS.in_state_idx=(2);
      out_state_idx=(-1);
      react_label="my react";
      participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and known_transition_3 =
    {
      Tracking_bigraph.TTS.in_state_idx=(1);
      out_state_idx=(-1);
      react_label="my react2";
      participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and known_transition_4 = 
    {
      Tracking_bigraph.TTS.in_state_idx=(1);
      out_state_idx=(-1);
      react_label="my react";
      participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and state = {Tracking_bigraph.TTS.bigraph=Bigraph.Big.one;index=1} in
  let known_transitions = [known_transition_1;known_transition_2;known_transition_3;known_transition_4] in
  let known_trans_dict = 
    Seq.unfold 
      (
        fun list -> 
          match list with 
          | [] -> None
          | h::t -> 
            let dict_elem_key = h.Tracking_bigraph.TTS.in_state_idx,h.react_label
            and dict_elem_val = h in
            Some ((dict_elem_key,dict_elem_val),t)
      ) 
      known_transitions |> Hashtbl.of_seq in
  let result = Phase4.is_update_possible state ~ui2state_map ~ui2par_map applied_transition known_trans_dict
  and expected_result = true, Some known_transition_4 in
  assert_equal
    ~msg:"Result of is_update_possible is not equal to expected"
    expected_result
    result
let test_is_update_possible_2 _ =
  let ui2state_map = Ui.make_map_of_list [(7,0);(21,1);(777,2)]
  and ui2par_map = Ui.make_map_of_list [(777,0);(7,1)] 
  and applied_transition = 
    {
      Tracking_bigraph.TTS.in_state_idx=(1);
      out_state_idx=(-1);
      react_label="my react";
      participants=(Bigraph.Iso.of_list [(0,1);(1,2)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and known_transition_1 = 
    {
      Tracking_bigraph.TTS.in_state_idx=(1);
      out_state_idx=(-1);
      react_label="my react";
      participants=(Bigraph.Iso.of_list [(0,1);(1,2)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and known_transition_2 =
    {
      Tracking_bigraph.TTS.in_state_idx=(2);
      out_state_idx=(-1);
      react_label="my react";
      participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and known_transition_3 =
    {
      Tracking_bigraph.TTS.in_state_idx=(1);
      out_state_idx=(-1);
      react_label="my react2";
      participants=(Bigraph.Iso.of_list [(0,2);(1,0)]);
      residue=Bigraph.Fun.empty;
      actual_out_state=Bigraph.Big.one
    }
  and state = {Tracking_bigraph.TTS.bigraph=Bigraph.Big.one;index=1} in
  let known_transitions = [known_transition_1;known_transition_2;known_transition_3] in
  let known_trans_dict = 
    Seq.unfold 
      (
        fun list -> 
          match list with 
          | [] -> None
          | h::t -> 
            let dict_elem_key = h.Tracking_bigraph.TTS.in_state_idx,h.react_label
            and dict_elem_val = h in
            Some ((dict_elem_key,dict_elem_val),t)
      ) 
      known_transitions |> Hashtbl.of_seq in
  let result = Phase4.is_update_possible state ~ui2state_map ~ui2par_map applied_transition known_trans_dict
  and expected_result = false, None in
  assert_equal
    ~msg:"Result of is_update_possible is not equal to expected"
    expected_result
    result
let suite =
    "Phase 4" >::: [
        "Is update with trans possible test 1">:: test_is_update_with_trans_possible_1;
        "Is update with trans possible test 2">:: test_is_update_with_trans_possible_2;
        "Is update with trans possible test 3">:: test_is_update_with_trans_possible_3;
        "Is update possible test 1">:: test_is_update_possible_1;
        "Is update possible test 2">:: test_is_update_possible_2;
    ]

let () =
  run_test_tt_main suite