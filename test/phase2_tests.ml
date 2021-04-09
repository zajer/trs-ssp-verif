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

let suite =
    "Phase 2" >::: [
        "Generating mapping of UI on redex test">:: test_gen_ui2redex;
        "Extracting time change from raw trans fun">:: test_time_change;
    ]

let () =
  run_test_tt_main suite