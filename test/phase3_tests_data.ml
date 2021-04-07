open Ssp_verification
module IntSet = Set.Make(Int)
module OneAgentState = Ssp.Template_state;;
OneAgentState._number_of_agents := 1;;
let phase_test_one_agent_previous_state =
"
{(0, A:0),(1, B:0),(2, C:0)}
0 3 0
000
100
000" |> Bigraph.Big.of_string
let phase_test_one_agent_output_state =
"
{(0, A:0),(1, B:0),(2, C:0)}
0 3 0
000
000
100" |> Bigraph.Big.of_string
let phase_test_one_agent_trans_fun = 
    {
        Ssp.State.permutation_with_time_shift=[(1,1)]; 
        react_label="PT-1A-R1"; 
        from_idx=0; 
        to_idx=1; 
        transition_idx=1
    } |> OneAgentState.parse_trans_fun
let phase_test_one_agent_ewalk =
    [{
        Phase3.trans_fun=phase_test_one_agent_trans_fun;
        ui2redex=[(1,0)]|> Ui.make_map_of_list;
        ui2state=[(1,0);(2,1);(3,2)]|>Ui.make_map_of_list;
        first_new_ui=4;
        time_change=((IntSet.singleton 1),1)
    }]
let phase_test_one_agent_all_states =
    [
        (0,phase_test_one_agent_previous_state);
        (1,phase_test_one_agent_output_state)
    ] |> List.to_seq |> Hashtbl.of_seq
let phase_test_one_agent_transition = {
        Tracking_bigraph.TTS.in_state_idx=0;
        out_state_idx=1;
        react_label="PT-1A-R1";
        participants=Bigraph.Iso.of_list [(0,0);(1,1);(2,2)];
        residue=Bigraph.Fun.of_list [(0,0);(1,1);(2,2)];
        actual_out_state=Bigraph.Big.one
    }
let phase_test_one_agent_all_trans_by_idx =  
    [(1,phase_test_one_agent_transition)] |> List.to_seq |> Hashtbl.of_seq
let phase_test_one_agent_all_trans_by_keys =  
    [
        ((phase_test_one_agent_transition.in_state_idx,phase_test_one_agent_transition.react_label),phase_test_one_agent_transition)
    ] |> List.to_seq |> Hashtbl.of_seq
module TwoAgentsState = Ssp.Template_state;;
TwoAgentsState._number_of_agents := 2;;
let phase_test_two_agents_previous_state =
"
{(0, A:0),(1, A:0),(2, B:0),(3, C:0)}
0 4 0
0000
0000
1100
0000" |> Bigraph.Big.of_string
let phase_test_two_agents_middle_state =
"
{(0, A:0),(1, A:0),(2, B:0),(3, C:0)}
0 4 0
0000
0000
1000
0100" |> Bigraph.Big.of_string
let phase_test_two_agents_output_state =
"
{(0, A:0),(1, A:0),(2, B:0),(3, C:0)}
0 4 0
0000
0000
0000
1100" |> Bigraph.Big.of_string
let phase_test_two_agents_trans_fun_1 = 
    {
        Ssp.State.permutation_with_time_shift=[(1,1);(2,0)]; 
        react_label="PT-2A-R1"; 
        from_idx=0; 
        to_idx=1; 
        transition_idx=1
    } |> TwoAgentsState.parse_trans_fun
let phase_test_two_agents_trans_fun_2 = 
    {
        Ssp.State.permutation_with_time_shift=[(2,1);(1,0)]; 
        react_label="PT-2A-R1"; 
        from_idx=0; 
        to_idx=1; 
        transition_idx=2
    } |> TwoAgentsState.parse_trans_fun
let phase_test_two_agents_trans_fun_3 = 
    {
        Ssp.State.permutation_with_time_shift=[(1,0);(2,1)]; 
        react_label="PT-2A-R1"; 
        from_idx=1; 
        to_idx=2; 
        transition_idx=3
    } |> TwoAgentsState.parse_trans_fun
let phase_test_two_agents_ewalk_1 =
    [
        {
            Phase3.trans_fun=phase_test_two_agents_trans_fun_2;
            ui2redex=[(2,0);(3,1);(4,2)]|> Ui.make_map_of_list;
            ui2state=[(1,0);(2,1);(3,2);(4,3)]|>Ui.make_map_of_list;
            first_new_ui=5;
            time_change=((IntSet.singleton 2),1)
        };
        {
            Phase3.trans_fun=phase_test_two_agents_trans_fun_3;
            ui2redex=[(1,0);(3,1);(4,2)]|> Ui.make_map_of_list;
            ui2state=[(1,0);(2,1);(3,2);(4,3)]|>Ui.make_map_of_list;
            first_new_ui=5;
            time_change=((IntSet.singleton 1),1)
        };
    ]
let phase_test_two_agents_ewalk_2 =
    [
        {
            Phase3.trans_fun=phase_test_two_agents_trans_fun_1;
            ui2redex=[(1,0);(3,1);(4,2)]|> Ui.make_map_of_list;
            ui2state=[(1,0);(2,1);(3,2);(4,3)]|>Ui.make_map_of_list;
            first_new_ui=5;
            time_change=((IntSet.singleton 1),1)
        };
        {
            Phase3.trans_fun=phase_test_two_agents_trans_fun_3;
            ui2redex=[(2,0);(3,1);(4,2)]|> Ui.make_map_of_list;
            ui2state=[(1,0);(2,1);(3,2);(4,3)]|>Ui.make_map_of_list;
            first_new_ui=5;
            time_change=((IntSet.singleton 2),1)
        };
    ]
let phase_test_two_agents_all_states =
    [
        (0,phase_test_two_agents_previous_state);
        (1,phase_test_two_agents_middle_state);
        (2,phase_test_two_agents_output_state);
    ] |> List.to_seq |> Hashtbl.of_seq
let phase_test_two_agents_transition_1 = {
        Tracking_bigraph.TTS.in_state_idx=0;
        out_state_idx=1;
        react_label="PT-2A-R1";
        participants=Bigraph.Iso.of_list [(0,0);(1,2);(2,3)];
        residue=Bigraph.Fun.of_list [(0,0);(1,1);(2,2);(3,3)];
        actual_out_state=Bigraph.Big.one
    }
let phase_test_two_agents_transition_2 = {
        Tracking_bigraph.TTS.in_state_idx=0;
        out_state_idx=1;
        react_label="PT-2A-R1";
        participants=Bigraph.Iso.of_list [(0,1);(1,2);(2,3)];
        residue=Bigraph.Fun.of_list [(0,1);(1,0);(2,2);(3,3)];
        actual_out_state=Bigraph.Big.one
    }
let phase_test_two_agents_transition_3 = {
        Tracking_bigraph.TTS.in_state_idx=1;
        out_state_idx=2;
        react_label="PT-2A-R1";
        participants=Bigraph.Iso.of_list [(0,1);(1,2);(2,3)];
        residue=Bigraph.Fun.of_list [(0,0);(1,1);(2,2);(3,3)];
        actual_out_state=Bigraph.Big.one
    }
let phase_test_two_agents_all_trans_by_idx =  
    [(1,phase_test_two_agents_transition_1);(2,phase_test_two_agents_transition_2);(3,phase_test_two_agents_transition_3)] |> List.to_seq |> Hashtbl.of_seq
let phase_test_two_agents_all_trans_by_keys () = 
    let result = Hashtbl.create 3 in
    Hashtbl.add result (phase_test_two_agents_transition_1.in_state_idx,phase_test_two_agents_transition_1.react_label) phase_test_two_agents_transition_1;
    Hashtbl.add result (phase_test_two_agents_transition_2.in_state_idx,phase_test_two_agents_transition_2.react_label) phase_test_two_agents_transition_2;
    Hashtbl.add result (phase_test_two_agents_transition_3.in_state_idx,phase_test_two_agents_transition_3.react_label) phase_test_two_agents_transition_3;
    result
    (*
    [
        ((phase_test_two_agents_transition_1.in_state_idx,phase_test_two_agents_transition_1.react_label),phase_test_two_agents_transition_1);
        ((phase_test_two_agents_transition_2.in_state_idx,phase_test_two_agents_transition_2.react_label),phase_test_two_agents_transition_2);
        ((phase_test_two_agents_transition_3.in_state_idx,phase_test_two_agents_transition_3.react_label),phase_test_two_agents_transition_3)
    ] |> List.to_seq |> Hashtbl.of_seq*)