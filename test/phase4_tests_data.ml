let update_test_1_state =
"
{(0, C:0),(1, A:0),(2, B:0)}
0 3 0
000
000
000"
let update_test_2_state =
"
{(0, C:0),(1, A:0)}
0 2 0
00
00"
let update_test_3_state =
"
{(0, C:0),(1, A:0),(2, B:0),(3, D:0)}
0 4 0
0000
0000
0000
0000"
let update_test_4_state =
"
{(0, C:0),(1, A:0),(2, D:0)}
0 3 0
000
000
000"
let perform_phase_test_invalid_previous_state =
"
{(0, A:0)}
0 1 0
0
"
let perform_phase_test_correct_previous_state =
"
{(0, A:0),(1, B:0)}
0 2 0
00
00"
let perform_phase_test_invalid_current_state = 
"
{(0, A:0),(1, C:0),(2, D:0)}
0 3 0
000
000
000"
let perform_phase_test_correct_current_state = 
"
{(0, A:0),(1, B:0),(2, C:0)}
0 3 0
000
000
000"