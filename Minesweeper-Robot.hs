import Data.List

type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)

up:: MyState -> MyState

up (S (x,y) mines prev_command prev_state) | (x-1) >= 0 =  (S (x-1,y) mines "up" (S (x,y) mines prev_command prev_state))
                                           | otherwise = Null
down:: MyState -> MyState

down (S (x,y) mines prev_command prev_state) | (x+1) <= 3  = (S (x+1,y) mines "down" (S (x,y) mines prev_command prev_state))
                                             | otherwise = Null


left:: MyState -> MyState

left (S (x,y) mines prev_command prev_state) | (y-1) >= 0 = (S (x,y-1) mines "left" (S (x,y) mines prev_command prev_state))
                                             | otherwise = Null


right:: MyState -> MyState

right (S (x,y) mines prev_command prev_state) | (y+1) <= 3 = (S (x,y+1) mines "right" (S (x,y) mines prev_command prev_state))
                                              | otherwise = Null


collect:: MyState -> MyState

collect (S curr_pos mines prev_command prev_state) | not ((find (==curr_pos) mines) == Nothing) = (S curr_pos (delete curr_pos mines) "collect" (S curr_pos mines prev_command prev_state))
                                                   | otherwise = Null




nextMyStates::MyState->[MyState]

nextMyStates state = (nextMyStatesHelper up state) ++ (nextMyStatesHelper down state) ++ (nextMyStatesHelper left state) ++  (nextMyStatesHelper right state) ++ (nextMyStatesHelper collect state)

nextMyStatesHelper move_fn state | (move_fn state) == Null = []
                                 | otherwise = [move_fn state]



isGoal::MyState->Bool

isGoal (S _ mines _ _) = mines == []

search::[MyState]->MyState

search [] = Null
search (h:t) | isGoal h = h
             | otherwise = search (t ++ nextMyStates h)



constructSolution:: MyState ->[String]


constructSolution (S _ _ prev_command prev_state) | prev_state == Null = []
                                                  | otherwise = (constructSolution prev_state) ++ [prev_command] 




solve :: Cell->[Cell]->[String]


solve c mines = solve_with_states (S c mines "" Null)

solve_with_states state | isGoal state = constructSolution state
                        | otherwise = solve_with_states (search (nextMyStates state))






















