-- Simon Newham | NWHSIM001
-- Palindrome Truring machine
-- x is right array h is head and y is left array

-- Input example: tmpal "aabbaa" => true

tmpal :: String -> Char
tmpal s | s == "" =  'a'
		| otherwise = state0 'B' (head s) ((tail s) ++ "B")

--if blank true, if a,b replace a,b and move right
state0 x h y  | h == 'B' = 'a'
              | h == 'b' = state1 "B" (head y) (tail y ++ "B")
              | h == 'a' = state4 "B" (head y) (tail y ++ "B") --consume a and move right

--move left if blank to state 2,  otherwise move right but don't write
state1 x h y | h=='B'  = state2 ("B"++init x) (last x) ("B" ++ y) --move left and check char
             | otherwise = state1 ((tail x)++[h]) (head y) ((tail y)++"B") --move right


state2 x h y | h=='a' = 'b' --reject
             | h=='b' = state3 ("B"++init x) (last x) ("B"++y)  -- change head to blank and go back to start
             | h=='B' = 'a'-- must be odd number so true


state3 x h y | h=='B' = state0 'B' (head y) (tail y)--move one right to state0?
             | otherwise = state3 ("B"++init x) (last x) ([h]++y)  --move left


state4 x h y | h == 'B' = state5 ("B"++init x) (last x) ("B" ++ y)  --if blank move left 
             | otherwise = state4 ((tail x)++[h]) (head y) ((tail y)++"B") --carry on right otherwise
             
state5 x h y | h == 'a' = state3 ("B"++init x) (last x) ("B"++y)  -- change head to blank and go back to start
             | h == 'b'= 'b' -- first and end dont match so false
             | h == 'B' = 'a' -- odd all blanks so true