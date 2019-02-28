module PrinterUtility where

data Iseq = INil | IStr String | IAppend Iseq Iseq | IIndent Iseq | INewline

-- ------------------- --
-- FUNCTIONS WITH ISEQ --
-- ------------------- --

iNil :: Iseq                        -- The empty iseq
iNil = INil

iStr :: String -> Iseq              -- Turn a string into an iseq
iStr str = IStr str

iAppend :: Iseq -> Iseq -> Iseq     -- Append two iseqs
iAppend seq1 seq2 = IAppend seq1 seq2

iNewline :: Iseq                    -- New line with indentation
iNewline = INewline

iIndent :: Iseq -> Iseq             -- Indent an iseq
iIndent seq = IIndent seq

iDisplay :: Iseq -> String          -- Turn an iseq into a string
iDisplay seq = flatten 0 [(seq,0)]

iConcat :: [Iseq] -> Iseq           -- Converts a list of iseq into a single iseq
iConcat = foldr iAppend iNil

iInterleave :: Iseq -> [Iseq] -> Iseq  -- Interleaves an iseq in a list of iseq
iInterleave x (y:ws) = if null ws then y else y `iAppend` x `iAppend` iInterleave x ws
iInterleave x [] = INil

flatten :: Int              -- Current column
           -> [(Iseq, Int)] -- Work list (Iseq with related indentation)
           -> String        -- Result
flatten _ [] = ""
flatten col ((INewline, indent) : seqs) -- we need to perform indentation in the new line
    = '\n' : replicate indent ' ' ++ flatten indent seqs -- indent is the new current column
flatten col ((IIndent seq, _) : seqs) -- we need to indent to the current column
    = flatten col ((seq, col) : seqs) -- simply sets the current indentation from the current column
flatten col ((INil,_) : seqs) = flatten col seqs
flatten col ((IStr s,_) : seqs) 
    = s ++ flatten (col + length s) seqs -- current column is updated
flatten col ((IAppend seq1 seq2,i) : seqs) 
    = flatten col ((seq1,i) : (seq2,i) : seqs)