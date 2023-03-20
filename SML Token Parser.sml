(* Defining the different types of tokens that will be parsed*)
datatype token = ID of string | EQ | PL | MI | TI | DI;

fun parse(inputFile) =
    let 
        val input = TextIO.openIn inputFile
        
(* Read input from the file and append characters to a list
    as long as they are alphabetical *)
        fun read(input) =
            case TextIO.input1(input) of
                NONE => []
                |SOME c => if Char.isAlpha(c) 
                then c :: (read(input)) 
                else []

        fun helper(input,acc) =  
            case TextIO.input1(input) of
                NONE => (TextIO.closeIn(input); acc)
              | SOME c => 
                    if Char.isAlpha(c) then
                        let 
                            val word = c :: (read(input)) (* relating back to read function *)
                        in  
                            (* takes the word in char list form and turns into string *)
                            (* concats ID "STRING" to the accumilation list *)
                            helper(input, acc @ [ID (implode(word))])
                        end
                    else
                        (* Pattern Matching for each Special Symbol *)
                        (* Takes in input and then concats to acc *)
                        case c of 
                          #"=" => helper(input, acc @ [EQ])
                        | #"+" => helper(input, acc @ [PL])
                        | #"-" => helper(input, acc @ [MI])
                        | #"*" => helper(input, acc @ [TI])
                        | #"/" => helper(input, acc @ [DI])
                        | _ => 
                                if Char.isDigit(c) then 
                                (* Needs to return empty list aswell to get around the mix matched types *)
                                (TextIO.closeIn(input); print "\nCompilation error\n"; [])
                                else
                                helper(input, acc)
    in  
    (* Calling function *)    
        helper(input, [])
    end



