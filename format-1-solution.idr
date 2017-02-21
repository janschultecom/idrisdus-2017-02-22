data Format = Number Format | 
              Str Format  |
              Lit String Format |
              End


PrintfType : Format -> Type
PrintfType (Number fmt) = (i:Int) -> PrintfType fmt
PrintfType (Str fmt) = (s:String) -> PrintfType fmt 
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String

printfFmt : (fmt:Format) -> (acc:String) -> PrintfType fmt 
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i) 
printfFmt (Str fmt) acc = \s => printfFmt fmt (acc ++ s) 
printfFmt (Lit str fmt) acc = printfFmt fmt (acc ++ str) 
printfFmt End acc = acc 

toFormat : (xs : List Char) -> Format
toFormat ('%' :: 'i' :: xs) = Number (toFormat xs)
toFormat ('%' :: 's' :: xs) = Str (toFormat xs)
toFormat (x :: xs) = let y = toFormat xs in
                         case y of 
                              Lit str fmt => Lit (strCons x str) fmt
                              fmt => Lit (strCons x "") fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ "" 

