*Main Data.List.Split Data.Char Data.String Data.Array> :r
[1 of 1] Compiling Main             ( C:\iban.hs, interpreted )
Ok, modules loaded: Main.
*Main Data.List.Split Data.Char Data.String Data.Array> iban "NL39 RABO 0300 0652 64"
True
*Main Data.List.Split Data.Char Data.String Data.Array> iban "NL39 RABO 0300 0652 640"
False
*Main Data.List.Split Data.Char Data.String Data.Array> iban "NL39 RABO 0300 0652 64p"
False
*Main Data.List.Split Data.Char Data.String Data.Array> iban "NL39 rabo 0300 0652 64p"
False
*Main Data.List.Split Data.Char Data.String Data.Array> iban "NL39 rabo 0300 0652 64"
True