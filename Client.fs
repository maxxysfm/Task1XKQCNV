namespace Task1XKQCNV

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating

[<JavaScript>]
module Client =
    // The templates are loaded from the DOM, so you just can edit index.html
    // and refresh your browser, no need to recompile unless you add or remove holes.
    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    [<SPAEntryPoint>]
    let Main () =
        // Els� k�t �rt�k amit l�tunk a sz�m�l�g�pen
        let mutable inputTemp = Var.Create "0"
        let mutable inputResult = Var.Create ""
        
        // Egy sz�m mindig szerepelni fog a fels� mez�ben
        let ClearTemp() = 
            inputTemp.Value <- "0"
        
        // Als� mez�ben viszont nem k�telez� rakni m�g sz�mot, mivel m�g nem biztos hogy van �rt�k amivel tudunk szorozni/osztani
        let ClearResult() = 
            inputResult.Value <- ""
        
        // Melyik m�veletet v�gzi el a sz�mol�g�p legk�zelebb
        // + - / *
        let mutable inputEquation = Var.Create "="

        let ClearOnputEquation() = 
            inputEquation.Value <- "="

        // 0-9 buttonok lenyom�sakor -1 a vessz� (,)
        let inputAdd(x:int) =
            
            // Vessz� beilleszt�s
            if(x = -1) then 
                if(inputTemp.Value.Contains(".") = false) then 
                    inputTemp.Value <- inputTemp.Value + "."
            
            // Ha 0 van be�rva, akkor nem akarunk t�bb null�t beilleszteni, 
            // viszont ha m�r van egy sz�m, akkor m�g� illeszti a null�t �s a t�bbi sz�mot is
            else if(inputTemp.Value <> "0" && x = 0) then 
                inputTemp.Value <- inputTemp.Value + "0"
            else
                
                // Ha csak 0 van be�rva
                if(inputTemp.Value = "0") then 
                    inputTemp.Value <- (x |> string)
                else 
                    inputTemp.Value <- inputTemp.Value + (x |> string)
                    
        
        // Sz�mol�s
        let TempToResult() = inputResult.Value <- inputTemp.Value
        
        // Az = gomb lenyom�sa ut�na k�vetkezik a sz�mol�s, 
        // ha nagyon C#-osan akartam volna megoldani, akkor a button-t beleraktam volna a v�ltoz� nev�be.
        let calculateResult(x:string) =
            
            if(x="=") then 

                // Ha az el�z� billenyt� lenyom�s is az = volt, akkor csak beilleszti azt a sz�mot, ami fel�l szerepel
                TempToResult()
            
            if(x="+") then 
                if(inputResult.Value = "") then TempToResult()
                else inputResult.Value <- (float inputResult.Value + float inputTemp.Value).ToString()
            
            if(x="-") then 
                if(inputResult.Value = "") then TempToResult()
                else inputResult.Value <- (float inputResult.Value - float inputTemp.Value).ToString()
            
            if(x="*") then 
                if(inputResult.Value = "") then TempToResult()
                else inputResult.Value <- (float inputResult.Value * float inputTemp.Value).ToString()
            
            if(x="/") then 
                if(inputResult.Value = "") then TempToResult()
                else inputResult.Value <- (float inputResult.Value / float inputTemp.Value).ToString()
                if(inputResult.Value ="Infinity") then 
                    
                    // �kezetes bet�k eset�ben nem megfelel� a ki�r�s, lehet UTF-el van a baj
                    // https://forums.websharper.com/blog/79902 - Tan�r�r p�ld�ja alapj�n!
                    sprintf "Nem lehet null�val osztani!"
                        |> JS.Alert
            
            if(x="Sin") then 
                if(inputResult.Value = "") then TempToResult()
                inputResult.Value <- (Math.Sin(float inputResult.Value)).ToString()
                ClearOnputEquation()
            
            if(x="Cos") then 
                if(inputResult.Value = "") then TempToResult()
                inputResult.Value <- (Math.Cos(float inputResult.Value)).ToString()
                ClearOnputEquation()
            
            if(x="1/x") then 
                if(inputResult.Value = "") then TempToResult()
                inputResult.Value <- (float 1 / float inputResult.Value).ToString()
                ClearOnputEquation()
                        
            // Kor�bbi tesztel�skor hagytam benne, mivel volt hogy nem jelezte ki a sz�mol�g�p az el�z� lenyomott billety� ikonj�t
            if(x<>"") then ClearTemp()

            // Ha "nem egy sz�m" lesz az eredm�ny, teh�t ha pl.: 0-�t 0-val osztunk, nem pedig pl.: 1-et a 0-val, akkor m�s hibaeset keletkezik
            if(inputResult.Value = "NaN") then
                sprintf "Hiba - Nincs eredm�nye a sz�mol�snak!"
                    |> JS.Alert
        
        IndexTemplate.Main()
            .Input(inputTemp)
            .InputResult(inputResult)
            .Equation(inputEquation)
            .Clear(fun _ ->
                ClearTemp()
            )
            .AllClear(fun _ ->
                ClearOnputEquation()
                ClearTemp()
                ClearResult()
            )
            .CopyResult(fun _ ->
                inputTemp.Value <- inputResult.Value
            )
            .Add_comma(fun _ ->
                inputAdd(-1)
            )            
            .Add_0(fun _ ->
                inputAdd(0)
            )
            .Add_1(fun _ ->
                inputAdd(1)
            )
            .Add_2(fun _ ->
                inputAdd(2)
            )
            .Add_3(fun _ ->
                inputAdd(3)
            )            
            .Add_4(fun _ ->
                inputAdd(4)
            )
            .Add_5(fun _ ->
                inputAdd(5)
            )
            .Add_6(fun _ ->
                inputAdd(6)
            )
            .Add_7(fun _ ->
                inputAdd(7)
            )            
            .Add_8(fun _ ->
                inputAdd(8)
            )
            .Add_9(fun _ ->
                inputAdd(9)
            )
            .Calculate(fun _ ->
                
                calculateResult(inputEquation.Value)
                inputEquation.Value <- "="
            )
            .Plus(fun _ ->
                
                calculateResult(inputEquation.Value)
                inputEquation.Value <- "+"
            )
            .Minus(fun _ ->
                
                calculateResult(inputEquation.Value)
                inputEquation.Value <- "-"
            )
            .Multiplication(fun _ ->
                
                calculateResult(inputEquation.Value)
                inputEquation.Value <- "*"
            )
            .Division(fun _ ->
                
                calculateResult(inputEquation.Value)
                inputEquation.Value <- "/"
            )
            .Sin(fun _ ->
                
                inputEquation.Value <- "Sin"
                calculateResult(inputEquation.Value)
            )
            .Cos(fun _ ->
                
                inputEquation.Value <- "Cos"
                calculateResult(inputEquation.Value)
            )
            
            // 1/x, rem�lhet�leg ez kellett
            .MultiplicativeInverse(fun _ ->
                
                inputEquation.Value <- "1/x"
                calculateResult(inputEquation.Value)
            )
            .Doc()
        |> Doc.RunById "main"
