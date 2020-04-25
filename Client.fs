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
        // Elsõ két érték amit látunk a számólógépen
        let mutable inputTemp = Var.Create "0"
        let mutable inputResult = Var.Create ""
        
        // Egy szám mindig szerepelni fog a felsõ mezõben
        let ClearTemp() = 
            inputTemp.Value <- "0"
        
        // Alsó mezõben viszont nem kötelezõ rakni még számot, mivel még nem biztos hogy van érték amivel tudunk szorozni/osztani
        let ClearResult() = 
            inputResult.Value <- ""
        
        // Melyik mûveletet végzi el a számológép legközelebb
        // + - / *
        let mutable inputEquation = Var.Create "="

        let ClearOnputEquation() = 
            inputEquation.Value <- "="

        // 0-9 buttonok lenyomásakor -1 a vesszõ (,)
        let inputAdd(x:int) =
            
            // Vesszõ beillesztés
            if(x = -1) then 
                if(inputTemp.Value.Contains(".") = false) then 
                    inputTemp.Value <- inputTemp.Value + "."
            
            // Ha 0 van beírva, akkor nem akarunk több nullát beilleszteni, 
            // viszont ha már van egy szám, akkor mögé illeszti a nullát és a többi számot is
            else if(inputTemp.Value <> "0" && x = 0) then 
                inputTemp.Value <- inputTemp.Value + "0"
            else
                
                // Ha csak 0 van beírva
                if(inputTemp.Value = "0") then 
                    inputTemp.Value <- (x |> string)
                else 
                    inputTemp.Value <- inputTemp.Value + (x |> string)
                    
        
        // Számolás
        let TempToResult() = inputResult.Value <- inputTemp.Value
        
        // Az = gomb lenyomása utána következik a számolás, 
        // ha nagyon C#-osan akartam volna megoldani, akkor a button-t beleraktam volna a változó nevébe.
        let calculateResult(x:string) =
            
            if(x="=") then 

                // Ha az elõzõ billenytû lenyomás is az = volt, akkor csak beilleszti azt a számot, ami felül szerepel
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
                    
                    // Ékezetes betûk esetében nem megfelelõ a kiírás, lehet UTF-el van a baj
                    // https://forums.websharper.com/blog/79902 - Tanárúr példája alapján!
                    sprintf "Nem lehet nullával osztani!"
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
                        
            // Korábbi teszteléskor hagytam benne, mivel volt hogy nem jelezte ki a számológép az elõzõ lenyomott billetyû ikonját
            if(x<>"") then ClearTemp()

            // Ha "nem egy szám" lesz az eredmény, tehát ha pl.: 0-át 0-val osztunk, nem pedig pl.: 1-et a 0-val, akkor más hibaeset keletkezik
            if(inputResult.Value = "NaN") then
                sprintf "Hiba - Nincs eredménye a számolásnak!"
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
            
            // 1/x, remélhetõleg ez kellett
            .MultiplicativeInverse(fun _ ->
                
                inputEquation.Value <- "1/x"
                calculateResult(inputEquation.Value)
            )
            .Doc()
        |> Doc.RunById "main"
