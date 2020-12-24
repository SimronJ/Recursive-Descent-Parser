// Load some auxiliary tools
#load "grammartools.fsx"
open CSCI374.GrammarTools
open CSCI374.ParserTypes

type Tokenizer(grammar: PRODUCTION [], verbose: bool) =
    let mutable inputState = []
    let mutable curentToken = INVALID

    // Access to the
    member this.CurrentToken = curentToken    
    member this.NextToken() =
        let tkn, input = CSCI374.Lexer.token inputState
        inputState <- input
        curentToken <- tkn
        this
    
    member this.InputState
        with set(str) = inputState <- Seq.toList str
    member this.IsVerbose = verbose
    member this.PrintRule ruleIdx =
        printGrammarRule false grammar ruleIdx // print rule
    new(grammar) = Tokenizer(grammar, false)

/// This infix operator function provides verbose output while calling
/// a particular production rule
let (==>) (cnxt:Tokenizer) (prod:Tokenizer->Tokenizer) =
    if cnxt.IsVerbose then
        printfn "Enter <%A> with token `%A`" prod cnxt.CurrentToken
    let nextcnxt = prod cnxt
    if cnxt.IsVerbose then
        printfn "Exit <%A> with token `%A`" prod cnxt.CurrentToken
    nextcnxt
    
/// This infix operator function will allow to print a production rule
/// call `cnxt @ 2` will print second grammar rule
let (@) (cnxt:Tokenizer) ruleIdx =
    cnxt.PrintRule ruleIdx
    cnxt

let grammarfake = parseGrammarString """
S -> eaf | eUT
T -> e
U -> UcS | ae
"""
let grammar = parseGrammarString """
S -> eaf | eUT
T -> e
U -> aeV
V -> cSV | ε
"""
printfn "%A" grammar

// Show grammar rules
printGrammar grammar

let rec ProdS (cnxt:Tokenizer) =
    // check the current token is `E` then move to next token because S -> eaf | eUT
    if cnxt.CurrentToken = E then
        cnxt.NextToken() |> ignore
        if cnxt.CurrentToken = A then
        // 1: S → eaf
         cnxt @(1)==> Match A ==> Match F 
        else
            // 2: S → eUT
         cnxt @(2)==> ProdU ==> ProdT
    else
        cnxt
/// The function for production T → e is straight forward: match nonterminal `e`
and ProdT (cnxt:Tokenizer) =
    // 3: T -> e
    cnxt @(3)==> Match E

and ProdU (cnxt:Tokenizer) =
    // 4: U → aeV
    cnxt @(4)==> Match A ==> Match E ==> ProdV

and ProdV (cnxt:Tokenizer) =
    if cnxt.CurrentToken = C then
        //5: V → cSV
        cnxt.NextToken() @(5) ==> ProdS ==> ProdV
    else
        //6: V → ε
        cnxt @(6) ==> Match EPS 

/// For each terminal symbol compare it with a current token
/// and if they match, continue with the next token, else there is an error
and Match term cnxt =
    if cnxt.IsVerbose then printfn "Match %A with %A" term cnxt.CurrentToken
    //printf "The Term `%A` and the current Token `%A`" term cnxt.CurrentToken
    // if we matched the current token with a terminal symbol
    if term = cnxt.CurrentToken then
        cnxt.NextToken() // read next token
    else
        failwith (sprintf "Cannot match symbol `%A` with `%A`" term cnxt.CurrentToken)
    
/// Start parsing by calling starting symbol function
let parser (cnxt:Tokenizer) :Tokenizer =    
    // Read token and pass it to the function for S rule
    cnxt.NextToken() ==> ProdS


let inputString = "eaeceafceaeceafee"
inputString |> Seq.toList |> CSCI374.Lexer.tokenize |> printfn "%A"

Tokenizer(grammar, InputState=inputString) |> parser |> ignore

Tokenizer(grammar, true, InputState="eaeceafceaeceafee") |> parser |> ignore
Tokenizer(grammar, true, InputState="eaeceaeee") |> parser |> ignore
Tokenizer(grammar, true, InputState="eaeceaeeceafe") |> parser |> ignore
Tokenizer(grammar, true, InputState="eaeceafceaeeceafe") |> parser |> ignore
Tokenizer(grammar, true, InputState="eaeceafceaeee") |> parser |> ignore