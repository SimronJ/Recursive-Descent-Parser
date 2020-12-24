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

let grammar = parseGrammarString """
    S → T | ( S + T )
    T → a
"""
printfn "%A" grammar


// Show grammar rules
printGrammar grammar

/// Start with a production S → T | ( S + T )
/// Let's enumerate rules as follows
/// 1: S → T
/// 2: S → ( S + T )
/// because RHS productions are pairwise disjoint, the rule #2 is always starts with matching `(`
let rec ProdS (cnxt:Tokenizer) =
    // check the current token is `(` then select rule #2
    if cnxt.CurrentToken = LPAR then
        // 2: S -> ( S + T )
        cnxt.NextToken() @(2)==> ProdS ==> Match PLUS ==> ProdT ==> Match RPAR 
    else
        // 1: S -> T
        cnxt @(1)==> ProdT 

/// The function for production T → a is straight forward: match nonterminal `a`
and ProdT (cnxt:Tokenizer) =
    // 3: T -> a
    cnxt @(3)==> Match A

/// For each terminal symbol compare it with a current token
/// and if they match, continue with the next token, else there is an error
and Match term cnxt =
    if cnxt.IsVerbose then printfn "Match %A with %A" term cnxt.CurrentToken
    // if we matched the current token with a terminal symbol
    if term = cnxt.CurrentToken then
        cnxt.NextToken() // read next token
    else
        failwith (sprintf "Cannot match symbol `%A` with `%A`" term cnxt.CurrentToken)
    
/// Start parsing by calling starting symbol function
let parser (cnxt:Tokenizer) :Tokenizer =    
    // Read token and pass it to the function for S rule
    cnxt.NextToken() ==> ProdS

let inputString = "((a+a)+a)"
inputString |> Seq.toList |> CSCI374.Lexer.tokenize |> printfn "%A"

Tokenizer(grammar, InputState = inputString) |> parser |> ignore

Tokenizer(grammar, true, InputState="((a+)+a)") |> parser |> ignore

