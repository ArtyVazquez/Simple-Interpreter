open System

type Operator = PLUS | MINUS | TIMES | DIV

type Name = String

type Expression =
 | IntConstant of int32
 | BinOp of Operator * Expression * Expression
 | Let of Name * Expression * Expression
 | Var of Name
 | Eq of Expression * Expression
 | Neq of Expression * Expression
 | If of Expression * Expression * Expression
 | FunctionDecl of Name * Name list * Expression * Expression
 | FunctionCall of Name * Expression list
 
type Value =
 | IntValue of int32
 | BoolValue of bool
 
type Option =      
   | Some of Value         
   | None 
    
let rec eval (c:Expression) (vars:Map<Name, Option>) (funs:Map<Name, (Name list * Expression)>) : Option =
 match c with
 | IntConstant(value) -> Some (IntValue value)
 | BinOp(op, left, right) -> match eval left vars funs, eval right vars funs with
                             | Some (IntValue l), Some (IntValue r) -> match op with
                                                                       | PLUS -> Some(IntValue(l+r))
                                                                       | MINUS -> Some(IntValue(l-r))
                                                                       | TIMES -> Some(IntValue(l*r))
                                                                       | DIV -> if r = 0 then None
                                                                                else Some(IntValue(l/r))
                             | _, _ -> None
 | Var variable -> if (vars.TryFind (variable)).IsNone then None
                   else (vars.TryFind (variable)).Value
 | Let (varName, value, body) -> match eval value vars funs with
                                      | Some o -> eval body (vars.Add( varName, Some o)) funs
                                      | _ -> None
 | Eq (left, right) -> match eval left vars funs, eval right vars funs with
                       | Some l, Some r -> Some(BoolValue(l=r))
                       | _ -> None
 | Neq (left, right) -> match eval left vars funs, eval right vars funs with
                        | Some l, Some r -> Some(BoolValue(l<>r))
                        | _ -> None
 | If (cond, thenSide, elseSide) -> match eval cond vars funs with
                                    | Some (BoolValue o)  -> if o = true then eval thenSide vars funs
                                                             else eval elseSide vars funs
                                    | _ -> None
 | FunctionDecl (funName, formalArgs, funBody, scope) -> (eval scope vars (funs.Add(funName, (formalArgs, funBody))))
 | FunctionCall (funName, actualArguments) -> let evalArgs = List.map (fun x -> (eval x vars funs)) actualArguments
                                              if (funs.TryFind(funName).IsNone)
                                                then None
                                              else 
                                                  let (argsName, b) = (funs.TryFind(funName).Value)
                                                  let varMapArgs = List.map2 (fun x y -> (x, y)) argsName evalArgs
                                                  eval b (Map(Seq.concat[(Map.toSeq vars); (Map.toSeq (Map.ofList varMapArgs))])) funs
                                              
(*..............................Programs..............................................................................*)

let p = BinOp(PLUS,
         IntConstant(400),
         IntConstant(74))

let p1 = IntConstant(474)

let p2 = BinOp(
             DIV,
             BinOp(PLUS,
                 IntConstant(400),
                 IntConstant(74)),
                 IntConstant(3))

let p3 =
    Eq(
        BinOp(DIV,
            BinOp(PLUS,
                IntConstant(400),
                IntConstant(74)),
                IntConstant(3)),
            IntConstant(158))

let p4 = If(
            Eq(BinOp(DIV,
                    BinOp(PLUS,
                        IntConstant(400),
                        IntConstant(74)),
                        IntConstant(3)),
                IntConstant(158)),
            IntConstant(474),
            BinOp(DIV,
                  IntConstant(474),
                  IntConstant(0)))

let p5 = Let("bot",
            IntConstant (3),
            BinOp(
                PLUS,
                Let("bot",
                    IntConstant(2),
                    Var "bot"),
                 If(Eq(Var "bot",
                        IntConstant(0)),
                    BinOp(DIV,
                        IntConstant(474),
                        IntConstant(0)),
                    BinOp(DIV,
                        BinOp(PLUS,
                            IntConstant(400),
                            IntConstant(74)),
                            Var "bot"))))

(*
            function f(top,bot) : FunctionDecl (funName, formalArgs, funBody, scope)
             if (bot == 0) then 0 else top/bot
            let bot = 3 in
             (let bot = 2 in bot)
             +
             (f(400+74,bot) + f(470+4,0))
             
            • Constants
            • Binary operation
            • Comparison
            • Conditional execution
            • Variables
            • Functions
*)
let p6 = FunctionDecl("f",
                      ["top";"bot"],
                      If(Eq( Var "bot",
                            IntConstant(0)),
                            IntConstant(0),
                            BinOp(
                                  DIV,
                                  Var "top",
                                  Var "bot")),
                      Let ("bot",
                          IntConstant(3),
                          BinOp(PLUS,
                                Let ("bot",
                                     IntConstant(2),
                                     Var "bot"),
                                BinOp(PLUS,
                                    FunctionCall("f", [BinOp(PLUS, IntConstant(400), IntConstant(74)); Var "bot"]),
                                    FunctionCall("f", [BinOp(PLUS, IntConstant(400), IntConstant(4)); IntConstant(0)])))))

let fac10 = FunctionDecl("fact", ["n"],
                                If (Eq(Var "n",
                                       IntConstant(1)),
                                    IntConstant(1),
                                    BinOp(
                                        TIMES,
                                        Var "n",
                                        FunctionCall("fact",
                                                     [BinOp(MINUS, Var "n", IntConstant(1))]))),
                                FunctionCall( "fact",
                                             [IntConstant(10)]))


// Lexical(static) scoping
let scope = Let("var",
                IntConstant(400),
                BinOp(PLUS,
                      Let("var",
                          IntConstant(70),
                          Var "var"),
                      BinOp(PLUS,
                            IntConstant(4),
                            Var "var")))

let funCap = Let ("var",
                  IntConstant(2),
                  FunctionDecl( "ReadVar", [],
                                Var "var",
                                FunctionCall( "ReadVar",[])))

printfn "Value: %A" (eval p1 (Map []) (Map [])) // Result ----> 474
printfn "Value: %A" (eval p2 (Map []) (Map [])) // Result ----> 158
printfn "Value: %A" (eval p3 (Map []) (Map [])) // Result ----> True
printfn "Value: %A" (eval p4 (Map []) (Map [])) // Result ----> 474
printfn "Value: %A" (eval p5 (Map []) (Map [])) // Result ----> 160
printfn "Value: %A" (eval p6 (Map []) (Map [])) // Result ----> 160
printfn "Value: %A" (eval fac10 (Map []) (Map [])) // Result ---> 3628800
printfn "Value: %A" (eval scope (Map []) (Map [])) // Scope
printfn "Value: %A" (eval funCap (Map []) (Map [])) // Result ---> 2