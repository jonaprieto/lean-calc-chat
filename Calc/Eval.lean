/-
Copyright (c) 2026 Jonathan Prieto-Cubides. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Claude
-/

import Grip

/-!
# Calc.Eval: the calculator behind the chat UI

The grammar is a [grip](https://github.com/jonaprieto/lean-grip) byte parser, so there is no
hand-rolled tokenizer and no `partial`: recursion goes through `GParser.fix`, and every
repetition goes through `GParser.many`, whose always-consume grade is checked at compile time.

Failures come back as grip's positioned `ParseError`, and `ParseError.pretty` renders the
`line:col: message` / source line / caret block that `Calc.View` shows in the transcript.

Values are `Float`. Lean's kernel does not reduce `Float`, so the self-check at the end of this
file is discharged by `native_decide` rather than `decide`.

```
expression := term (('+' | '-') term)*
term       := unary (('*' | '/' | '%') unary)*
unary      := ('-' | '+') unary | power
power      := atom ('^' unary)*
atom       := number | name | name '(' expression ')' | '(' expression ')'
```

`power` sitting below `unary` is what makes `-3^2` evaluate to `-9`; `power` taking a full
`unary` on its right is what makes `2^3^2` evaluate to `512` and `2^-2` parse at all.
-/

open Grip

namespace Calc

/-- An expression tree. -/
inductive Expr where
  /-- A literal number. -/
  | number (value : Float)
  /-- A named constant. -/
  | name (value : String)
  /-- A one-argument function call. -/
  | apply (function : String) (argument : Expr)
  /-- Arithmetic negation. -/
  | negate (inner : Expr)
  /-- A binary operator applied to two operands. -/
  | binary (operator : Char) (left right : Expr)
  deriving Repr, Inhabited

/-! ## Lexemes -/

private def digitsToFloat (digits : List Char) : Float :=
  digits.foldl (fun value character =>
    value * 10.0 + (character.toNat - '0'.toNat).toFloat) 0.0

/-- Decode a `digits[.digits]` lexeme. Rejects `.`, `1.2.3`, and anything else the byte scan
let through. -/
private def floatOfLexeme (text : String) : Option Float :=
  match text.splitOn "." with
  | [whole] =>
      if whole.isEmpty then none else some (digitsToFloat whole.toList)
  | [whole, fraction] =>
      if whole.isEmpty && fraction.isEmpty then none
      else
        some (digitsToFloat whole.toList +
          digitsToFloat fraction.toList / Float.pow 10.0 fraction.length.toFloat)
  | _ => none

private def decodeNumber (arr : ByteArray) (start stop : Nat) : Option Expr :=
  match String.fromUTF8? (arr.extract start stop) with
  | some text => (floatOfLexeme text).map Expr.number
  | none => none

private def isNumberByte (byte : UInt8) : Bool := Ascii.isDigit byte || byte == Ascii.dot

/-- A number literal, decoded straight from its consumed byte range. -/
private def number : GParser conditional Expr :=
  GParser.captureWith? decodeNumber (GParser.takeWhile1 isNumberByte) <?> "a number"

/-- A constant or function name. -/
private def identifier : GParser conditional String :=
  GParser.capture (GParser.takeWhile1 Ascii.isAlpha) <?> "a name"

private def operatorChar (byte : UInt8) : Char := Char.ofNat byte.toNat

private def addOperator : GParser conditional Char :=
  operatorChar <$> (GParser.ws *> GParser.oneOf [Ascii.plus, Ascii.dash]) <?> "'+' or '-'"

private def mulOperator : GParser conditional Char :=
  operatorChar <$>
    (GParser.ws *> GParser.oneOf [Ascii.code '*', Ascii.code '/', Ascii.code '%'])
    <?> "'*', '/' or '%'"

private def foldBinary (first : Expr) (rest : List (Char × Expr)) : Expr :=
  rest.foldl (fun left (operator, right) => .binary operator left right) first

/-! ## Grammar -/

/-- Parse one expression. -/
def expression : GParser conditional Expr :=
  GParser.fix fun expression =>
    let group : GParser conditional Expr :=
      GParser.ch '(' *> (GParser.ws *> expression) <* (GParser.ws *> GParser.ch ')')
    let call : GParser conditional Expr :=
      GParser.map2
        (fun name argument =>
          match argument with
          | none => Expr.name name
          | some argument => Expr.apply name argument)
        identifier (GParser.optional (GParser.ws *> group))
    let atom : GParser conditional Expr :=
      GParser.ws *> GParser.dispatch fun leading =>
        if leading == Ascii.lparen then group
        else if isNumberByte leading then number
        else call
    let unary : GParser conditional Expr :=
      GParser.fix fun unary =>
        let power : GParser conditional Expr :=
          GParser.map2
            (fun base rest => rest.foldl (fun left right => Expr.binary '^' left right) base)
            atom (GParser.many (GParser.ws *> GParser.ch '^' *> unary))
        GParser.ws *> GParser.dispatch fun leading =>
          if leading == Ascii.dash then Expr.negate <$> (GParser.ch '-' *> unary)
          else if leading == Ascii.plus then GParser.ch '+' *> unary
          else power
    let term : GParser conditional Expr :=
      GParser.map2 foldBinary unary
        (GParser.many (GParser.map2 (fun operator operand => (operator, operand))
          mulOperator unary))
    GParser.map2 foldBinary term
      (GParser.many (GParser.map2 (fun operator operand => (operator, operand))
        addOperator term))

/-- One expression and nothing else.

The label matters: `many` reports its failure at the offset where the repetition started, so a
bare "end of input" would blame the last operator the chain accepted. -/
def parser : GParser conditional Expr :=
  (GParser.ws *> expression) <*
    (GParser.ws *> (GParser.eof <?> "an operator or the end of the expression"))

/-- Parse a line of input, keeping grip's positioned error. -/
def parse (input : String) : Except ParseError Expr :=
  GParser.parse parser input.toUTF8

/-! ## Evaluating -/

private def applyFunction (function : String) (value : Float) : Except String Float :=
  match function with
  | "sqrt" => if value < 0.0 then .error "sqrt needs a non-negative argument" else .ok value.sqrt
  | "abs" => .ok value.abs
  | "floor" => .ok value.floor
  | "ceil" => .ok value.ceil
  | "round" => .ok value.round
  | "ln" => if value ≤ 0.0 then .error "ln needs a positive argument" else .ok value.log
  | "exp" => .ok value.exp
  | "sin" => .ok value.sin
  | "cos" => .ok value.cos
  | "tan" => .ok value.tan
  | _ => .error s!"'{function}' is not a function I know"

private def applyOperator (operator : Char) (left right : Float) : Except String Float :=
  match operator with
  | '+' => .ok (left + right)
  | '-' => .ok (left - right)
  | '*' => .ok (left * right)
  | '/' => if right == 0.0 then .error "division by zero" else .ok (left / right)
  | '%' =>
      if right == 0.0 then .error "modulo by zero"
      else .ok (left - right * (left / right).floor)
  | '^' => .ok (Float.pow left right)
  | _ => .error s!"'{operator}' is not an operator I know"

/-- Evaluate an expression tree. `ans` supplies the value of the `ans` constant. -/
def eval (ans : Float) : Expr → Except String Float
  | .number value => .ok value
  | .name "ans" => .ok ans
  | .name "pi" => .ok 3.141592653589793
  | .name "tau" => .ok 6.283185307179586
  | .name "e" => .ok 2.718281828459045
  | .name value => .error s!"'{value}' is not a name I know"
  | .apply function argument => do applyFunction function (← eval ans argument)
  | .negate inner => do .ok (-(← eval ans inner))
  | .binary operator left right => do
      applyOperator operator (← eval ans left) (← eval ans right)

inductive EvalError where
  | parse (error : ParseError)
  | evaluation (message : String)

/-- Parse and evaluate while preserving parser errors for rich source diagnostics. -/
def evaluateDetailed (ans : Float) (input : String) : Except EvalError Float :=
  match parse input with
  | .error error => .error (.parse error)
  | .ok expression =>
      match eval ans expression with
      | .ok value => .ok value
      | .error message => .error (.evaluation message)

/-- Parse and evaluate one line, using the legacy plain-string error shape. -/
def evaluate (ans : Float) (input : String) : Except String Float :=
  match evaluateDetailed ans input with
  | .ok value => .ok value
  | .error (.parse error) => .error (error.pretty input.toUTF8)
  | .error (.evaluation message) => .error message

/-! ## Display -/

private def trimTrailingZeros (text : String) : String :=
  if text.contains '.' then
    let stripped := text.toList.reverse.dropWhile (· == '0')
    let stripped := match stripped with
      | '.' :: rest => rest
      | _ => stripped
    String.ofList stripped.reverse
  else
    text

/-- Format a result the way a calculator display would: no trailing zeros, named infinities. -/
def formatValue (value : Float) : String :=
  if value.isNaN then "nan"
  else if value.isInf then (if value < 0.0 then "-inf" else "inf")
  else trimTrailingZeros (toString value)

/-! ## Self-check

One runnable check over the parts with real branching: precedence, both associativities, unary
minus, the function table, and each failure path.
-/

private def evaluatesTo (input expected : String) : Bool :=
  match evaluate 0.0 input with
  | .ok value => formatValue value == expected
  | .error _ => false

private def fails (input : String) : Bool :=
  match evaluate 0.0 input with
  | .ok _ => false
  | .error _ => true

private def selfCheck : Bool :=
  evaluatesTo "2+3*4" "14" &&
  evaluatesTo "(2+3)*4" "20" &&
  evaluatesTo "2^3^2" "512" &&
  evaluatesTo "-3^2" "-9" &&
  evaluatesTo "2^-2" "0.25" &&
  evaluatesTo "10/4" "2.5" &&
  evaluatesTo "7%3" "1" &&
  evaluatesTo "1.5 + 0.5" "2" &&
  evaluatesTo "sqrt(16)" "4" &&
  evaluatesTo "abs(0-5)" "5" &&
  evaluatesTo "round(2.4) + ceil(2.1) + floor(2.9)" "7" &&
  evaluatesTo "  8  -  3  -  2 " "3" &&
  fails "2+" &&
  fails "1/0" &&
  fails "(1+2" &&
  fails "nope(1)" &&
  fails "2 $ 3" &&
  fails "1.2.3" &&
  fails ""

example : selfCheck = true := by native_decide

end Calc
