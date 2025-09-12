namespace VerilLean.Lang.Syntax

-- # Expressions

/-
decimal_number ::=
(v)   unsigned_number
(v) | [ size ] decimal_base unsigned_number
    | [ size ] decimal_base x_digit { _ }
    | [ size ] decimal_base z_digit { _ }
-/
inductive DecimalNumber where
| unsigned_number (v: Nat)
| size_unsigned_number (sz: Option Nat) (v: Nat)
deriving Repr
