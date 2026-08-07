/- # Sized bitvectors for Verilog semantics.
   Uses Lean's `BitVec n` as the underlying representation. -/

namespace VerilLean.Lib

-- ## Core type

-- Sized bitvector with signedness flag.
structure SZ where
  width : Nat
  val   : BitVec width
  signed : Bool
  deriving Repr

instance : BEq SZ where
  beq a b := a.width == b.width && a.val.toNat == b.val.toNat && a.signed == b.signed

instance : Inhabited SZ where
  default := ⟨0, 0#0, false⟩

-- ## Notation: #{value, width, signed}

-- Construct an SZ from an integer value, width, and signedness.
def SZ.mk' (z : Int) (w : Nat) (s : Bool) : SZ :=
  ⟨w, BitVec.ofInt w z, s⟩

scoped notation "#{" z ", " w ", " s "}" => SZ.mk' z w s

-- ## Constants

def sznil : SZ := #{0, 0, false}
def szF0  : SZ := #{0, 1, true}
def szF1  : SZ := #{-1, 1, true}

def sz_int32 : Nat := 32
def sz_int32_nat : Nat := 32

-- ## Base conversion helpers

/- Convert a "binary-encoded decimal" Z to actual binary.
    E.g., `binaryZtoZ 4 101` interprets digits of 101 in base-10 as binary digits. -/
def binaryZtoZ : Nat → Int → Int
  | 0, _ => 0
  | n + 1, bz => (binaryZtoZ n (bz / 10)) * 2 + (bz % 10)

-- Convert an "octal-encoded decimal" Z to actual value.
def octalZtoZ : Nat → Int → Int
  | 0, _ => 0
  | n + 1, oz => (octalZtoZ n (oz / 10)) * 8 + (oz % 10)

-- ## Normalization / value extraction

-- Unsigned normalized value (mod 2^width). Already guaranteed by BitVec.
def SZ.normZ (sz : SZ) : Int := sz.val.toNat

-- Signed normalized value (sign-extended).
def SZ.normS (sz : SZ) : Int := sz.val.toInt

/- Normalized value respecting the signedness flag.
 -/
def SZ.norm (sz : SZ) : Int :=
  if sz.signed then sz.normS else sz.normZ

-- Check if the normalized value is zero.
def SZ.isZero (sz : SZ) : Bool := sz.norm == 0

-- ## Internal helpers

-- Construct a 1-bit boolean result.
private def boolSZ (b : Bool) : SZ :=
  ⟨1, if b then 1#1 else 0#1, false⟩

-- Apply a binary operation on the normalized Int values, producing result with given width/sign.
private def binIntOp (f : Int → Int → Int) (lv rv : SZ) (w : Nat) (s : Bool) : SZ :=
  SZ.mk' (f lv.norm rv.norm) w s

-- Max width of two SZs.
private def maxWidth (a b : SZ) : Nat := max a.width b.width

-- Both-signed flag.
private def bothSigned (a b : SZ) : Bool := a.signed && b.signed

-- ## Sign / unsigned conversion

def SZ.toSigned (sz : SZ) : SZ := { sz with signed := true }
def SZ.toUnsigned (sz : SZ) : SZ := { sz with signed := false }

-- ## Extension / truncation

-- Zero-extend to a new width.
def SZ.zeroExt (newWidth : Nat) (sz : SZ) : SZ :=
  ⟨newWidth, BitVec.ofNat newWidth sz.val.toNat, sz.signed⟩

-- Sign-extend to a new width.
def SZ.signExt (newWidth : Nat) (sz : SZ) : SZ :=
  ⟨newWidth, BitVec.ofInt newWidth sz.val.toInt, sz.signed⟩

-- Extract MSB portion (top `w` bits).
def SZ.msb (w : Nat) (sz : SZ) : SZ :=
  let shift := sz.width - w
  ⟨w, BitVec.ofNat w (sz.val.toNat >>> shift), sz.signed⟩

-- Extract LSB portion (bottom `w` bits).
def SZ.lsb (w : Nat) (sz : SZ) : SZ :=
  ⟨w, BitVec.ofNat w sz.val.toNat, sz.signed⟩

-- Cast to a new width given by the value of another SZ.
def SZ.castV (newWidthSZ : SZ) (sz : SZ) : SZ :=
  let nw := newWidthSZ.val.toNat
  if nw == sz.width then sz
  else if sz.width ≤ nw then
    SZ.mk' sz.norm nw sz.signed
  else  -- truncation
    sz.lsb nw

-- Cast to match another SZ's width and signedness.
def SZ.castD (target : SZ) (sz : SZ) : SZ :=
  if target.width == sz.width && target.signed == sz.signed then sz
  else
    let truncated := if sz.width ≤ target.width then
      SZ.mk' sz.norm target.width sz.signed
    else sz.lsb target.width
    { truncated with signed := target.signed }

-- ## Bit select / range

-- Select a single bit at index `i`.
def SZ.select (sz : SZ) (i : Int) : SZ :=
  boolSZ (sz.val.getLsbD i.toNat)

-- Extract a range [msb:lsb] (inclusive).
def SZ.range (sz : SZ) (msbi lsbi : Int) : SZ :=
  let lo := lsbi.toNat
  let hi := msbi.toNat
  let w := hi - lo + 1
  ⟨w, BitVec.ofNat w (sz.val.toNat >>> lo), false⟩

-- ## Concatenation

-- Concatenate two SZs (msb ## lsb). IEEE 11.8.1: result is unsigned.
def SZ.concat2 (msz lsz : SZ) : SZ :=
  let mw := msz.width
  let lw := lsz.width
  let w := mw + lw
  ⟨w, BitVec.ofNat w (msz.val.toNat <<< lw ||| lsz.val.toNat), false⟩

-- Concatenate a list of SZs.
def SZ.concat : List SZ → SZ
  | [] => sznil
  | sz :: rest => sz.concat2 (SZ.concat rest)

-- Replicate an SZ `n` times.
def SZ.rep (n : Nat) (sz : SZ) : SZ := SZ.concat (List.replicate n sz)

-- ## Unary operators

-- Unary minus: -v
def SZ.uMinus (v : SZ) : SZ := SZ.mk' (-v.val.toInt) v.width v.signed

-- Logical NOT: !v (1 if zero, 0 otherwise)
def SZ.uNot (v : SZ) : SZ := boolSZ v.isZero

-- Bitwise NOT: ~v
def SZ.uNeg (v : SZ) : SZ :=
  ⟨v.width, ~~~v.val, v.signed⟩

-- Reduction AND: &v (1 iff all bits are 1)
def SZ.uAnd (v : SZ) : SZ :=
  boolSZ (v.val.toInt == -1)

-- Reduction NAND: ~&v
def SZ.uNand (v : SZ) : SZ :=
  boolSZ (v.val.toInt != -1)

-- Reduction OR: |v (1 iff any bit is 1)
def SZ.uOr (v : SZ) : SZ := boolSZ (!v.isZero)

-- Reduction NOR: ~|v
def SZ.uNor (v : SZ) : SZ := boolSZ v.isZero

-- Reduction XOR: ^v (parity)
private def xorBits (n : Nat) (bv : BitVec n) : Bool :=
  let rec go (i : Nat) (acc : Bool) : Bool :=
    if i >= n then acc
    else go (i + 1) (xor acc (bv.getLsbD i))
  go 0 false

def SZ.uXor (v : SZ) : SZ := boolSZ (xorBits v.width v.val)

-- Reduction XNOR: ~^v
def SZ.uXnor (v : SZ) : SZ := boolSZ (!(xorBits v.width v.val))

-- ## Binary arithmetic operators

def SZ.bAdd (lv rv : SZ) : SZ :=
  binIntOp (· + ·) lv rv (maxWidth lv rv) (bothSigned lv rv)

def SZ.bSub (lv rv : SZ) : SZ :=
  binIntOp (· - ·) lv rv (maxWidth lv rv) (bothSigned lv rv)

def SZ.bMul (lv rv : SZ) : SZ :=
  binIntOp (· * ·) lv rv (maxWidth lv rv) (bothSigned lv rv)

def SZ.bDiv (lv rv : SZ) : SZ :=
  binIntOp (· / ·) lv rv (maxWidth lv rv) (bothSigned lv rv)

def SZ.bRem (lv rv : SZ) : SZ :=
  binIntOp (· % ·) lv rv (maxWidth lv rv) (bothSigned lv rv)

-- ## Binary comparison operators

-- Verilog equality (==): compares normalized values.
def SZ.equiv (a b : SZ) : Bool := a.norm == b.norm

def SZ.bEq  (lv rv : SZ) : SZ := boolSZ (lv.equiv rv)
def SZ.bNEq (lv rv : SZ) : SZ := boolSZ (!lv.equiv rv)

-- Four-state equality (===): compares raw bit values.
def SZ.bFEq  (lv rv : SZ) : SZ := boolSZ (lv.val.toNat == rv.val.toNat)
def SZ.bFNEq (lv rv : SZ) : SZ := boolSZ (lv.val.toNat != rv.val.toNat)

-- Wildcard equality (=?=, !?=): same as four-state for our 2-state model.
def SZ.bWEq  (lv rv : SZ) : SZ := boolSZ (lv.val.toNat == rv.val.toNat)
def SZ.bWNEq (lv rv : SZ) : SZ := boolSZ (lv.val.toNat != rv.val.toNat)

def SZ.bLAnd (lv rv : SZ) : SZ := boolSZ (!lv.isZero && !rv.isZero)
def SZ.bLOr  (lv rv : SZ) : SZ := boolSZ (!lv.isZero || !rv.isZero)

def SZ.bPow (lv rv : SZ) : SZ :=
  binIntOp (fun l r => (l.toNat ^ r.toNat : Nat)) lv rv lv.width (bothSigned lv rv)

def SZ.bLt (lv rv : SZ) : SZ := boolSZ (lv.norm < rv.norm)
def SZ.bLe (lv rv : SZ) : SZ := boolSZ (lv.norm ≤ rv.norm)
def SZ.bGt (lv rv : SZ) : SZ := boolSZ (lv.norm > rv.norm)
def SZ.bGe (lv rv : SZ) : SZ := boolSZ (lv.norm ≥ rv.norm)

-- ## Binary bitwise operators

private def bitwiseOperand (w : Nat) (signedResult : Bool) (v : SZ) : BitVec w :=
  if signedResult then BitVec.ofInt w v.normS
  else BitVec.ofNat w v.val.toNat

def SZ.bAnd (lv rv : SZ) : SZ :=
  let w := maxWidth lv rv
  let signedResult := bothSigned lv rv
  ⟨w, bitwiseOperand w signedResult lv &&& bitwiseOperand w signedResult rv, signedResult⟩

def SZ.bOr (lv rv : SZ) : SZ :=
  let w := maxWidth lv rv
  let signedResult := bothSigned lv rv
  ⟨w, bitwiseOperand w signedResult lv ||| bitwiseOperand w signedResult rv, signedResult⟩

def SZ.bXor (lv rv : SZ) : SZ :=
  let w := maxWidth lv rv
  let signedResult := bothSigned lv rv
  ⟨w, bitwiseOperand w signedResult lv ^^^ bitwiseOperand w signedResult rv, signedResult⟩

def SZ.bXnor (lv rv : SZ) : SZ :=
  let w := maxWidth lv rv
  let signedResult := bothSigned lv rv
  ⟨w, ~~~(bitwiseOperand w signedResult lv ^^^ bitwiseOperand w signedResult rv), signedResult⟩

-- ## Binary shift operators

-- Logical shift right (>>).
def SZ.bShr (lv rv : SZ) : SZ :=
  let amt := rv.normZ.toNat
  ⟨lv.width, BitVec.ofNat lv.width (lv.val.toNat >>> amt), lv.signed⟩

-- Logical shift left (<<).
def SZ.bShl (lv rv : SZ) : SZ :=
  let amt := rv.normZ.toNat
  ⟨lv.width, BitVec.ofNat lv.width (lv.val.toNat <<< amt), lv.signed⟩

-- Arithmetic shift right (>>>). Preserves sign.
def SZ.bSar (lv rv : SZ) : SZ :=
  let amt := rv.normZ.toNat
  SZ.mk' (lv.norm >>> amt) lv.width lv.signed

-- Arithmetic shift left (<<<). Preserves sign.
def SZ.bSal (lv rv : SZ) : SZ :=
  let amt := rv.normZ.toNat
  SZ.mk' (lv.norm <<< amt) lv.width lv.signed

-- ## Structural equality (for HMap matching)

-- Strict structural equality: same width, signedness, and value.
def SZ.eqStr (a b : SZ) : Bool :=
  a.width == b.width && a.signed == b.signed && a.equiv b

end VerilLean.Lib
