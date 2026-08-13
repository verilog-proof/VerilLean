-- Hierarchical maps for Verilog semantics.

import VerilLean.Lib.SZ
import VerilLean.Lang.Syntax

namespace VerilLean.Lib

open VerilLean.Lang.Syntax (VId)

-- ## Core type

inductive HMap where
  | empty
  | bits (b : SZ)
  | arr (vs : List (Int × HMap))
  | str (fields : List (String × HMap))
  deriving Inhabited, Repr, BEq

-- ## Basic accessors

def HMap.isEmpty : HMap → Bool
  | .empty => true
  | _ => false

def HMap.isBits : HMap → Bool
  | .bits _ => true
  | _ => false

-- Extract the SZ from an HMap.bits, or default.
def hbits : HMap → SZ
  | .bits b => b
  | _ => default

-- Extract the SZ from an HMap.bits, as Option.
def hbitsO : HMap → Option SZ
  | .bits b => some b
  | _ => none

-- ## Array select / range

-- Lookup by Int key in an association list.
def hselectA : Int → List (Int × HMap) → HMap
  | _, [] => .empty
  | i, (k, v) :: rest => if i == k then v else hselectA i rest

/- Select from an HMap by SZ index.
    For bits: bit-select. For arr: array lookup. -/
def hselect (h : HMap) (i : SZ) : HMap :=
  match h with
  | .bits b => .bits (b.select i.norm)
  | .arr vs => hselectA i.norm vs
  | _ => .empty

def hselectO (h : HMap) (i : SZ) : Option HMap :=
  match h with
  | .bits b => some (.bits (b.select i.norm))
  | .arr vs =>
    match vs.find? (fun p => p.1 == i.norm) with
    | some (_, v) => some v
    | none => none
  | _ => none

-- Filter array entries whose key is in [lo, hi].
def hrangeA (lo hi : Int) : List (Int × HMap) → List (Int × HMap)
  | [] => []
  | (k, v) :: rest =>
    if lo ≤ k && k ≤ hi then (k, v) :: hrangeA lo hi rest
    else hrangeA lo hi rest

/- Range extraction from an HMap.
    For bits: bit-range. For arr: filter by index range. -/
def hrange (h : HMap) (msbi lsbi : SZ) : HMap :=
  match h with
  | .bits b => .bits (b.range msbi.norm lsbi.norm)
  | .arr vs => .arr (hrangeA lsbi.norm msbi.norm vs)
  | _ => .empty

def hrangeO (h : HMap) (msbi lsbi : SZ) : Option HMap :=
  match h with
  | .bits _ => some (hrange h msbi lsbi)
  | .arr _ => some (hrange h msbi lsbi)
  | _ => none

-- ## Arr / Str extraction

def harr : HMap → List (Int × HMap)
  | .arr vs => vs
  | _ => []

def hstr : HMap → List (String × HMap)
  | .str fs => fs
  | _ => []

-- ## String-keyed access

-- Lookup by String key in an association list.
def haccessV : VId → List (String × HMap) → HMap
  | _, [] => .empty
  | i, (k, v) :: rest => if i == k then v else haccessV i rest

-- Field access on an HMap.str.
def haccess (h : HMap) (i : VId) : HMap :=
  match h with
  | .str fs => haccessV i fs
  | _ => .empty

def haccessO (h : HMap) (i : VId) : Option HMap :=
  match h with
  | .str fs =>
    match fs.find? (fun p => p.1 == i) with
    | some (_, v) => some v
    | none => none
  | _ => none

-- ## Array construction

-- Build an array association list from a list of HMaps, indexing from `start`.
def hlistArr (start : Int) : List HMap → List (Int × HMap)
  | [] => []
  | h :: rest => (start, h) :: hlistArr (start + 1) rest

-- Build an HMap.arr from a list of HMaps (0-indexed).
def harray (hs : List HMap) : HMap :=
  .arr (hlistArr 0 hs)

-- ## Hierarchical paths

inductive HElt where
  | ind (i : SZ)
  | vid (i : String)
  deriving Inhabited, Repr, BEq

abbrev HPath := List HElt

def HElt.beq : HElt → HElt → Bool
  | .ind a, .ind b => a == b
  | .vid a, .vid b => a == b
  | _, _ => false

def HPath.beq : HPath → HPath → Bool
  | [], [] => true
  | a :: as_, b :: bs => HElt.beq a b && HPath.beq as_ bs
  | _, _ => false

-- The top-level element of a path (last element, since paths are root-first).
def HPath.top : HPath → Option HElt
  | [] => none
  | [e] => some e
  | _ :: rest => HPath.top rest

-- Navigate one step into an HMap.
def hmove (h : HMap) (e : HElt) : HMap :=
  match e with
  | .ind i => hselect h i
  | .vid i => haccess h i

-- Find the path to a variable name in an HMap.str (returns first match).
def hpos : VId → List (String × HMap) → Option HPath
  | _, [] => none
  | i, (k, _) :: rest =>
    if i == k then some [HElt.vid i]
    else hpos i rest

-- Lookup by hierarchical path.
def hfind : HMap → HPath → HMap
  | h, [] => h
  | h, e :: rest => hfind (hmove h e) rest

-- Create a singleton nested HMap from a root-first path (head is outermost).
def hsingle : HPath → HMap → HMap
  | [], v => v
  | HElt.ind i :: np, v => .arr [(i.norm, hsingle np v)]
  | HElt.vid i :: np, v => .str [(i, hsingle np v)]

-- ## Iteration helpers

/- Update-or-insert in array by Int key.
    If key found, apply `f` to existing value. Otherwise insert `(key, d)`. -/
def hiterArr (f : HMap → HMap) (d : HMap) (key : Int) : List (Int × HMap) → List (Int × HMap)
  | [] => [(key, d)]
  | (k, v) :: rest =>
    if key == k then (k, f v) :: rest
    else (k, v) :: hiterArr f d key rest

/- Update-or-insert in str by String key.
    If key found, apply `f` to existing value. Otherwise insert `(key, d)`. -/
def hiterStr (f : HMap → HMap) (d : HMap) (key : String) : List (String × HMap) → List (String × HMap)
  | [] => [(key, d)]
  | (k, v) :: rest =>
    if key == k then (k, f v) :: rest
    else (k, v) :: hiterStr f d key rest

-- ## Add / update

-- Add a value at a hierarchical path.
def hadd (h : HMap) (p : HPath) (v : HMap) : HMap :=
  match p, h with
  | [], _ => v
  | [HElt.ind i], .arr vs => .arr (hiterArr (fun _ => v) v i.norm vs)
  | [HElt.vid i], .str fs => .str (hiterStr (fun _ => v) v i fs)
  | HElt.ind i :: rest, .arr vs =>
    .arr (hiterArr (fun sub => hadd sub rest v) (hadd .empty rest v) i.norm vs)
  | HElt.vid i :: rest, .str fs =>
    .str (hiterStr (fun sub => hadd sub rest v) (hadd .empty rest v) i fs)
  | _, _ => h

-- Update a value at a hierarchical path with a function.
def hupdf (h : HMap) (p : HPath) (f : HMap → HMap) : HMap :=
  match p, h with
  | [], _ => f h
  | [HElt.ind i], .arr vs => .arr (hiterArr f (f .empty) i.norm vs)
  | [HElt.vid i], .str fs => .str (hiterStr f (f .empty) i fs)
  | HElt.ind i :: rest, .arr vs =>
    .arr (hiterArr (fun sub => hupdf sub rest f) (hupdf .empty rest f) i.norm vs)
  | HElt.vid i :: rest, .str fs =>
    .str (hiterStr (fun sub => hupdf sub rest f) (hupdf .empty rest f) i fs)
  | _, _ => h

-- ## Binary merge operations (parameterized by merge function f)

-- Update-oriented array merge

-- For each entry in arr1: if key exists in arr2, apply `f h1 h2`; else keep h1.
def hbinUArr1 (f : HMap → HMap → HMap) (arr1 arr2 : List (Int × HMap)) : List (Int × HMap) :=
  arr1.map fun (k, v1) =>
    match arr2.find? (fun p => p.1 == k) with
    | some (_, v2) => (k, f v1 v2)
    | none => (k, v1)

-- Entries in arr2 whose keys are NOT in arr1.
def hbinUArr2 (arr1 arr2 : List (Int × HMap)) : List (Int × HMap) :=
  arr2.filter fun (k, _) => !(arr1.any fun (k', _) => k' == k)

-- Update-oriented merge: hbinUArr1 ++ hbinUArr2.
def hbinUArr (f : HMap → HMap → HMap) (arr1 arr2 : List (Int × HMap)) : List (Int × HMap) :=
  hbinUArr1 f arr1 arr2 ++ hbinUArr2 arr1 arr2

-- Update-oriented string merge

-- For each entry in str1: if key exists in str2, apply `f h1 h2`; else keep h1.
def hbinUStr1 (f : HMap → HMap → HMap) (str1 str2 : List (String × HMap)) : List (String × HMap) :=
  str1.map fun (k, v1) =>
    match str2.find? (fun p => p.1 == k) with
    | some (_, v2) => (k, f v1 v2)
    | none => (k, v1)

-- Entries in str2 whose keys are NOT in str1.
def hbinUStr2 (str1 str2 : List (String × HMap)) : List (String × HMap) :=
  str2.filter fun (k, _) => !(str1.any fun (k', _) => k' == k)

-- Update-oriented merge: hbinUStr1 ++ hbinUStr2.
def hbinUStr (f : HMap → HMap → HMap) (str1 str2 : List (String × HMap)) : List (String × HMap) :=
  hbinUStr1 f str1 str2 ++ hbinUStr2 str1 str2

-- Function-application-oriented array merge

-- For each entry in arr1: apply `f h1 (lookup_or_empty arr2)`.
def hbinFArr1 (f : HMap → HMap → HMap) (arr1 arr2 : List (Int × HMap)) : List (Int × HMap) :=
  arr1.map fun (k, v1) =>
    match arr2.find? (fun p => p.1 == k) with
    | some (_, v2) => (k, f v1 v2)
    | none => (k, f v1 .empty)

-- For entries in arr2 not in arr1: apply `f empty h2`.
def hbinFArr2 (f : HMap → HMap → HMap) (arr1 arr2 : List (Int × HMap)) : List (Int × HMap) :=
  (arr2.filter fun (k, _) => !(arr1.any fun (k', _) => k' == k)).map fun (k, v2) =>
    (k, f .empty v2)

-- Function-application-oriented merge: hbinFArr1 ++ hbinFArr2.
def hbinFArr (f : HMap → HMap → HMap) (arr1 arr2 : List (Int × HMap)) : List (Int × HMap) :=
  hbinFArr1 f arr1 arr2 ++ hbinFArr2 f arr1 arr2

-- Function-application-oriented string merge

-- For each entry in str1: apply `f h1 (lookup_or_empty str2)`.
def hbinFStr1 (f : HMap → HMap → HMap) (str1 str2 : List (String × HMap)) : List (String × HMap) :=
  str1.map fun (k, v1) =>
    match str2.find? (fun p => p.1 == k) with
    | some (_, v2) => (k, f v1 v2)
    | none => (k, f v1 .empty)

-- For entries in str2 not in str1: apply `f empty h2`.
def hbinFStr2 (f : HMap → HMap → HMap) (str1 str2 : List (String × HMap)) : List (String × HMap) :=
  (str2.filter fun (k, _) => !(str1.any fun (k', _) => k' == k)).map fun (k, v2) =>
    (k, f .empty v2)

-- Function-application-oriented merge: hbinFStr1 ++ hbinFStr2.
def hbinFStr (f : HMap → HMap → HMap) (str1 str2 : List (String × HMap)) : List (String × HMap) :=
  hbinFStr1 f str1 str2 ++ hbinFStr2 f str1 str2

-- ## High-level merge operations

mutual
/- Recursive hierarchical update: h1 updated by h2.
    Defined via mutual recursion with helpers that inline the recursive merge
    calls from hbinUArr/hbinUStr, so that Lean can verify structural termination. -/
def hupds : HMap → HMap → HMap
  | .empty, h2 => h2
  | .bits b1, .empty => .bits b1
  | .bits _, .bits b2 => .bits b2
  | .bits b1, _ => .bits b1
  | .arr hs1, .empty => .arr hs1
  | .arr _, .bits b2 => .bits b2
  | .arr hs1, .arr hs2 => .arr (hupdsArr hs1 hs2 ++ hbinUArr2 hs1 hs2)
  | .arr hs1, .str _ => .arr hs1
  | .str s1, .empty => .str s1
  | .str _, .bits b2 => .bits b2
  | .str s1, .arr _ => .str s1
  | .str s1, .str s2 => .str (hupdsStr s1 s2 ++ hbinUStr2 s1 s2)

-- Helper: merge matching entries from arr1 against arr2, inlining hupds calls.
def hupdsArr : List (Int × HMap) → List (Int × HMap) → List (Int × HMap)
  | [], _ => []
  | (k, v1) :: rest, arr2 =>
    match arr2.find? (fun p => p.1 == k) with
    | some (_, v2) => (k, hupds v1 v2) :: hupdsArr rest arr2
    | none => (k, v1) :: hupdsArr rest arr2

-- Helper: merge matching entries from str1 against str2, inlining hupds calls.
def hupdsStr : List (String × HMap) → List (String × HMap) → List (String × HMap)
  | [], _ => []
  | (k, v1) :: rest, str2 =>
    match str2.find? (fun p => p.1 == k) with
    | some (_, v2) => (k, hupds v1 v2) :: hupdsStr rest str2
    | none => (k, v1) :: hupdsStr rest str2
end

-- Predicate-based flat update (single entry): if predicate holds on the key, update.
def phupdsI (p : VId → Bool) (h2 : HMap) : (String × HMap) → (String × HMap)
  | (k, v) => if p k then (k, hupds v h2) else (k, v)

-- Predicate-based flat updates: apply hupds to matching fields.
def phupds (p : VId → Bool) (h1 : HMap) (h2 : HMap) : HMap :=
  match h1 with
  | .str fs => .str (fs.map (phupdsI p h2))
  | _ => h1

-- Left-priority flat merge: for each field in h1, merge with h2's matching field.
def hmergeL (h1 h2 : HMap) : HMap :=
  match h1, h2 with
  | .str s1, .str s2 => .str (hbinUStr hupds s1 s2)
  | _, _ => h1

-- Right-priority flat merge: for each field in h2, merge with h1's matching field.
def hmergeR (h1 h2 : HMap) : HMap :=
  match h1, h2 with
  | .str s1, .str s2 => .str (hbinUStr hupds s2 s1)
  | _, _ => h2

-- ## Filtering

-- Filter string fields by a list of names to keep.
def hfilterStr (names : List VId) : List (String × HMap) → List (String × HMap)
  | [] => []
  | (k, v) :: rest =>
    if names.any (· == k) then (k, v) :: hfilterStr names rest
    else hfilterStr names rest

-- Filter an HMap.str by a list of field names.
def hfilter (names : List VId) (h : HMap) : HMap :=
  match h with
  | .str fs => .str (hfilterStr names fs)
  | _ => h

end VerilLean.Lib
