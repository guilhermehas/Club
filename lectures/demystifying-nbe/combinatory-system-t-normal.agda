----------------------
-- Demystifying NbE --
----------------------

-- This is the short version of the code used during the ITC talk.

-- For full version see:
--   https://github.com/nachivpn/gluetn

-- For abstract of the talk see:
--   https://github.com/InitialTypes/Club/wiki/Abstracts.2019.DemystifyingNbE

open import Data.Unit using (⊤; tt)
open import Data.Empty using (⊥)
open import Data.Bool using (Bool; true; false; _∧_; T)
open import Data.Nat
  using (ℕ ; zero ; suc)
open import Data.Product
  -- using (∃ ; _×_ ; _,_)
  renaming (proj₁ to π₁ ; proj₂ to π₂)
open import Relation.Nullary
  using (¬_)
open import Relation.Binary.PropositionalEquality
  using (_≡_; cong₂; sym; subst; cong)
  renaming (trans to ≡-trans)
open import Relation.Binary.Construct.Closure.ReflexiveTransitive
  using (Star)
  renaming (_◅◅_ to trans)

open Star renaming (ε to refl)
open _≡_ renaming (refl to ≡-refl)

infixr 5 _⇒_
infixl 5 _∙_
infix  3 _⟶*_

-- Types
data Ty : Set where
  Nat : Ty
  _⇒_ : (a : Ty) → (b : Ty) → Ty

variable
  a b c : Ty

-- Terms
data Tm : Ty → Set where
  K    : Tm (a ⇒ b ⇒ a)
  S    : Tm ((a ⇒ b ⇒ c) ⇒ (a ⇒ b) ⇒ a ⇒ c)
  Zero : Tm Nat
  Succ : Tm (Nat ⇒ Nat)
  Rec  : Tm (a ⇒ (Nat ⇒ a ⇒ a) ⇒ Nat ⇒ a)
  _∙_  : Tm (a ⇒ b) → Tm a → Tm b

-- Examples of terms
module _ where

  id : Tm (a ⇒ a)
  id {a} = S ∙ K ∙ K {b = a}

  plus : Tm Nat → Tm Nat → Tm Nat
  plus m n = Rec ∙ n ∙ (K ∙ Succ) ∙ m

  one = Succ ∙ Zero
  two = Succ ∙ one

  three : Tm Nat
  three = plus one two

  three' : Tm Nat
  three' = plus two one

----------------------
-- Redution relations
----------------------

-- Single-step reduction relation
data _⟶_ : Tm a → Tm a → Set where

  redk : {x : Tm a} {y : Tm b}
    → (K ∙ x ∙ y) ⟶ x

  reds : {g : Tm (a ⇒ b ⇒ c)} {f : Tm (a ⇒ b)} {x : Tm a}
    → (S ∙ g ∙ f ∙ x) ⟶ (g ∙ x ∙ (f ∙ x))

  rec0 : {b : Tm a} {f : Tm (Nat ⇒ a ⇒ a)}
    → (Rec ∙ b ∙ f ∙ Zero) ⟶ b

  recs : {b : Tm a} {f : Tm (Nat ⇒ a ⇒ a)} {n : Tm Nat}
    → (Rec ∙ b ∙ f ∙ (Succ ∙ n)) ⟶ (f ∙ n ∙ (Rec ∙ b ∙ f ∙ n))

  fun  : {t t' : Tm (a ⇒ b)} {u : Tm a}
    → t ⟶ t'
    → (t ∙ u) ⟶ (t' ∙ u)

  arg  : {t : Tm (a ⇒ b)} {u u' : Tm a}
    → u ⟶ u'
    → (t ∙ u) ⟶ (t ∙ u')

-- Multi-step (0 or more single-steps) reduction relation
_⟶*_ : Tm a → Tm a → Set
_⟶*_ = Star (_⟶_)

-- embed ⟶ to ⟶*
lift : {e e' : Tm a}
  → e ⟶ e'
  → e ⟶* e'
lift p = p ◅ refl

-- congruence rule for ∙ (in ⟶*)
cong-∙  : {t t' : Tm (a ⇒ b)} {u u' : Tm a}
    → t ⟶* t'
    → u ⟶* u'
    → t ∙ u ⟶* t' ∙ u'
cong-∙ refl    refl    = refl
cong-∙ refl    (x ◅ q) = (arg x) ◅ (cong-∙ refl q)
cong-∙ (x ◅ p) q       = (fun x) ◅ (cong-∙ p q)

-------------------------------
-- Normalization by Evaluation
-------------------------------

isSucc : Tm a → Set
isSucc K = ⊥
isSucc S = ⊥
isSucc Zero = ⊥
isSucc Succ = ⊤
isSucc Rec = ⊥
isSucc (_ ∙ _) = ⊥

isNormal : Tm a → Set
isNormal K = ⊤
isNormal S = ⊤
isNormal Zero = ⊤
isNormal Succ = ⊤
isNormal Rec = ⊤
isNormal (K ∙ term) = isNormal term
isNormal (S ∙ term) = isNormal term
isNormal (Succ ∙ term) = isNormal term
isNormal (Rec ∙ term) = isNormal term
isNormal (K ∙ _ ∙ _) = ⊥
isNormal (S ∙ term ∙ term') = isNormal term × isNormal term'
isNormal (Rec ∙ term ∙ term') = isNormal term × isNormal term'
isNormal (term ∙ term' ∙ term'' ∙ term''') = isSucc term × isSucc term' × isSucc term'' × isNormal term'''

DoesntReduce : Tm a → Set
DoesntReduce {a} t = {t' : Tm a} → ¬ (t ⟶ t')

normalDoesntReduce : (t : Tm a) (normal : isNormal t) → DoesntReduce t
normalDoesntReduce (K ∙ t) normal (arg step) = normalDoesntReduce t normal step
normalDoesntReduce (S ∙ t') normal (arg step) = normalDoesntReduce t' normal step
normalDoesntReduce (Succ ∙ t') normal (arg step) = normalDoesntReduce t' normal step
normalDoesntReduce (Rec ∙ t') normal (arg step) = normalDoesntReduce t' normal step
normalDoesntReduce (S ∙ t' ∙ t'') normal (fun (arg step)) = normalDoesntReduce t' (π₁ normal) step
normalDoesntReduce (S ∙ t' ∙ t'') normal (arg step) = normalDoesntReduce t'' (π₂ normal) step
normalDoesntReduce (Rec ∙ t' ∙ t'') (u , u') (fun (arg step)) = normalDoesntReduce t' u step
normalDoesntReduce (Rec ∙ t' ∙ t'') (u , u') (arg step) = normalDoesntReduce t'' u' step
normalDoesntReduce (.(K ∙ _ ∙ _) ∙ t' ∙ t'' ∙ t''') normal (fun (fun (fun redk))) = π₁ normal
normalDoesntReduce (.(S ∙ _ ∙ _ ∙ _) ∙ t' ∙ t'' ∙ t''') normal (fun (fun (fun reds))) = π₁ normal
normalDoesntReduce (.(Rec ∙ _ ∙ _ ∙ Zero) ∙ t' ∙ t'' ∙ t''') normal (fun (fun (fun rec0))) = π₁ normal
normalDoesntReduce (.(Rec ∙ _ ∙ _ ∙ (Succ ∙ _)) ∙ t' ∙ t'' ∙ t''') normal (fun (fun (fun recs))) = π₁ normal
normalDoesntReduce (.(_ ∙ _) ∙ t' ∙ t'' ∙ t''') normal (fun (fun (fun (fun step)))) = π₁ normal
normalDoesntReduce (.(_ ∙ _) ∙ t' ∙ t'' ∙ t''') normal (fun (fun (fun (arg step)))) = π₁ normal
normalDoesntReduce (t ∙ .(K ∙ _ ∙ _) ∙ t'' ∙ t''') normal (fun (fun (arg redk))) = π₁ (π₂ normal)
normalDoesntReduce (t ∙ .(S ∙ _ ∙ _ ∙ _) ∙ t'' ∙ t''') normal (fun (fun (arg reds))) = π₁ (π₂ normal)
normalDoesntReduce (t ∙ .(Rec ∙ _ ∙ _ ∙ Zero) ∙ t'' ∙ t''') normal (fun (fun (arg rec0))) = π₁ (π₂ normal)
normalDoesntReduce (t ∙ .(Rec ∙ _ ∙ _ ∙ (Succ ∙ _)) ∙ t'' ∙ t''') normal (fun (fun (arg recs))) = π₁ (π₂ normal)
normalDoesntReduce (t ∙ .(_ ∙ _) ∙ t'' ∙ t''') normal (fun (fun (arg (fun step)))) = π₁ (π₂ normal)
normalDoesntReduce (t ∙ .(_ ∙ _) ∙ t'' ∙ t''') normal (fun (fun (arg (arg step)))) = π₁ (π₂ normal)
normalDoesntReduce (t ∙ t' ∙ .(K ∙ _ ∙ _) ∙ t''') normal (fun (arg redk)) = π₁ (π₂ (π₂ normal))
normalDoesntReduce (t ∙ t' ∙ .(S ∙ _ ∙ _ ∙ _) ∙ t''') normal (fun (arg reds)) = π₁ (π₂ (π₂ normal))
normalDoesntReduce (t ∙ t' ∙ .(Rec ∙ _ ∙ _ ∙ Zero) ∙ t''') normal (fun (arg rec0)) = π₁ (π₂ (π₂ normal))
normalDoesntReduce (t ∙ t' ∙ .(Rec ∙ _ ∙ _ ∙ (Succ ∙ _)) ∙ t''') normal (fun (arg recs)) = π₁ (π₂ (π₂ normal))
normalDoesntReduce (t ∙ t' ∙ .(_ ∙ _) ∙ t''') normal (fun (arg (fun step))) = π₁ (π₂ (π₂ normal))
normalDoesntReduce (t ∙ t' ∙ .(_ ∙ _) ∙ t''') normal (fun (arg (arg step))) = π₁ (π₂ (π₂ normal))
normalDoesntReduce (t ∙ t' ∙ t'' ∙ t''') normal (arg step) = normalDoesntReduce t''' (π₂ (π₂ (π₂ normal))) step

Normal : (a : Ty) → Set
Normal a = Σ[ t ∈ Tm a ] isNormal t

⟦_⟧ : (a : Ty) → Set
⟦ Nat ⟧ = ℕ
⟦ x ⇒ y ⟧ = Normal (x ⇒ y) × (⟦ x ⟧ → ⟦ y ⟧)

quot' : ⟦ a ⟧ → Normal a
quot' {Nat} zero = Zero , tt
quot' {Nat} (suc x) = Succ ∙ π₁ (quot' x) , π₂ (quot' x)
quot' {a ⇒ b} (t , _) = t

quot : ⟦ a ⟧ → Tm a
quot x = π₁ (quot' x)

quot'n : (t : ⟦ a ⟧) → isNormal (π₁ (quot' t))
quot'n x = π₂ (quot' x)

infixl 5 _∘_

_∘_ : ⟦ a ⇒ b ⟧ → ⟦ a ⟧ → ⟦ b ⟧
_∘_ (_ , f) x = f x

rec' : ⟦ a ⟧ → ⟦ Nat ⇒ a ⇒ a ⟧ → ⟦ Nat ⟧ → ⟦ a ⟧
rec' b f zero = b
rec' b f (suc n) = f ∘ n ∘ (rec' b f n)

eval : Tm a → ⟦ a ⟧
eval K = (K , tt) , (λ x → ((K ∙ quot x) , quot'n x) , (λ _ → x))
eval S = (S , tt) , (λ g
  → (S ∙ quot g , π₂ (π₁ g)) , λ f
    → (S ∙ quot g ∙ quot f , (π₂ (π₁ g)) , (π₂ (π₁ f))) , λ x
      → (g ∘ x) ∘ (f ∘ x))
eval Zero = zero
eval Succ = (Succ , tt) , suc
eval (Rec {a}) = (Rec , tt) , (λ b
  → (Rec ∙ quot b , quot'n  b) , λ f
    → (Rec ∙ quot b ∙ quot f , quot'n b , quot'n f)
      , rec' b f)
eval (t ∙ u) = eval t ∘ eval u

normNormal : Tm a → Normal a
normNormal t = quot' (eval t)

norm : Tm a → Tm a
norm t = quot (eval t)

-- Examples of normalization
module _ where

  three≈three' : norm three ≡ norm three'
  three≈three' = ≡-refl

  sillyId : Tm (a ⇒ a)
  sillyId {a} = S ∙ (K ∙ K ∙ K {a} {a}) ∙ K {_} {a}

  sillyId≈Id : norm id ≡ norm (sillyId {a})
  sillyId≈Id = ≡-refl

-------------------------------------------------------------
-- Logging the reduction trace of the normalization function
-------------------------------------------------------------

-- "Trace builder"
-- (a logical relation between terms and semantic values)
R : Tm a → ⟦ a ⟧ → Set

-- A trace builder of type `R t x` contains:
-- 1. a reduction trace t ⟶* (quote x)
-- 2. (if t has a function type)
--    a function which, given the trace builder for an argument u,
--    returns a new trace builder for its application with t

R {Nat}   t n = t ⟶* quot n
R {a ⇒ b} t f = t ⟶* quot f
  -- secret sauce (ss)
  × ({u : Tm a}{x : ⟦ a ⟧} → R u x → R (t ∙ u) (f ∘ x))

R-imp : {t : Tm a} {u : ⟦ a ⟧} → R t u → ⟦ a ⟧
R-imp {u = u} _ = u

-- build/extract the trace
R-reduces : {t : Tm a} {x : ⟦ a ⟧}
  → R t x
  → t ⟶* quot x
R-reduces {Nat}   t⟶*n       = t⟶*n
R-reduces {a ⇒ b} (t⟶*f , _) = t⟶*f

-- given trace builder of function and an argument,
-- we get a trace builder for their application
R-app : {t : Tm (a ⇒ b)} {f : ⟦ a ⇒ b ⟧} {u : Tm a} {x : ⟦ a ⟧}
  → R t f
  → R u x
  → R (t ∙ u) (f ∘ x)
R-app (_ , ss) uRx = ss uRx

-- given a reduction (g ⟶* f)
-- and a trace builder `R f x`,
-- then we get trace builder `R g x`
R-chain : {f g : Tm a} {x : ⟦ a ⟧}
  → g ⟶* f
  → R f x
  → R g x
R-chain {Nat} g⟶*f f⟶*n
  = trans g⟶*f f⟶*n
R-chain {a ⇒ b} g⟶*f (f⟶*h , ss)
  = trans g⟶*f f⟶*h
  , λ {u} {y} uRy → R-chain (cong-∙ g⟶*f refl) (ss uRy)

-- trace builder for recursion
R-rec : {e : Tm a} {v : ⟦ a ⟧}
  {t : Tm (Nat ⇒ a ⇒ a)} {f : ⟦ Nat ⇒ a ⇒ a ⟧}
  {n : Tm Nat} {m : ⟦ Nat ⟧}
  → R e v
  → R t f
  → R n m
  → R (Rec ∙ e ∙ t  ∙ n) (rec' v f m)
R-rec {m = zero} p q r
  = R-chain (trans (cong-∙ refl r) (lift rec0)) p
R-rec {_} {e} {v} {t} {f} {m = suc m} p q r
  = R-chain
      (trans (cong-∙ refl r) (lift recs))
      (R-app {f = π₂ f m} (R-app {f = f} q refl) (R-rec {f = f} {m = m} p q refl))

-- implement a trace builder for the entirety of `eval`
fund : (t : Tm a) → R t (eval t)
fund Zero = refl
fund Succ = refl , (λ x → cong-∙ refl x)
fund K = refl , (λ x
  → (cong-∙ refl (R-reduces x))
    , λ x₁ → R-chain (lift redk) x)
fund (t ∙ u) = R-app {f = eval t} (fund t) (fund u)
fund S = refl , λ {_} {x} p →
  cong-∙ refl (R-reduces {x = x} p) , λ {_} {y} q →
    (cong-∙ (cong-∙ refl (R-reduces {x = x} p)) (R-reduces {x = y} q)) , λ {_} {z} r →
      R-chain (lift reds) (R-app {f = π₂ x z} (R-app {f = x} p r) (R-app {f = y} q r))
fund Rec
  = refl , λ {_} {x} p →
    (cong-∙ refl (R-reduces p)) , λ {_} {y} q →
      (cong-∙ (cong-∙ refl (R-reduces p)) (R-reduces {x = y} q)) , λ {_} {n} r →
        R-rec {m = n} p q r

-- build a trace using the trace builder from `fund`
trace : (t : Tm a) → t ⟶* norm t
trace t = R-reduces (fund t)

------------
-- Exercises
------------

-----
-- 1. Prove soundness of (multi-step) reduction
-----

sound-red : {t t' : Tm a} → t ⟶ t' → eval t ≡ eval t'
sound-red redk = ≡-refl
sound-red reds = ≡-refl
sound-red rec0 = ≡-refl
sound-red recs = ≡-refl
sound-red (fun st) rewrite sound-red st = ≡-refl
sound-red (arg st) rewrite sound-red st = ≡-refl

sound-red* : {t t' : Tm a} → t ⟶* t' → eval t ≡ eval t'
sound-red* refl = ≡-refl
sound-red* (x ◅ st) rewrite sound-red x | sound-red* st = ≡-refl

-----
-- 2. Show that `norm t` doesn't reduce further
-----

nfDoesntReduce : (t : Tm a) → DoesntReduce (norm t)
nfDoesntReduce t with (t' , isNormal) ← normNormal t = normalDoesntReduce t' isNormal

-----
-- 3. Prove weak normalization
-----

WeakNorm : Tm a → Set
WeakNorm t = ∃ λ t' → (t ⟶* t') × DoesntReduce t'

weakNorm : ∀ (t : Tm a) → WeakNorm t
weakNorm t = norm t , trace _ , nfDoesntReduce t

-----
-- 4. Prove church-rosser property
-----

Converge : (t t' : Tm a) → Set
Converge t t' =  ∃ λ v → (t ⟶* v) × (t' ⟶* v)

church-rosser : {t u u' : Tm a}
 → t ⟶* u
 → t ⟶* u'
 → Converge u u'
π₁ (church-rosser {t = t} p q) = norm t
π₁ (π₂ (church-rosser {t = t} p q)) rewrite sound-red* p = trace _
π₂ (π₂ (church-rosser {t = t} p q)) rewrite sound-red* q = trace _

-----
-- 5. Prove decidability of convergence
-----

open import Relation.Nullary using (Dec ; yes ; no; _because_; ofʸ; ofⁿ)

_≟t_ : (a b : Ty) → Dec (a ≡ b)
Nat ≟t Nat = yes ≡-refl
Nat ≟t (b ⇒ b₁) = no (λ ())
(a ⇒ a₁) ≟t Nat = no (λ ())
(a ⇒ a₁) ≟t (b ⇒ b₁) with a ≟t b | a₁ ≟t b₁
... | .true because ofʸ p | .true because ofʸ p₁ = yes (cong₂ _⇒_ p p₁)
... | .true because ofʸ p | .false because ofⁿ ¬p = no λ { ≡-refl → ¬p ≡-refl}
... | .false because ofⁿ ¬p | .true because ofʸ p = no λ{ ≡-refl → ¬p ≡-refl}
... | .false because ofⁿ ¬p | .false because ofⁿ ¬p₁ = no λ{ ≡-refl → ¬p ≡-refl}

_≟_ : (t t' : Tm a) → Dec (t ≡ t')
K ≟ K = yes ≡-refl
K ≟ (t' ∙ t'') = no (λ ())
S ≟ S = yes ≡-refl
S ≟ (t' ∙ t'') = no (λ ())
Zero ≟ Zero = yes ≡-refl
Zero ≟ (t' ∙ t'') = no (λ ())
Succ ≟ Succ = yes ≡-refl
Succ ≟ (t' ∙ t'') = no (λ ())
Rec ≟ Rec = yes ≡-refl
Rec ≟ (t' ∙ t'') = no (λ ())
(t ∙ t₁) ≟ K = no (λ ())
(t ∙ t₁) ≟ S = no (λ ())
(t ∙ t₁) ≟ Zero = no (λ ())
(t ∙ t₁) ≟ Succ = no (λ ())
(t ∙ t₁) ≟ Rec = no (λ ())
(_∙_ {a} t t') ≟ (_∙_ {a'} u u') with a ≟t a'
... | _ because ofⁿ ¬p = no λ{ ≡-refl → ¬p ≡-refl}
... | _ because ofʸ ≡-refl with t ≟ u | t' ≟ u'
... | _ because ofʸ ≡-refl | _ because ofʸ ≡-refl = yes ≡-refl
... | _ because ofʸ _  | _ because ofⁿ ¬p = no λ{ ≡-refl → ¬p ≡-refl}
... | _ because ofⁿ ¬p | _ because ofʸ _  = no λ{ ≡-refl → ¬p ≡-refl}
... | _ because ofⁿ ¬p | _ because ofⁿ _  = no λ{ ≡-refl → ¬p ≡-refl}

converge?t : (t t' : Tm a) → norm t ≡ norm t' → Converge t t'
converge?t t t' eq = (norm t) , trace t , subst (λ x → t' ⟶* x) (sym eq) (trace t')

converge?f : (t t' : Tm a) → ¬ (norm t ≡ norm t') → ¬ (Converge t t')
converge?f t t' ¬eq (u , st , st') = ¬eq (cong quot (≡-trans (sound-red* st) (sym (sound-red* st'))))

converge? : (t t' : Tm a) → Dec (Converge t t')
converge? t t' with norm t ≟ norm t'
... | _ because ofʸ p = yes (converge?t _ _ p)
... | _ because ofⁿ ¬p = no (converge?f _ _ ¬p)
