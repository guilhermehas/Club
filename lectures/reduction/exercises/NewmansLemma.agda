-- Newman's diamond lemma: Local confluence implies confluence for any
-- well-founded (or terminating) relation `R`.

module NewmansLemma {ℓ} {A : Set ℓ} (let Rel = A → A → Set ℓ) (R : Rel) where

private
  variable
    a a₁ a₂ a' : A
    b b₁ b₂ b' : A
    c c₁ c₂ c' : A
    r r₁ r₂ r' : R a b

-- Reflexive–transitive closure of `R`

data R* : Rel where
  nil  : R* a a
  cons : (r : R a b) → (rs : R* b c) → R* a c

refl : R* a a
refl = nil

singl : R a b → R* a b
singl r = cons r refl

trans : R* a b → R* b c → R* a c
trans nil         ss = ss
trans (cons r rs) ss = cons r (trans rs ss)

-- Joinable elements

data Joinable (a₁ a₂ : A) : Set ℓ where
  join : (rs₁ : R* a₁ b) → (rs₂ : R* a₂ b) → Joinable a₁ a₂

-- (Locally) confluent elements

LocConfl = λ a → ∀ {b₁ b₂} → (r₁  : R  a b₁) → (r₂  : R  a b₂) → Joinable b₁ b₂
Confl    = λ a → ∀ {b₁ b₂} → (rs₁ : R* a b₁) → (rs₂ : R* a b₂) → Joinable b₁ b₂

-- Accessible elements

data Acc (a : A) : Set ℓ where
  acc : (p : ∀ {b} → (r : R a b) → Acc b) → Acc a

-- Well-foundedness

WF = ∀ {a} → Acc a

-- Goal: prove Newman's diamond lemma:

module _ (wf : WF) (lc : ∀ {a} → LocConfl a) where
  newman's-lemma1 : {a a1 a2 : A} → Acc a → R a a1  → R* a a2 → Joinable a1 a2
  newman's-lemma2 : {a a1 a2 : A} → Acc a → R* a a1 → R* a a2 → Joinable a1 a2

  newman's-lemma1 _ Raa1 nil = join nil (cons Raa1 nil)
  newman's-lemma1 (acc f) Raa1 (cons Rab R*aa2) with lc Raa1 Rab
  ... | join {x} R*xa1 R*ba1 with newman's-lemma2 (f Rab) R*ba1 R*aa2
  ... | join Raa- Ra2a- = join (trans R*xa1 Raa-) Ra2a-

  newman's-lemma2 _ nil Raa2 = join Raa2 nil
  newman's-lemma2 (acc f) (cons Rab Raa1) Raa2 with newman's-lemma1 (acc f) Rab Raa2
  ... | join Rba1 Ra2a with newman's-lemma2 (f Rab) Rba1 Raa1
  ... | join Raa- Ra1a- = join Ra1a- (trans Ra2a Raa-)

  newman's-lemma : ∀ {a} → Confl a
  newman's-lemma = newman's-lemma2 wf

-- Hint: the lemma cannot be proven directly in this formulation, a
-- proof needs auxiliary lemmata and definitions.
