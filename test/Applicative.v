(* Verification of Applicative laws for (a generalization of) SqlDecoder  *)

Set Warnings "-notation-overridden,-parsing".

(* Model Monoid & Applicative in general *)

Inductive MonoidDict (M : Type) :=
| monoid_dict : M -> (M -> M -> M) -> MonoidDict M.
Arguments monoid_dict {M} _ _.

Definition MonoidLaws {M : Type} (dict : MonoidDict M) :=
  match dict with
  | monoid_dict mempty append =>
    (forall (m : M), append mempty m = m) (* left identity *)
    /\ (forall (m : M), append m mempty = m) (* right identity *)
    /\  (forall (a  b c : M), append a (append b c) = append (append a b) c) (* associativity *)
end.

Definition Pure F := forall A, A -> F A.
Definition App F := forall A B, F (A -> B) -> F A -> F B.
(* Definition ApplicativeDict F := (Pure F, App F). *)
Inductive ApplicativeDict F := applicative_dict : Pure F -> App F -> ApplicativeDict F.
Arguments applicative_dict {F} _ _.

(* The Monoidal type generalizes SqlDecoder, substituting any Monoid M
for the [Text], and any Applicative F for the SqlParser. *)

Inductive Monoidal (M : Type) (F : Type -> Type) (A : Type) : Type := 
| monoidal : M -> (F A) -> Monoidal M F A.

Arguments monoidal {M} {F} {A} _ _.

Definition pure_monoidal {M : Type} {F : Type -> Type}
(m_dict : MonoidDict M) (f_dict : ApplicativeDict F) : Pure (Monoidal M F) :=
  match m_dict, f_dict with
  | monoid_dict mempty _, applicative_dict pure_f _ => 
    fun {A : Type} (a : A) => monoidal mempty (pure_f A a)
  end.
 
Definition app_monoidal {M : Type} {F : Type -> Type}
(m_dict : MonoidDict M) (f_dict : ApplicativeDict F) : App (Monoidal M F) :=
  match m_dict, f_dict with
  | monoid_dict _ append, applicative_dict _ app =>
    fun {A : Type} {B : Type} (mo : Monoidal M F (A -> B)) (no : Monoidal M F A) =>
      match mo, no with
      | monoidal m1 fab, monoidal m2 fa => monoidal (append m1 m2) (app A B fab fa)
      end
  end.

Definition ApplicativeMonoidal {M : Type} {F : Type -> Type}
(m_dict : MonoidDict M) (f_dict : ApplicativeDict F) : ApplicativeDict (Monoidal M F) :=
  applicative_dict (pure_monoidal m_dict f_dict) (app_monoidal m_dict f_dict).

Definition identity (A : Type) (a : A) : A := a.

Definition ApplicativeIdentity {F : Type->Type} (dict : ApplicativeDict F) : Prop :=
  match dict with
    | applicative_dict pure app => 
      forall A fa, app A A (pure (A -> A) (fun x => x)) fa = fa
end.

Theorem app_id_monoidal : forall (M : Type) (F : Type -> Type) (A : Type)
    (m_dict : MonoidDict M) (m_laws : MonoidLaws m_dict)
    (f_dict : ApplicativeDict F) (f_id : ApplicativeIdentity f_dict)
    (v : Monoidal M F A),
    ApplicativeIdentity (ApplicativeMonoidal m_dict f_dict).
Proof.
  intros. destruct v.
  destruct m_dict as [mempty append].
  destruct m_laws as [left_id [right_id m_assoc]].
  destruct f_dict as [pure app].
  simpl. intros. destruct fa. rewrite left_id. rewrite f_id. reflexivity.
Qed.

Definition ApplicativeHomomorphism { F : Type -> Type} (dict : ApplicativeDict F) : Prop :=
  match dict with
    | applicative_dict pure app =>
      forall (A B : Type) (f : A -> B) (a : A),
        app A B (pure (A->B) f) (pure A a) = pure B (f a)
  end.

Theorem homomorphism_monoidal : forall (M : Type) (F : Type -> Type)
    (m_dict : MonoidDict M) (m_laws : MonoidLaws m_dict) (f_dict : ApplicativeDict F)
    (homomorphism_f : ApplicativeHomomorphism f_dict),
  ApplicativeHomomorphism (ApplicativeMonoidal m_dict f_dict).
Proof.
  intros.
  destruct m_dict as [mempty append].
  destruct m_laws as [left_id [right_id m_assoc]].
  destruct f_dict as [pure app].

  simpl.  intros. rewrite left_id.
  (* unfold ApplicativeHomomorphism in homomorphism_f. *)
  rewrite homomorphism_f. reflexivity.
Qed.

Definition andThen {A B : Type} (a : A) : (A -> B) -> B :=
  fun f => f a.

Definition ApplicativeInterchange { F : Type -> Type} (dict : ApplicativeDict F) : Prop :=
  match dict with
    | applicative_dict pure app =>
      forall (A B : Type) (u : F (A -> B)) (y : A),
        app A B u (pure A y) = app (A->B) B (pure ((A->B) -> B) (andThen y) ) u
end.

Theorem interchange_monoidal : forall (M : Type) (F : Type -> Type)
    (m_dict : MonoidDict M) (m_laws : MonoidLaws m_dict) (f_dict : ApplicativeDict F)
(interchange_f : ApplicativeInterchange f_dict),
    ApplicativeInterchange (ApplicativeMonoidal m_dict f_dict).
Proof.
  intros. 
  destruct m_dict as [mempty append].
  destruct m_laws as [left_id [right_id m_assoc]].
  destruct f_dict as [pure app].
  simpl. intros. destruct u.
  rewrite interchange_f. rewrite left_id, right_id. reflexivity.
Qed.

Definition ApplicativeComposition { F : Type -> Type} (dict : ApplicativeDict F) : Prop :=
  match dict with
  | applicative_dict pure app =>
    forall (A B : Type) (u : F (A -> B)) (y : A),
      app A B u (pure A y) = app (A->B) B (pure ((A->B) -> B) (andThen y) ) u
  end.

Theorem composition_monoidal : forall (M : Type) (F : Type -> Type)
    (m_dict : MonoidDict M) (m_laws : MonoidLaws m_dict) (f_dict : ApplicativeDict F)
    (composition_f : ApplicativeComposition f_dict),
    ApplicativeComposition (ApplicativeMonoidal m_dict f_dict).
Proof.
  intros.
  destruct m_dict as [mempty append].
  destruct m_laws as [left_id [right_id m_assoc]].
  destruct f_dict as [pure app].
  simpl. intros. destruct u.
  rewrite composition_f. rewrite left_id, right_id. reflexivity.
Qed.
                                                 
