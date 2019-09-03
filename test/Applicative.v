(* Verification of Applicative laws for (a generalization of) SqlDecoder  *)

Set Warnings "-notation-overridden,-parsing".

(* Model Monoid & Applicative in general *)

Inductive MonoidDict (M : Type) :=
| monoid_dict : M -> (M -> M -> M) -> MonoidDict M.
Arguments monoid_dict {M} _ _.

Definition MonoidLaws {M : Type} (dict : MonoidDict M) :=
  match dict with
  | monoid_dict mempty append =>
    forall (m : M), append mempty m = m (* left identity *)
    /\ forall (m : M), append m mempty = m (* right identity *)
    /\  forall (a  b c : M), append a (append b c) = append (append a b) c (* associativity *)
end.

Definition Pure F := forall A, A -> F A.
Definition App F := forall A B, F (A -> B) -> F A -> F B.
Definition ApplicativeDict F := (Pure F, App F).

Definition ApplicativeIdentity {F : Type->Type} (dict : ApplicativeDict F) :=
  match dict with
  | (pure, app) => forall A, app (pure identity) a = a
end.



(* The Monoidal type generalizes SqlDecoder, substituting any Monoid M
for the [Text], and any Applicative F for the SqlParser. *)

Inductive Monoidal (M : Type) (F : Type -> Type) (A : Type) : Type := 
| monoidal : M -> (F A) -> Monoidal M F A.

Arguments monoidal {M} {F} {A} _ _.


Definition pure_monoidal {M : Type} {F : Type -> Type} { A : Type }
    (mempty : M) (pure_f : forall A, A -> F A) (a : A) : Monoidal M F A :=
        monoidal mempty (pure_f A a).

Definition app_monoidal {M : Type} {F : Type -> Type} { A B : Type}
(* type class methods *) (append : M -> M -> M) (app_f : forall A B, F (A -> B) -> F A -> F B)
    (mo : Monoidal M F (A -> B)) ( no : Monoidal M F A) : Monoidal M F B :=
  match mo, no with
  | monoidal m1 fab, monoidal m2 fa => monoidal (append m1 m2) (app_f A B fab fa)
  end.

Definition identity (A : Type) (a : A) : A := a.
  
Theorem app_id_monoidal : forall (M : Type) (F : Type -> Type) (A : Type)
    (mempty : M) (append : M -> M -> M) (m_id : forall (m : M), append mempty m = m) (* monoid *)
    (pure_f : forall A, A -> F A) (app_f : forall A B, F (A -> B) -> F A -> F B)  (* applicative F *)
    (app_id_f : forall (a : F A), app_f A A (pure_f (A -> A) (identity A)) a = a)
    (v : Monoidal M F A),
    @app_monoidal M F A A append app_f (pure_monoidal mempty pure_f (identity A)) v = v.
Proof.
  intros. destruct v. simpl. rewrite m_id. rewrite app_id_f. reflexivity.
Qed.

Theorem homomorphism_monoidal : forall (M : Type) (F : Type -> Type) (A B : Type)
    (mempty : M) (append : M -> M -> M) (m_id : forall (m : M), append mempty m = m) (* monoid *)
    (pure_f : forall A, A -> F A) (app_f : forall A B, F (A -> B) -> F A -> F B)  (* applicative F *)
(app_id_f : forall (a : F A), app_f A A (pure_f (A -> A) (identity A)) a = a)
(homomorphism_f : forall (f : A -> B) (a : A), app_f A B (pure_f (A->B) f) (pure_f A a) = pure_f B (f a))
    (f : A -> B) (a : A),
    @app_monoidal M F A B append app_f
        (pure_monoidal mempty pure_f f)
        (pure_monoidal mempty pure_f a)
    = (pure_monoidal mempty pure_f (f a)).
Proof.
  intros. simpl. rewrite m_id. rewrite homomorphism_f. reflexivity.
Qed.

Definition andThen {A B : Type} (a : A) : (A -> B) -> B :=
  fun f => f a.

Theorem interchange_monoidal : forall (M : Type) (F : Type -> Type) (A B : Type)
(mempty : M) (append : M -> M -> M)  (* monoid *)
(m_id_l : forall (m : M), append mempty m = m) (m_id_r : forall (m : M), append m mempty = m)
    (pure_f : forall A, A -> F A) (app_f : forall A B, F (A -> B) -> F A -> F B)  (* applicative F *)
    (app_id_f : forall (a : F A), app_f A A (pure_f (A -> A) (identity A)) a = a)
(homomorphism_f : forall (f : A -> B) (a : A), app_f A B (pure_f (A->B) f) (pure_f A a) = pure_f B (f a))
(interchange_f : forall (u : F (A -> B)) (y : A), app_f A B u (pure_f A y) =
                                                  app_f (A->B) B (pure_f ((A->B) -> B) (andThen y) ) u)
(u : Monoidal M F (A -> B)) (y : A),
    @app_monoidal M F A B append app_f u (pure_monoidal mempty pure_f y) =
    @app_monoidal M F (A->B) B append app_f  (pure_monoidal mempty pure_f (andThen y)) u.
Proof.
  intros. destruct u. simpl. rewrite interchange_f. rewrite m_id_l, m_id_r. reflexivity.
Qed.

Theorem composition_monoidal : forall (M : Type) (F : Type -> Type) (A B : Type)
    (mempty : M) (append : M -> M -> M)  (* monoid *)
(m_id_l : forall (m : M), append mempty m = m) (m_id_r : forall (m : M), append m mempty = m)
(pure_f : forall A, A -> F A) (app_f : forall A B, F (A -> B) -> F A -> F B)  (* applicative F *)
    (app_id_f : forall (a : F A), app_f A A (pure_f (A -> A) (identity A)) a = a)
    (homomorphism_f : forall (f : A -> B) (a : A), app_f A B (pure_f (A->B) f) (pure_f A a) = pure_f B (f a))
    (interchange_f : forall (u : F (A -> B)) (y : A), app_f A B u (pure_f A y) =
                                                  app_f (A->B) B (pure_f ((A->B) -> B) (andThen y) ) u)
