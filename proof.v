(* stateWriter laws *)

Require Import Notations.
Require Import FunctionalExtensionality.

Set Implicit Arguments.

(** * The Monad Type Class *)

Definition idf {A : Type} (x : A) : A := x.

Definition composition {A} {B} {C} (f : B -> C) (g : A -> B) (a : A) := f (g a).

Class Functor f := {
  fmap: forall A B, (A -> B) -> f A -> f B;
  fmap_id: (forall A (a: f A), fmap idf a = a);
  fmap_dist: (forall A B C (fn : A -> B) (g : B -> C) (a : f A),
           fmap g (fmap fn a) = fmap (fun x => g (fn x)) a)
}.

Class Applicative (m : Type -> Type) (F: Functor m) := {
  pure: forall A, A -> m A;
  app: forall A B, m (A -> B) -> m A -> m B;
  app_identity: forall A (a : m A), app (pure idf) a = a;
  app_homo: forall A B (f : A -> B) (a : A), app (pure f) (pure a) = pure (f a);
  app_interchange: forall A B (u : m (A -> B)) (y : A), app u (pure y) = app (pure (fun f => f y)) u;
  app_composition: forall A B C (u : m (B -> C)) (v : m (A -> B)) (w : m A),
      app u (app v w) =
         app (app (app (pure composition) u) v) w;
  fmap_pure: forall A B (a : A) (f : A -> B),
      fmap f (pure a) = pure (f a)
}.

Class Monad (m : Type -> Type) (AF: Functor m) (AP: Applicative AF) := {
  bind : forall A B, m A -> (A -> m B) -> m B;
  right_unit: forall A (a: m A), a = bind a (fun x => pure x);
  left_unit: forall A (a: A) B (f: A -> m B),
       f a = bind (pure a) f;
  associativity: forall A (ma : m A) B f C (g: B -> m C),
       bind ma (fun x => bind (f x) g) = bind (bind ma f) g;
  fmap_return: forall A B (ma : m A) (f : A -> B),
       bind ma (fun a => pure (f a)) = fmap f ma;
  bind_fmap: forall A B C (ma : m A) (f : A -> B) (mf : B -> m C), 
       bind (fmap f ma) mf = bind ma (composition mf f)
}.

Definition RSST (R : Type) (W : Type) (S : Type) (M : Type -> Type) (A : Type) :=
 R -> (S * W) -> M (A * (S * W)).

Definition first {A : Type} {B : Type} {C : Type} (f : A -> B) (x : (A*C)) :=
  (f (fst x), snd x).

Theorem pair_idf : forall A B, (fun (x : A * B) => (fst x, snd x)) = idf.
Proof.
  intros. apply functional_extensionality.
  destruct x. simpl. unfold idf. reflexivity.
Qed.

Theorem funProductBreak: forall (A B C: Type) (f : (A*B) -> C), (fun x : A * B => let (a,b) := x in f (a,b)) = f.
Proof.
intros. apply functional_extensionality. intros. destruct x. reflexivity.
Qed.

Theorem fstSnd: forall (A B C: Type) (f : A -> B -> C),
  (fun x : A * B => let (a,b) := x in f a b) = (fun x => f (fst x) (snd x)).
Proof.
  intros.
  extensionality x. destruct x. simpl. reflexivity.
Qed.

Instance RSSTFunctor R W S (M : Type -> Type) (F: Functor M) : Functor (RSST R W S M) :=
{ fmap M A f FA := fun r sw => fmap (first f) (FA r sw)
}.
Proof.
- (* fmap_identity *)
  intros T m.
  extensionality r.
  extensionality sw.
  destruct sw.
  unfold first. unfold idf.
  rewrite pair_idf.
  rewrite fmap_id.
  reflexivity.
- (* fmap dist *)
  intros.
  extensionality r.
  extensionality sw.
  apply fmap_dist.
Defined.

Instance RSSTApplicative : forall R W S (M : Type -> Type) (FM : Functor M) (AM : Applicative FM) (MM : Monad AM)
  , Applicative (RSSTFunctor R W S FM) :=
{
  pure := fun A x r sw => @pure M FM AM (A*(S*W)) (x, sw);
  app A B mf mb := fun r sw =>
     @bind M FM AM MM ((A -> B)*(S*W)) (B*(S*W)) (mf r sw) (fun blob1 =>
        let (f,sw') := blob1
        in @bind M FM AM MM (A*(S*W)) (B * (S*W)) (mb r sw') (fun blob2 =>
            let (x,sw'') := blob2 in pure (f x, sw'')))
}.
Proof.
- (* app identity *)  
  intros.
  extensionality r. extensionality sw.
  rewrite <- left_unit.
  unfold idf.
  rewrite funProductBreak.
  rewrite <- right_unit.
  reflexivity.
- (* app_homo *)
  intros.
  extensionality r. extensionality sw.
  rewrite <- left_unit.   rewrite <- left_unit. reflexivity.
- (* app_interchange *)
  intros.
  extensionality r. extensionality sw.
  rewrite <- left_unit. apply f_equal.
  extensionality sw'.
  destruct sw.
  destruct sw'.
  rewrite <- left_unit. reflexivity.
- (* app_composition *)
  intros.
  extensionality r. extensionality sw.
  rewrite <- left_unit. rewrite <- associativity. rewrite <- associativity.
  apply f_equal.
  extensionality sw'.
  destruct sw. destruct sw'. rewrite <- left_unit.
  rewrite <- associativity. rewrite <- associativity. apply f_equal.
  extensionality sw''.
  destruct sw''.
  rewrite <- left_unit.
  rewrite <- associativity. apply f_equal.
  apply functional_extensionality. intros.
  destruct p0. destruct x. rewrite <- left_unit. unfold composition. reflexivity.
- (* fmap pure *)
  intros. unfold fmap. simpl.
  extensionality r. extensionality sw.
  rewrite fmap_pure.
  unfold first.
  simpl. reflexivity.
Defined.

Instance RSSTMonad : forall R W S (M : Type -> Type) (FM : Functor M) (AM : Applicative FM) (MM : Monad AM)
   , Monad (RSSTApplicative R W S MM) :=
{
  bind A B ma f :=
  fun r sw =>
    @bind M FM AM MM (A*(S*W)) (B*(S*W)) (ma r sw) (fun blob =>
        let (a,sw') := blob
        in  (f a) r sw')
}.
Proof.
- (* right unit *)
  intros.
  extensionality r. extensionality sw.
  unfold pure. simpl.
  rewrite funProductBreak. rewrite <- right_unit. reflexivity.
- (* left unit *)
  intros.
  extensionality r. extensionality sw.
  unfold pure. simpl.
  rewrite <- left_unit. reflexivity.
- (* associativity *)
  intros.
  extensionality r. extensionality sw.
  rewrite <- associativity. apply f_equal.
  extensionality sw'.
  destruct sw'. reflexivity.
- (* fmap return *)
  intros.
  extensionality r. extensionality sw.
  destruct sw.
  unfold fmap. simpl.
  rewrite <- fmap_return.
  unfold first.
  assert(RW: (fun blob : A * (S * W) => let (a, sw') := blob in pure (f a, sw')) = (fun blob : A*(S*W) => pure (f (fst blob), snd blob))).
  {
    extensionality x. destruct x. simpl. reflexivity.
  }
  rewrite RW. reflexivity.
- (* bind_fmap *)
  intros.
  extensionality r. extensionality sw.
  destruct sw.
  unfold composition, fmap. simpl.
  rewrite bind_fmap. unfold composition. unfold first.
  rewrite fstSnd. reflexivity.
Defined.

Class StateMonad (S : Type) (m : Type -> Type) (AF: Functor m) (AP: Applicative AF) (AM: Monad AP) := {
  get : m S;
  put : S -> m unit;
  stateLaw1: @bind m AF AP AM S unit get put = pure tt;
  stateLaw2: forall (s s' : S), bind unit (put s) (fun _ => put s') = put s';
  stateLaw3: forall (s : S), @bind m AF AP AM unit S (put s) (fun _ => get) = @bind m AF AP AM unit S (put s) (fun _ => pure s);
  stateLaw4: forall B (k : S -> S -> m B),
          @bind m AF AP AM S B get (fun s => @bind m AF AP AM S B get (fun s' => k s s'))
        = @bind m AF AP AM S B get (fun s => k s s)
}.

Instance RSSTState : forall R W S (M : Type -> Type) (FM : Functor M) (AM : Applicative FM) (MM : Monad AM)
   , StateMonad S (RSSTMonad R W S MM) :=
{
  get := fun r sw => pure (fst sw, sw);
  put := fun ns r sw => pure (tt, (ns, snd sw))
}.
Proof.
- extensionality r; extensionality sw.
  destruct sw. unfold pure at 3; simpl.
  repeat(rewrite <- left_unit). simpl.
  reflexivity.
- intros.
  extensionality r; extensionality sw.
  destruct sw.
  unfold bind at 1. simpl.
  repeat(rewrite <- left_unit). simpl.
  reflexivity.
- intros.
  extensionality r; extensionality sw.
  destruct sw.
  unfold bind at 1;simpl. 
  repeat(rewrite <- left_unit). simpl.
  reflexivity.
- intros.
  unfold bind at 1;simpl.
  extensionality r; extensionality sw.
  repeat(rewrite <- left_unit); simpl.
  reflexivity.
Defined.

Class Monoid W := {
  mempty : W;
  mappend : W -> W -> W;
  midleft: forall x, mappend mempty x = x;
  midright: forall x, mappend x mempty = x;
  massoc: forall a b c, mappend (mappend a b) c = mappend a (mappend b c)
}.

Class WriterMonad (W : Type) (m : Type -> Type) (AF: Functor m) (AP: Applicative AF) (AM: Monad AP) (MW: Monoid W):= {
  tell: W -> m unit;
  listen: forall A, m A -> m (A*W);
  pass: forall A, m (A*(W->W)) -> m A
}.

Instance RSSTWriter: forall R W S (M : Type -> Type) (FM : Functor M) (AM : Applicative FM) (MM : Monad AM) (MW: Monoid W)
  , WriterMonad (RSSTMonad R W S MM) MW :=
{
  tell := fun w _ sw =>
     let (s,ow) := sw in
     pure (tt, (s,mappend ow w));
  listen := fun A rw r sw =>
    let (s,w) := sw
    in  @bind M FM AM MM (A*(S*W)) (A*W*(S*W))
          (rw r (s,mempty))
          (fun blob => let (a,sw') := blob in let (ns,nw) := sw' in pure ((a,nw),(ns,mappend w nw)));
  pass := fun A rw r sw =>
    let (s,w) := sw
    in  @bind M FM AM MM (A*(W->W)*(S*W)) (A*(S*W)) (rw r (s,mempty))
            (fun blob =>
              let (afw,sw') := blob in
              let (a,fw) := afw in
              let (s',w') := sw' in
              pure (a,(s',mappend w (fw w'))))
}.

Definition lift {M : Type -> Type} {FM: Functor M} {AM: Applicative FM} {MM: Monad AM} {R} {W} {S} {A} :
  M A -> RSST R W S M A :=
     fun act _ sw => @bind M FM AM MM A (A*(S*W)) act (fun a => pure (a,sw)).


Theorem listenA R W S (M : Type -> Type) (FM: Functor M) (AM: Applicative FM) (MM: Monad AM) (MW: Monoid W):
  forall A (act: M A),
       @listen W (RSST R W S M) (RSSTFunctor R W S FM)
              (RSSTApplicative R W S MM) (RSSTMonad R W S MM) MW
              (RSSTWriter R S MM MW) A
      
   (lift act) = fmap (fun v => (v,mempty)) (lift act).
Proof.
  intros.
  unfold listen, fmap. simpl. unfold first.
  extensionality r. extensionality sw.
  destruct sw. unfold lift.
  rewrite <- associativity.
  rewrite fmap_return.
  rewrite fmap_dist. simpl.
  rewrite <- fmap_return.
  apply f_equal.
  extensionality a.
  rewrite <- left_unit.
  rewrite midright.
  reflexivity.
Qed.

