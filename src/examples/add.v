Require Extraction.
Require Import Arith.

Extract Inductive bool => "bool" ["true" "false"].
Extract Inductive nat => "nat" ["0" "fun x -> x + 1"]
                               "(fun zero succ n -> if n = 0 then zero () else succ (n-1))".
Extract Constant plus => "( + )".

Definition add_two_num (m n : nat) := plus m n.

Proposition add_two_num_satisfies_the_specification:
  forall m n, add_two_num m n = plus m n.
Proof.
  intros m n.
  reflexivity.
Qed.

Recursive Extraction add_two_num.
