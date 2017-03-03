#

<div style="text-align: center; font-size: 200%; margin-top: 2em;">
test.check and uniform sampling
</div>

# Outline

- Introduction to random testing

- Zoom-in on uniform sampling of recursive datatypes

- Study some "real world" examples

- Zoom-out to the many spin-offs and applications of QuickCheck

#

<div style="text-align: center; font-size: 200%; margin-top: 2em;">
test.check
</div>

# Counting Sort

We want to sort:

<table>
 <tr class="array">
  <td>2</td>
  <td>5</td>
  <td>1</td>
  <td>6</td>
 </tr>
</table>

We count the number of occurences of 1 ... 6 in a temporary array

<table>
 <tr class="array">
  <td>1</td>
  <td>1</td>
  <td>0</td>
  <td>0</td>
  <td>1</td>
  <td>1</td>
 </tr>
 <tr class="array-legend">
  <td>1</td>
  <td>2</td>
  <td>3</td>
  <td>4</td>
  <td>5</td>
  <td>6</td>
 </tr>
</table>

We scan the temporary array from left to right to build the sorted list

<table>
 <tr class="array">
  <td>1</td>
  <td>2</td>
  <td>5</td>
  <td>6</td>
 </tr>
</table>


# Counting Sort - Implementation

~~~~{.clj}
(defn counting-sort [xs]
  (let [len (+ (apply max xs) 1)
        counts (int-array len)]
    (doseq [i xs]
      (aset-int counts i (+ 1 (aget counts i))))
    (let [res (transient [])]
      (dotimes [i len]
        (if (pos? (aget counts i))
          (conj! res i)))
      (persistent! res))))
~~~~

. . .

~~~{.clj}
(counting-sort [42])     ; => [42]
(counting-sort [2 5 1])  ; => [1 2 5]
~~~

Are we good?

# Counting Sort - Testing

We check this property

~~~{.clj}
(def counting-sort-correctness
  (prop/for-all [v (gen/vector gen/int)]
                (= (counting-sort v) (sort v))))

~~~

against many random lists.

~~~{.clj}
(tc/quick-check 100 counting-sort-correctness)
~~~

# Counting Sort - Testing

~~~{.clj}
{:result #error {
 :cause "Wrong number of args (0) passed to: core/max"
 :via
 [{:type clojure.lang.ArityException
   :message "Wrong number of args (0) passed to: core/max"
   :at [clojure.lang.AFn throwArity "AFn.java" 429]}]
 :trace [...]}, :seed 1486908296933, :failing-size 0, :num-tests 1,
 :fail [[]],
 :shrunk {:total-nodes-visited 0, :depth 0, :result #error {
 :cause "Wrong number of args (0) passed to: core/max"
 :via
 [{:type clojure.lang.ArityException
   :message "Wrong number of args (0) passed to: core/max"
   :at [clojure.lang.AFn throwArity "AFn.java" 429]}]
 :trace [...]}, :smallest [[]]}}
~~~

# Counting Sort - Testing

~~~{.clj}
{
        "Wrong number of args (0) passed to: core/max"












                :smallest [[]] }
~~~

#  Counting Sort - Take 2

~~~{.clj}
(defn counting-sort [xs]
  (let [len (+ (apply max xs) 1)
        counts (int-array len)]
    (doseq [i xs]
      (aset-int counts i (+ 1 (aget counts i))))
    (let [res (transient [])]
      (dotimes [i len]
        (if (pos? (aget counts i))
          (conj! res i)))
      (persistent! res))))
~~~

#  Counting Sort - Take 2

~~~{.clj hl_lines="2 3"}
(defn counting-sort [xs]
  (if (empty? xs)
    xs
    (let [len (+ (apply max xs) 1)
          counts (int-array len)]
      (doseq [i xs]
        (aset-int counts i (+ 1 (aget counts i))))
      (let [res (transient [])]
        (dotimes [i len]
          (if (pos? (aget counts i))
            (conj! res i)))
        (persistent! res)))))
~~~

. . .

~~~{.clj}
{:result #error { ... java.lang.ArrayIndexOutOfBoundsException ... }
 ...
 :smallest [[-1]]}
~~~

#  Counting Sort - Take 3

~~~{.clj hl_lines="4 5 6 9 14"}
(defn counting-sort [xs]
  (if (empty? xs)
    xs
    (let [maximum (apply max xs)
          minimum (apply min xs)
          len (+ (- maximum minimum) 1)
          counts (int-array len)]
      (doseq [i xs]
        (let [index (- i minimum)]
          (aset-int counts index (+ 1 (aget counts index)))))
      (let [res (transient [])]
        (dotimes [i len]
          (if (pos? (aget counts i))
            (conj! res (+ i minimum))))
        (persistent! res)))))
~~~

. . .

~~~{.clj}
{:result false, ..., :smallest [[5 5]]}}
~~~

#  Counting Sort - Take 3

~~~{.clj hl_lines="4 5 6 9 14"}
(defn counting-sort [xs]
  (if (empty? xs)
    xs
    (let [maximum (apply max xs)
          minimum (apply min xs)
          len (+ (- maximum minimum) 1)
          counts (int-array len)]
      (doseq [i xs]
        (let [index (- i minimum)]
          (aset-int counts index (+ 1 (aget counts index)))))
      (let [res (transient [])]
        (dotimes [i len]
          (if (pos? (aget counts i))
            (conj! res (+ i minimum))))
        (persistent! res)))))
~~~

~~~{.clj}
{:result false, ..., :smallest [[5 5]]}}
{:result false, ..., :smallest [[2 2]]}}
~~~

#  Counting Sort - Take 3

~~~{.clj hl_lines="4 5 6 9 14"}
(defn counting-sort [xs]
  (if (empty? xs)
    xs
    (let [maximum (apply max xs)
          minimum (apply min xs)
          len (+ (- maximum minimum) 1)
          counts (int-array len)]
      (doseq [i xs]
        (let [index (- i minimum)]
          (aset-int counts index (+ 1 (aget counts index)))))
      (let [res (transient [])]
        (dotimes [i len]
          (if (pos? (aget counts i))
            (conj! res (+ i minimum))))
        (persistent! res)))))
~~~

~~~{.clj}
{:result false, ..., :smallest [[5 5]]}}
{:result false, ..., :smallest [[2 2]]}}
{:result false, ..., :smallest [[7 7]]}}
~~~

#  Counting Sort - Take 4

~~~{.clj hl_lines="13"}
(defn counting-sort [xs]
  (if (empty? xs)
    xs
    (let [maximum (apply max xs)
          minimum (apply min xs)
          len (+ (- maximum minimum) 1)
          counts (int-array len)]
      (doseq [i xs]
        (let [index (- i minimum)]
          (aset-int counts index (+ 1 (aget counts index)))))
      (let [res (transient [])]
        (dotimes [i len]
          (dotimes [_ (aget counts i)]
            (conj! res (+ i minimum))))
        (persistent! res)))))
~~~

. . .

~~~{.clj}
{:result true, :num-tests 100, :seed 1486910470345}
~~~

# test.check

test.check tries to falsify a property by feeding it with increasingly large random inputs
<br/>
<br/>

. . .

- Originally a Haskell library by Hughes et al.

- Now ported to numerous languages

- Many spin-offs: SmallCheck, Lazy SmallCheck, SmartCheck, QuickSpec, ...

<div class="notes">Initially a Haskell library by Hughes</div>

# Examples of properties

## Post Conditions
<br/>

- ∀ list, isSorted(sort(list))

- ∀ tree, isBalanced(balance(tree))

- ∀ graph, areStronglyConnected(scc(graph))

- ∀ program, valid(program) ⇒ compiles(program)

- ∀ s1 s2, isPrefix(s1, s2) ⇒ ∃ s3, s1 + s3 = s2

# Examples of properties

## Algebraic properties
<br/>

- ∀ list, reverse(reverse(list)) = list

- ∀ ast, parse(pretty(ast)) = ast

- ∀ list f g, map(g, map(f, list)) = map(g ∘ f, list)

- ∀ x ∈ M, x • e = x\ \ \ \ \ for some monoid M(•, e)

# Examples of properties

## Checking against a model
<br/>

- ∀ list, mySort(list) = trustedSort(list)

- ∀ graph v₁ v₂, djikstra(graph, v₁, v₂) = naiveShortestPath(graph, v₁, v₂)

- ∀ x, fastButcomplex(x) = obviouslyCorrectButSlow(x)

# Random Inputs

test.check is a DSL for writing generators.

~~~{.clj}
(gen/int) ; generators of integers

(gen/vector gen/int) ; vectors of integers

(gen/map gen/int gen/string) ; maps from integers to strings

(gen/choose 1 10) ; integers between 1 and 10
~~~

. . .

~~~{.clj}
(defrecord User [user-name user-id email active?])

(def user-gen
  (gen/fmap (partial apply ->User)
            (gen/tuple (gen/not-empty gen/string-alphanumeric)
                       gen/nat
                       email-gen
                       gen/boolean)))
~~~

#

<div style="text-align: center; font-size: 200%; margin-top: 2em;">
Recursive Datatypes
</div>


# Recursive Datatypes

With test.check, how do you generate

- A random binary tree?

- A random JSON object?

- A random AST?

- A random grammar?

. . .

<br/>
You don't! You're on your own.

. . .

(Actually starting from 0.5.9 there is something, but it's kind of broken)

# Generating a Random Binary Tree

<img src="img/exampleTree.png" style="margin-top:-1em;margin-right:2em;float:right;"></img>

~~~{.clj}
(defrecord Node [left-child right-child])

(def example-tree
  (->Node
   (->Node
    nil
    (->Node nil nil))
   (->Node
    (->Node nil nil)
    (->Node nil nil))))
~~~


We want to define

~~~{.clj}
(defn random-tree [] ...)
~~~


# Generating a Random Binary Tree

~~~{.clj}
(defn random-tree []
  (if (zero? (rand-int 2))
    nil
    (->Node (random-tree) (random-tree))))
~~~

. . .

What's wrong with this?


# Generating a Random Binary Tree

<img src="img/gen1.png" style="height: 550px; width: auto; margin-left: 5em;"></img>

# Generating a Random Binary Tree

<img src="img/gen6.png" style="height: 550px; width: auto; margin-left: 5em;"></img>

# Generating a Random Binary Tree - Take 2

Four more chances to pick a branch than a leaf

~~~{.clj hl_lines="2"}
(defn random-tree []
  (if (zero? (rand-int 5))
    nil
    (->Node (random-tree) (random-tree))))
~~~

(Same can be tried with any n>2, doesn't change the outcome.)

# Generating a Random Binary Tree - Take 2

<img src="img/gen2.png" style="height: 550px; width: auto; margin-left: 5em;"></img>

# Generating a Random Binary Tree - Take 3

Limiting the depth

~~~{.clj}
(defn random-tree [depth]
  (cond
    (zero? depth) nil
    (zero? (rand-int 5)) nil
    :else (->Node
           (random-tree (- depth 1))
           (random-tree (- depth 1)))))
~~~

~~~{.clj}
#(random-tree 5)
~~~

# Generating a Random Binary Tree - Take 3

<img src="img/gen3.png" style="height: 550px; width: auto; margin-left: 5em;"></img>

# Generating a Random Binary Tree - Take 3

<img src="img/randomtreesmaxdepth.png" style="height: 550px; width: auto; margin-left: 4em;"></img>

# Generating a Random Binary Tree - Take 4

"Limit the size of the generated test data" (QuickCheck original paper)

~~~{.clj}
(defn random-tree [n]
  (cond
    (zero? 0) nil
    (zero? (rand-int 5)) nil
    :else (->Node
           (random-tree (quot n 2))
           (random-tree (quot n 2)))))
~~~

~~~{.clj}
#(random-tree 30)
~~~

# Generating a Random Binary Tree - Take 4

<img src="img/gen4.png" style="height: 550px; width: auto; margin-left: 5em;"></img>

# Generating a Random Binary Tree - Take 4

<img src="img/randomtreesqc.png" style="height: 550px; width: auto; margin-left: 4em;"></img>

# Generating a Random Binary Tree - Take 5

At each step, randomly split the amount of available nodes between left and right, recurse. 

~~~{.clj}
(defn random-tree [n]
  (if (zero? n)
    nil
    (let [p (rand-int n)]
      (->Node (random-tree p) (random-tree (- n 1 p))))))
~~~

~~~{.clj}
#(random-tree 30)
~~~

# Generating a Random Binary Tree - Take 4

<img src="img/gen5.png" style="height: 550px; width: auto; margin-left: 5em;"></img>

# Generating a Random Binary Tree - Take 4

<img src="img/randomtreesgood.png" style="height: 550px; width: auto; margin-left: 4em;"></img>

# Victory!

Not so fast! What about `Tree[T]`? (e.g. Tree[Int])

~~~{.clj}
(defrecord LNode [label left-child right-child])
~~~

. . .

\ \ \ Use same algorithm, pick a random label for each node?

\

Consider `T = Tree[Tree[Tree[int]]]`


> - If we use n = 30 for each label then the result is huge (30⁴ nodes!), also some values are missing
> - If we use n = random(0, 30) for each label then worst case is still huge

# Wait, do we care?

Consider the abstract syntax trees of some typed programming language (e.g. java).

- Blocks are trees labelled by

- ... expressions, which are trees labelled by

- ... types, which are themselves trees

# Generating a Random Labelled Tree

Not so fast! What about `Tree[T]`? (e.g. Tree[Int])

~~~{.clj}
(defrecord LNode [label left-child right-child])
~~~

\ \ \ Use same algorithm, pick a random label for each node?

\

Consider `T = Tree[Tree[Tree[int]]]`


- If we use n = 30 for each label then the result is huge (30⁴ nodes!), also some values are missing
- If we use n = random(0, 30) for each label then worst case is still huge


# Generating a Random Labelled Tree

What if we split n between label, left and right and recurse?

. . .

~~~{.clj}
(defn random-tree [n]
  (if (zero? n)
    nil
    (let [[k l r] (random-triplet-of-sum (- n 1))]
      (->LNode
       (random-label k)
       (random-tree l)
       (random-tree r)))))
~~~

# Generating a Random Labelled Tree

<img src="img/randomLTreesBad.png" style="height: 550px; width: auto; margin-left: 10em;"></img>

# Generating a Random Labelled Tree

<img src="img/size_by_depth_bad.png" style="height: 550px; width: auto; margin-left: 5em;"></img>

# Is there any hope?

Agata (Jonas Duregård) improves on the splitting idea by dividing the size across types of same "dimension".

Writing such generators manually is virtually impossible. Generating the generators requires meta-programming.
\
\

<img src="img/agata.png" style="height: 200px; width: auto; margin-left: 4em;"></img>

. . .

There is a much simpler and elegant solution though!

# FEAT : Functional Enumeration of Algebraic Types

- A paper/library by Jonas Duregård et al.

- Builds an indexing function into the set of values of size n (e.g., "give me the 10th binary tree of size 20")

- Straightforward user interface:

~~~{.haskell}
data Tree = Leaf | Branch Tree Tree

instance Enumerate Tree where
  enumerate = consts [nullary Leaf, binary Branch]

someTree = select trees 30 10
~~~

. . .

<br/>
How does it work? Let's start by generating the set of all trees of size n.

# Trees of Size 0

<img src="img/trees0.png" style="height: 500px; width: auto; margin-left: 6em;"></img>

# Trees of Size 1

## 1 = 1 + 0 + 0

<img src="img/trees1.png" style="height: 500px; width: auto; margin-left: 6em;"></img>

# Trees of Size 2

## 2 = 1 + (0 + 1 | 1 + 0)

<img src="img/trees2.png" style="height: 500px; width: auto; margin-left: 6em;"></img>

# Trees of Size 3

## 3 = 1 + (0 + 2 | 1 + 1 | 2 + 0)

<img src="img/trees3.png" style="height: 500px; width: auto; margin-left: 6em;"></img>

# Trees of Size 4

## 4 = 1 + (0 + 3 | 1 + 2 | 2 + 1 | 3 + 0)

<img src="img/trees4.png" style="height: 500px; width: auto; margin-left: 6em;"></img>

# And so on

<img src="img/trees6.png" style="height: 500px; width: auto; margin-left: 6em;"></img>\ \ ...

# More Formally

~~~{.clj}
(def trees-0
  [nil])

(def trees-1
  (map node
       (product trees-0 trees-0)))

(def trees-2
  (map node
       (concat (product trees-0 trees-1)
               (product trees-1 trees-0))))

(def trees-3
  (map node
       (concat (product trees-0 trees-2)
               (product trees-1 trees-1)
               (product trees-2 trees-0))))

...
~~~

~~~{.clj}
(defn product [s1 s2] (for [x s1 y s2] (list x y))) 
(defn node [[l r]] (->Node l r))
~~~

# Key Idea: Don't Expand

~~~{.clj}
(def trees-0
  (->Singleton nil))

(def trees-1
  (->Map node
         (->Product trees-0 trees-0)))

(def trees-2
  (->Map node
         (->Concat (->Product trees-0 trees-1)
                   (->Product trees-1 trees-0))))

(def trees-3
  (->Map node
         (->Concat (->Product trees-0 trees-2)
                   (->Product trees-1 trees-1)
                   (->Product trees-2 trees-1))))

...
~~~

# More Precisely

- Finite sets are represented as expressions formed over singletons, product, union, and map

- Each of these sets has an easily computable cardinal, that we cache

~~~{.clj}
(defrecord Singleton [cardinal value])
(defrecord Map       [cardinal function set])
(defrecord Product   [cardinal set-1 set-2])
(defrecord Union     [cardinal set-1 set-2])
~~~

- This cardinal allows for efficient indexing into the set using simple modulo arithmetic

# Indexing Finite Sets

s₃ = map(Branch, (s₀ × s₂) ∪ (s₁ × s₁) ∪ (s₀ × s₂))

<img src="img/fig3.png" style="height: 500px; width: auto; margin-left: 6em;"></img>

# Indexing Finite Sets

s₃ = map(Branch, (s₀ × s₂) ∪ (s₁ × s₁) ∪ (s₀ × s₂))

<img src="img/fig4.png" style="height: 500px; width: auto; margin-left: 6em;"></img>

# Indexing Finite Sets

s₃ = map(Branch, (s₀ × s₂) ∪ (s₁ × s₁) ∪ (s₀ × s₂))

<img src="img/fig5.png" style="height: 500px; width: auto; margin-left: 6em;"></img>

# Indexing Finite Sets

s₃[4] = ?

<img src="img/fig6.png" style="height: 500px; width: auto; margin-left: 6em;"></img>

# Indexing Finite Sets

s₃[4] = map(Branch, (s₂ × s₀)[1]

<img src="img/fig7.png" style="height: 500px; width: auto; margin-left: 6em;"></img>


# Indexing Finite Sets

s₃[4] = Branch(s₂[1] × s₀[0])

<img src="img/fig8.png" style="height: 500px; width: auto; margin-left: 6em;"></img>

# Indexing Finite Sets

s₃[4] = Branch(map(Branch, (s₁ × s₀)[0]), s₀[0])

<img src="img/fig9.png" style="height: 500px; width: auto; margin-left: 6em;"></img>

# Indexing Finite Sets

s₃[4] = Branch(Branch(s₁[0] × s₀[0]), s₀[0])

<img src="img/fig10.png" style="height: 500px; width: auto; margin-left: 6em;"></img>

# Implementation Notes

Cardinals get huge: requires big integers

~~~
> (:card (trees 200))
512201493211017079467541693136328292324432464582475861864920694407578768023144072628540276213813397768975366156750120N
~~~


<br/>
Indexing easily hits the max stack size: naive implementation won't work

~~~
> (f-nth (trees 300) 100)
StackOverflowError   clojure.lang.AFn.applyToHelper (AFn.java:148)
~~~

# Generalization

- Works for any algebraic datatype

- Can even be used to enumerate some non-algebraic datatypes!

    - Sets
    - Maps
    - Rationals
    - ...

    Done by applying clever bijections to finite sets


# QuickCheck, Reloaded

How to pick a random value of size n, *uniformly*:

1. Compute sₙ

2. Pick a random number i between 0 and cardinal(sₙ)

3. Retrieve sₙ[i]

Et voilà!

# Random Labelled Trees Done Right

<img src="img/randomLTreesGood.png" style="height: 550px; width: auto; margin-left: 10em;"></img>

# Random Labelled Trees Done Right

<img src="img/size_by_depth_good.png" style="height: 550px; width: auto; margin-left: 5em;"></img>

# QuickCheck, Alternative

Instead of drawing random values, one can also simply enumerate sₙ[0], sₙ[1], sₙ[2], ...

. . .

Viable alternative: many wrong properties fail for small values.

#

<div style="text-align: center; font-size: 200%; margin-top: 2em;">
Examples
</div>

# Hash Array Mapped Trie (Demo)

<img src="img/HAMT.svg" style="height: 500px; width: auto; margin-left: 7em;"></img>

. . .

We test that `(map1 == map2) => (map1.hashCode == map2.hashCode)`

# Pretty-Printing Library (Demo)

Implementation of Phil Waldler's "A prettier printer".

Pretty-printing combinators:

~~~{.clj}
(group (concat "aaa" line "bbb"))
~~~

Will render as

~~~
aaa
bbb
~~~

or 

~~~
aaa bbb
~~~

depending on the specified width.

. . .

Semantics: layout that minimizes vertical space while fitting horizontally

# Priority Queues (Demo)

Priority queues with fast element removal.

Testing strategy: enumerate **programs** of the form

~~~{.dart}
q = new Queue()
q.add(3)
q.add(1)
q.peek()
q.removeMax()
q.peek()
~~~

Compare the program's execution trace between implementation and model.

. . .

This technique is described in "Testing Monadic Code with QuickCheck".

# HGom

Haskell tool that compiles specifications of the form

~~~
List = Nil() | Cons(head: int, tail: List)
~~~

into java code of the form

~~~
interface List { ... }
class Nil implements List { ... }
class Cons implements List { ... }
~~~

. . .

<br/>
Property: ∀ spec, valid(spec) ⇒ javacSucceeds(compile(spec))

# Acumen Language

Language for physics simulation.

~~~
class BouncingSpringBall1 (x,x',x'')
  x'' = - 9.8 + (1/x^2)
end
~~~

Two interpreters: 

- a pure reference one
- a parallel side-effectful efficient one

. . .

Property:\ \ \ ∀ program, slow-interpreter(program) = fast-interpreter(program)

# Pattern Matching

Found some minimal counter-example in some pattern subsumption algorithm.  

A pattern subsumes another pattern if all its possible instantiations are a super of all the other pattern's possible instantiations. For instance: `(cons x xs)` subsumes `(cons 1 xs)`.  

Found this pattern using FEAT:  

~~~
interp(s(s(z())), cons(x, cons(bv(x), x)))
~~~

<br/>

Lead the authors to revise the paper before publication.

# Xor4

What is the smallest expression using `or`, `and` and `xor` that implements `xor4`?

Example:

~~~
and(or(xor(a, b), xor(c, d)), xor(or(a, b), or(c, d)))
~~~

. . .

<br/>
Wrote a C++ mini-feat and map-reduced it. Each shard tests a range [i .. i+n].

#

<div style="text-align: center; font-size: 200%; margin-top: 2em;">
QuickCheck Spin-offs
</div>

# All QuickCheck Ports

QuickCheck has been ported to virtually all languages.

- Erlang has the best port
- Clojure's port is one of the best

<br/>

It also has inspired a whole line of research and spin-offs.

# SmallCheck

Lazily enumerates **all trees** up to a certain depth.

~~~{.clj}
(defn trees-up-to-depth [depth]
  (lazy-seq
   (if (zero? depth)
     (list nil)
     (let [rec (trees-up-to-depth (- depth 1))
           trees (for [l rec r rec] (->Node l r))]
       (cons nil trees)))))
~~~

. . .

Useful, but has some drawbacks:

- No random access

- Seemingly small values like `[0 0 0 0 0 0 0]` are virtually out of reach

- Values of depth d tend to be saturated

# All Binary Trees of Depth 4

<img src="img/smalltreesofdepth4.png" style="height: 550px; width: auto; margin-left: 6em;"></img>

# Lazy SmallCheck

Same as SmallCheck except it agressively cuts branches.

For example:

~~~{.haskell}
property list = (head list == 1) ==> ...
~~~

Lazy smallcheck will stop trying any [1, ...] after (1:⊥) fails. 

. . .

<br/>
Relies heavily on Haskell's execution model.

# "Lazy FEAT"

*Generating Constrained Random Data with Uniform Distribution* by Koen Claessen et al.

<br/>
Like LazySmallcheck but for FEAT. Way harder to implement.

<br/>
Enables the efficient enumeration of well-typed programs of size n:  

\ \ \ ∀ program, typechecks(prog) ⇒ run(prog, flags1) = run(prog, flags2)

# SmartCheck

Shrink on steroids: gets rid of the noise in counter-examples.
<br/>

Example from XMonad:

~~~
forall values x0 x1 x2 x3:
  StackSet
    (Screen (Workspace x0 (-1) (Just x1)) 1 1)
    x2 x3 (fromList [])
~~~

# QuickSpec (Demo)

Magically finds "theorems" about your code

- Input : a set of functions
- Output : a set of laws

Example input:

~~~
[]
++
~~~

Example output:

~~~
[] ++ x = x
x ++ [] = x
(x ++ y) ++ z = x ++ (y ++ z)
~~~

# Other Applications / Spin-Offs

- Isabelle: find counter-examples to conjectures
- Simulant: generate random RPC/HTTP traffic
- QuickCheck + Selenium : generate random UI interactions
- "Lemma discovery" for automated theorem proving via QuickSpec
- Operational semantics testing in Redex
- Galois: generates C code and checks behavior against specification
- ...

# Take-Away Points

- QuickCheck is *very* good at finding bugs
- QuickCheck forces you to reason about your programs
- It is hard but not impossible to generate random values of recursive datatypes
- Generating random values for your domain enables lots of awesome applications
