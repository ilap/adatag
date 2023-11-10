# Brief of Labeled Tree

- The tree model **`T`** representing the set **`S = {x1, ..., xn}`** is and must always be a **`Complete Binary Tree`**.
- A `Complete Binary Tree` can have an incomplete last level, as long as all the leaves in that level are arranged from left to right.
- A **node** (**`N`**) of the `T` tree has three pointers: one to its parent, to its left child, and to its right child.
- A **leaf** (**`L`**) is a node  that has two empty hashes (**`ε', ε"`**) as its children.
- Every node in `T` must be assigned a sequential index **`i`** starting from **`1`**, with the root of T having index 1, and this index `i` increases by 1 whenever a new node **`Ni`** is added. The value of `i` is always equal to the number of elements in `T`.
- The depth (**`d`**) is defined as the length of the simple path (number of edges) from the `root` node of `T` to node `N`.

- The node's index (`i`) must always be exactly double of its parent's index if it's on the left, or 2 times of the parent's index plus 1 if it's on the right.
- The integrity of T is ensured by the sequential index (`i`) and depth (`d`).
- Therefore the node index determines the position of any node within the 'T' tree and positions in its parent, except for the root node (as it doesn't have a parent).
  - The position of a node in the tree is determined by **`P = i - 2^d + 1`** which means it's the **`Pth`** node on level `d` of the `T` tree.
  - The **left**/**right** position within a node follows the **`even-odd`** rule:  if its index `i` is even, the child is on the left side; otherwise, it's on the right. This position is determined by **`Pn = In - 2Ip`** (`Ip` is the parent's index)  where `Pn` is 0 when the child is on the left, 1 when it's on the right, and results in an **error** otherwise.
- In order to keep track of changes in the state of the `T` tree, there are two types of nodes:
  - Updateable node (**`Nu`**): These nodes are already part of the tree, but their values will be updated sometime by an update process.
  - Appendable node (**`Na`**): These are then nodes where a new leaf `L` will be appended to.
- The right child (`Nr`) of an appendable node (`Na`), must always be an empty hash, represented as **`ε`**. The left child can be either an empty hash (**`ε`**) or a leaf.
