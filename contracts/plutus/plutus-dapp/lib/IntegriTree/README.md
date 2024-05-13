# Merkle vs. Labeled Tree

The insert operation is O(1) in a complete binary tree because the tree is always balanced. This means that there is always a place to insert a new node without having to rebalance the tree.

The complexity of constructing a Merkle tree or a complete binary tree from a list or array is O(n). This is because the tree must be built from the bottom up, and each node must be inserted into the tree one at a time.

In general, Merkle trees are more complex than complete binary trees, but they also offer some advantages. For example, Merkle trees can be used to verify the integrity of data, while complete binary trees are not.

## Complexity of Merkle trees and complete binary trees:

| Feature            | Merkle Tree                                                      | Complete Binary Tree                                                             |
| ------------------ | ---------------------------------------------------------------- | -------------------------------------------------------------------------------- |
| Purpose            | Verify data integrity                                            | Efficient searching and insertion                                                |
| Level completeness | All levels filled                                                | Last level may have missing nodes                                                |
| Structure          | Not necessarily balanced nor complete, with cryptographic hashes | Always balanced (not necesserally complete) tree with parent-child relationships |
| Complexity         | Space: O(n)                                                      | Space: O(n)                                                                      |
|                    | Search: O(log n)                                                 | Search: O(log n)                                                                 |
|                    | Insert: O(log n)                                                 | Insert: O(1)                                                                     |
|                    | Delete: O(log n)                                                 | Delete: O(log n)                                                                 |
| Construction       | O(n) from list or array                                          | O(n) from list or array                                                          |
| Additional Notes   | May require rebalancing after insertion or deletion              | Always balanced, no need for rebalancing                                         |

## Tree features

| Feature             | Balanced Tree | Complete Binary Tree | Unbalanced Tree |
| ------------------- | ------------- | -------------------- | --------------- |
| Depth of leaf nodes | Same          | Same                 | Different       |
| Level completeness  | Any           | Completely filled    | Any             |
| Hash calculation    | Efficient     | Efficient            | Inefficient     |
| Implementation      | Easier        | Easier               | More difficult  |

## Update Proof Rules -- DEPRECATED. it's too complex.

Proof rules for adding an element into the tree

| **#** | **Plca** | **Pl** | **Pr** | **Ok** | **Comment (assuming x valid)**                 |
| ----- | -------- | ------ | ------ | ------ | ---------------------------------------------- |
| 1     | ValHash  | Nu     | Na     | ok     |                                                |
| 2     | ValHash  | Na     | Nu     | ok     |                                                |
| 3     | Nu       | Hash   | Na     | ok     |                                                |
| 4     | Nu       | Na     | Hash   | ok     |                                                |
| 5     | Na       | Nu     | eH     | ok     |                                                |
| 6     | Na/Nu    | Hash   | eH     | ok     |                                                |
| 7     | Na/Nu    | eH     | eH     | ok     | Na==Nu & e is valid                            |
|       | ValHash  | Any    | Any    | nok    | Any other than Na, Nu or Nu, Na<br>are invalid |
|       | Hash     | Any    | Any    | nok    | Any other than Na, Nu or Nu, Na<br>are invalid |
|       | Nu       | Any    | Any    | nok    | Any other than Na, H or H, Na are<br>invalid   |
|       | Na       | eH     | eH     | nok    | Any Na with Pr == eH are invalid               |
