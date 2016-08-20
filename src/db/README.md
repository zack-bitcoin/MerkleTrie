This file implements a key-value store with fixed-sized data.
The Trie uses 2 of these key-value stores.
One holds the stems, which show the paths of the tree.
The other holds leaves, which are the data in the tree.

"low" is the interface of commands that the rest of the program use to read and write stems and leaves.