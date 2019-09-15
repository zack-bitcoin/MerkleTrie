Merkle Trie

This merkel tree database is sparse. That means you can make a proof to prove the non-existance of some data.

This tree can be configured to use either ram or hard drive for storage.

The root hash of a trie is deterministicly derived from the contents, order of insertion/deletion doesn't matter.

This is an order 16 radix tree. every node of the tree has 16 children.


Install process:

First you need erlang installed. 

Now to run the software simply: ```sh start.sh```

For examples on how to use it, look at [this](src/test_trie.erl)

here is an example of a project that uses this MerkleTrie: https://github.com/zack-bitcoin/amoveo
