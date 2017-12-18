Merkle Trie

the goal of this project is to:
1) have a merkel tree database on the hard drive
2) the root hash of a trie is deterministicly derived from the contents, order of insertion/deletion doesn't matter.
3) we can look up the state at any point in history
4) append-only immutable datastructure

Install process:

First you need erlang installed. 

Now to run the software simply: ```sh start.sh```

For examples on how to use it, look at [this](src/test_trie.erl)

here is an example of a project that uses this MerkleTrie: https://github.com/zack-bitcoin/amoveo
