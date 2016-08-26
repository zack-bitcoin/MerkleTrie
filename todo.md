


I want to upgrade the trie to be a patricia trie.
Hash collision attacks can use up many times as much memory at almost no additional cost. For example, if an attacker added 2 keys to the database such that the hash of each key started with identical 20 bytes. As it is currently written, we would need to add 40 stems and 2 leaves to the database, which is a lot of memory to keep track of.
The way to do this is to have 2 types of stems. One stem has 16 pointers and 1 spot for data. The other stem has only 1 pointer and part of the key.

The ethereum team describes it here:

https://github.com/ethereum/wiki/wiki/Patricia-Tree


Currently the stems and leaves are stored as fixed sized binary on the hard drive. But most stems are mostly empty. We could save a lot of space if we compress stems before writing them on the hard drive.
