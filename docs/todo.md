Leaves don't need to know how much money they have because their parent stem knows.
We need to be able to randomly choose someone weighted by their balance.
We need a totally ram version of the trie and the ability to use the ram trie to update our hard drive trie efficiently. This is a batch-write.
We should also be able to read a RAM trie out of the hard drive given a root and list of leaves.

We probably should not need to use CFG to access trie. ID is enough.



%ram version
{23372646,{2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}

%hard drive version
{26785770,{2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}

based on measurements of the dump, the ram version should be about twice as fast.