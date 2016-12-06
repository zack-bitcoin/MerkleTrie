get:get.
If we garbage collect part of the trie, and try to get from it, we need a useful error message.
It should not look the same as reading from an address that isn't written to.



Leaves don't need to know how much money they have because their parent stem knows.
We need a totally ram version of the trie and the ability to use the ram trie to update our hard drive trie efficiently. This is a batch-write.
We should also be able to read a RAM trie out of the hard drive given a root and list of leaves.



%ram version
{23372646,{2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}

%hard drive version
{26785770,{2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}

based on measurements of the dump, the ram version should be about twice as fast.