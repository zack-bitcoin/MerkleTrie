%Key = bits:top(ID),%this is a problem. If we start making a new trie from the same root, we don't want to keep incrementing addresses, this is an example of non-determinism, different nodes would assign different ID numbers to the same accounts.
    %where the "top" address is should depend on which root we are writing to. Each root should probably store a "top" along with it. So stems will need to be a little bigger.
    %storing a "top" along with each root isn't enough, we would need to store an entire "bits" object, which is one bit for every address being used. Maybe with compression this isn't so bad?
    %we are probably better off letting users select their own ID numbers within the accepted range, instead of assigning them.




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