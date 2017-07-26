test_trie:test(6, _) used to do a test where we restored info to a trie that was no longer existing.
This test is no longer passing, it needs to be fixed.



check out this explanation: http://blog.notdot.net/2009/12/Damn-Cool-Algorithms-Log-structured-storagexs

We want to keep track of some more data in each leaf. This new data should not be hashed into the merkel tree. 


store:store/5 needs to be fixed. there are comments on how.


get:get.
If we garbage collect part of the trie, and try to get from it, we need a useful error message.
It should not look the same as reading from an address that isn't written to.



We need a totally ram version of the trie and the ability to use the ram trie to update our hard drive trie efficiently. This is a batch-write.
We should also be able to read a RAM trie out of the hard drive given a root and list of leaves.



%ram version
{23372646,{2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}

%hard drive version
{26785770,{2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}}

based on measurements of the dump, the ram version should be about twice as fast. I am not sure why it isn't.