Merkle Trie

the goal of this project is to:
1) have a merkel tree on the hard drive
2) the root hash of a trie is deterministicly derived from the contents, order doesn't matter.
3) the keys to access are integers increasing from 0.
4) the data being stored is fixed size.

Install process:

First you need erlang installed. Then to install this software do: ```sh install.sh```

Now to run the software simply: ```sh start.sh```


Example usage:

```
1> Value  = <<27,27>>.
<<"\e\e">>
2> Root0 = 0.
0
3> ID = trieID.
4> Size = 2, %this means that each object being stored is 2 bytes big, 
5> Max = 20000000000, %and we can hold up to 20 billion objects.
6> trie_sup:start_link(Size, Max, ID). 
7> {Key1, Root1} = trie:put(Value, Root0, ID).
{1000,7}
8> {A, Value, B} = trie:get(Key1, Root1, ID).
{<<96,197,126,39,139,128,190,152,237,187,104,211>>,
 <<"\e\e">>,
 [{<<155,11,34,50,227,53,170,141,122,216,37,246>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0>>,
   <<0,0,0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,0,...>>}]}
9> verify:proof(A, trie:to_path(Key1, ID), Value, B).
true
10> trie:garbage([Root1], ID). %this deletes everything that isn't a branch from one of the roots in the list.
ok
11> trie:garbage_leaves([{trie:to_path(Key1, ID), Root1}], ID). %this deletes everything that isn't needed to prove the existence of the leaf at Key1.
ok
```

here is an example of a project that uses this MerkleTrie: https://github.com/BumblebeeBat/PinkFairy