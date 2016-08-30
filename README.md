Merkle Trie

the goal of this project is to have a merkel tree on the hard drive for a blockchain.
It remembers fixed size pieces of data. You can choose the size of data in the .app file when you use rebar to include this library.

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
4> trie_sup:start_link(2, 20000000000, ID).
4> {Key1, Root1} = trie:put(Value, Root0, ID).
{1000,7}
5> {A, Value, B} = trie:get(Key1, Root1, ID).
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
6> verify:proof(A, trie:to_path(Key1, ID), Value, B).
true
7> trie:garbage([Root1], ID). %this deletes everything that isn't a branch from one of the roots in the list.
ok
8> trie:garbage_leaves([{trie:to_path(Key1, ID), Root1}], ID). %this deletes everything that isn't needed to prove the existence of the leaf at Key1.
ok
```
