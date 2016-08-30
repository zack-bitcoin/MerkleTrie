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
3> ID = trieID,
3> {Key1, Root1} = trie:put(Value, Root0, ID).
{1000,7}
4> {A, Value, B} = trie:get(Key1, Root1, ID).
{<<111,152,253,167,118,87,135,8,237,82,129,92>>,
 <<"\e\e">>,
 [{<<253,167,171,40,133,247,8,13,11,174,246,99>>,
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
   <<244,186,52,52,132,113,125,109,58,8,...>>,
   <<0,0,0,0,0,0,0,0,0,...>>}]}
5> verify:proof(A, trie:to_path(Key1), Value, B).
true
6> trie:garbage([Root1], ID). %this deletes everything that isn't a branch from one of the roots in the list.
ok
7> trie:garbage_leaves([{trie:to_path(Key1), Root1}], trie:m(ID), ID). %this deletes everything that isn't needed to prove the existence of the leaf at Key1.
ok
```
