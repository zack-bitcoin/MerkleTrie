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
1> Value = 5.
5
2> Root0 = 0.
0
3> ID = trieID.
trieID
4> Size = 2.
2
5> WS = 1. %number of bytes to store weight
1
6> KeyLength = 5. %maximum depth of the trie
5
7> CFG = cfg:new(WS, KeyLength, Size, ID).
{cfg,1,5,2,trieID}
7> trie_sup:start_link(CFG).
{ok,<0.56.0>}
8> Weight = 1.
1
9> {Key1, Root1} = trie:put(Value, Root0, Weight, CFG).
{0,1}
10> {RootHash, Value, Weight, Proof1} = trie:get(Key1, Root1, CFG).
{<<102,98,103,55,233,120,36,6,83,156,252,246>>,
 5,1,
 [{<<185,26,241,1,92,76,72,163,23,23,230,168>>,
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
   <<0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,...>>}]}
11> Leaf1 = leaf:new(Key1, Weight, Value).
{leaf,0,1,5}
12> verify:proof(RootHash, Leaf1, Proof1, CFG).
true
13> trie:garbage([Root1], CFG).%This deletes everything that isn't a decendent of the stems in this list.
ok
14> trie:garbage_leaves([{leaf:path(Leaf1, KeyLength), Root1}], CFG).%This deletes everything that isn't necessary to prove the Leaf-Root pairs in the list
ok
```

here is an example of a project that uses this MerkleTrie: https://github.com/BumblebeeBat/PinkFairy