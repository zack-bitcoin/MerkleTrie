Merkle Trie

the goal of this project is to have a merkel tree on the hard drive for a blockchain.
It remembers fixed size pieces of data. You can choose the size of data in the .app file when you use rebar to include this library.

Install process:

First you need erlang installed. Then to install this software do: ```sh install.sh```

Now to run the software simply: ```sh start.sh```


Example usage:

```
1> Key = "key".
"key"
2> Value  = <<27,27>>.
<<"\e\e">>
3> Root0 = 0.
0
4> Root1 = trie:put(Key, Value, Root0).
1
5> {A, Value, B} = trie:get(Key, Root1).
{<<74,47,175,57,180,32,222,15,65,16,141,115,251,198,130,
   153,253,114,2,194,20,33,100,92,15,46,157,120,...>>,
 <<"\e\e">>,
 [{<<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...>>,
   <<131,151,102,148,90,150,233,191,28,30,181,49,163,97,81,
     207,61,34,201,131,76,...>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,0,0,...>>,
   <<0,0,0,0,0,0,0,0,0,...>>}]}
6> verify:proof(A, Key, Value, B).
true
```