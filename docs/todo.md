The keys we want to store are integers increasing from 0. So instead of taking the hash of the key, we should just use the key as a binary.

We should do writes and reads in batches.
Writing crawls up the tree from the leaves, and calculates the new root hash as a last step. So the number of batch-writes is probably the same as the depth of the longest branch.
reading crawls down the tree from the top.
Each branch should be a different process fanning outward to all the leaves. Then the leaves should be collected into a hash-table in ram for easy access.


