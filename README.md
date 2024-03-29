# Software Testing and Integration

## Group PT_MA2_3

### Members

- Carla Alvarado.
- Robert Diebels
- Gerrit Krijnen
- Alberto Martinez de Murga Ramirez

### How to use this repository

The ```master``` branch is the evaluation branch: it contains the final code to
be evaluated. The exercises will be made in different branches, one per exercise
at least, that will be merged based on a pull request. A reference of the branch
system can be found at [1].

The code for the exercises of each week are in separated folders. The build tool
used is ```cabal``` as it is suggested in [2]. In order to execute the code for
that week:
```
cabal repl week-n-ex-n
```
### Commit policy

Every commit should contain only one exercise or a partial implementation of one 
exercise. The idea is that doing this way it will be possible to cherry-pick
commits from different branches to create the final solution on the master
branch. This way we will avoid (or at least try to avoid) complex merges that 
can take long time to solve due to merge conflicts.

### Coding style

The coding guidelines and style used are the recommended in the official wiki[3]
and ```HLint```[4].

### TODO

- Discuss the use of ```HSpec```[5] instead of Testing for the tests.
- Discuss the use of ```stylish-haskell```[6] to prettify the code.
- Update the builds for the upcoming weeks.

### References

1.[A successful Git branching model](http://nvie.com/posts/a-successful-git-branching-model/)
2.[How to write a Haskell program](https://wiki.haskell.org/How_to_write_a_Haskell_program)
3.[Programming guidelines - haskell wiki](https://wiki.haskell.org/Programming_guidelines)
4.[hlint: Source code suggestions](https://hackage.haskell.org/package/hlint#readme)
5.[Hspec: A testing framework for Haskell](https://hspec.github.io/)
6.[Github:jaspervdj/stylish-haskell](https://github.com/jaspervdj/stylish-haskell)
