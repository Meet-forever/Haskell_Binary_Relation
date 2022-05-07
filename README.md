# Haskell Binary Relation

Project idea and instruction taken from: [PLC-Project2](https://tinman.cs.gsu.edu/~raj/4330/sp22/project2/)

<blockquote>"If there are two sets, and we want to check if there is any connection between the two, we use relations." -- Meet Patel </blockquote> 

### Available Relations
```
- Empty
- Universal
- Reflexive
- Symmetric
- Antisymmetric
- Transitive
- Equivalence 
```

<blockquote>"The closure of a subset is the result of a closure operator applied to the subset. The closure of a subset under some operations is the smallest subset that is closed under these operations. It is often called the span (for example linear span) or the generated set." -- Wikipedia</blockquote>

### Available Closures
```
- Reflexive Closure
- Symmetric Closure
- Transitive Closure
```

### Sample Run:
```bash
*Main> r1 = add (1,2) (add (2,3) (add (3,4) emptyBinaryRelation)) 
*Main> r2 = add (1,2) (add (3, 4) (add (2,3) emptyBinaryRelation))
*Main> reflexive r1
False
*Main> symmetric r2
False
*Main> r2' = symmetricClosure r2
*Main> symmetric r2'
True
*Main> toString r2'
"[(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]"
```
