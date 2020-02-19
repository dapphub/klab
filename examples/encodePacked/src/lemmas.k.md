```k
rule chop( A <<Int 96 ) => A <<Int 96
  requires #rangeAddress(A)

rule #padToWidth(32, #asByteStack( A <<Int 96 ))
  => #asByteStackInWidth(A <<Int 96, 32)

/* rule ( nthbyteof(( A <<Int 96), 0, 32) */
/*      : nthbyteof(( A <<Int 96), 1, 32) */
/*      : nthbyteof(( A <<Int 96), 2, 32) */
/*      : nthbyteof(( A <<Int 96), 3, 32) */
/*      : nthbyteof(( A <<Int 96), 4, 32) */
/*      : nthbyteof(( A <<Int 96), 5, 32) */
/*      : nthbyteof(( A <<Int 96), 6, 32) */
/*      : nthbyteof(( A <<Int 96), 7, 32) */
/*      : nthbyteof(( A <<Int 96), 8, 32) */
/*      : nthbyteof(( A <<Int 96), 9, 32) */
/*      : nthbyteof(( A <<Int 96), 10, 32) */
/*      : nthbyteof(( A <<Int 96), 11, 32) */
/*      : nthbyteof(( A <<Int 96), 12, 32) */
/*      : nthbyteof(( A <<Int 96), 13, 32) */
/*      : nthbyteof(( A <<Int 96), 14, 32) */
/*      : nthbyteof(( A <<Int 96), 15, 32) */
/*      : nthbyteof(( A <<Int 96), 16, 32) */
/*      : nthbyteof(( A <<Int 96), 17, 32) */
/*      : nthbyteof(( B <<Int 96), 18, 32) */
/*      : nthbyteof(( B <<Int 96), 19, 32) */
/*      : nthbyteof(( B <<Int 96), 0, 32) */
/*      : nthbyteof(( B <<Int 96), 1, 32) */
/*      : nthbyteof(( B <<Int 96), 2, 32) */
/*      : nthbyteof(( B <<Int 96), 3, 32) */
/*      : nthbyteof(( B <<Int 96), 4, 32) */
/*      : nthbyteof(( B <<Int 96), 5, 32) */
/*      : nthbyteof(( B <<Int 96), 6, 32) */
/*      : nthbyteof(( B <<Int 96), 7, 32) */
/*      : nthbyteof(( B <<Int 96), 8, 32) */
/*      : nthbyteof(( B <<Int 96), 9, 32) */
/*      : nthbyteof(( B <<Int 96), 11, 32) */
/*      : nthbyteof(( B <<Int 96), 12, 32) */
/*      : nthbyteof(( B <<Int 96), 13, 32) */
/*      : nthbyteof(( B <<Int 96), 14, 32) */
/*      : nthbyteof(( B <<Int 96), 15, 32) */
/*      : nthbyteof(( B <<Int 96), 16, 32) */
/*      : nthbyteof(( B <<Int 96), 17, 32) */
/*      : nthbyteof(( B <<Int 96), 18, 32) */
/*      : nthbyteof(( B <<Int 96), 19, 32) */
/*      : .WordStack */
/*      ) => #asByteStack(A <<Int 96) ++ #asByteStack(B <<Int 96) */
```
