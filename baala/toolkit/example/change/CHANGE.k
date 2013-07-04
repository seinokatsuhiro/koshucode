** -*- koshu -*-
**  
**  DATASETS
**    There are changes C when altering dataset B into A.
**  
**    B (base)    : ALTERED.k
**    A (altered) : BASE.k
**    C (change)  : A - B
**  
**  UPDATE
**    Dataset A is obtained by updating B by C.
**    Please execute: koshu-change B -u C
**

|-X BAZ /y '3' /z '3'
|-X FOO /x '0' /y '1'
|-- FOO /x '1' /y '0'
