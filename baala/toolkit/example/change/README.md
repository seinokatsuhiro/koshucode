
Calculate changeset
[`CHANGE.k`](https://github.com/seinokatsuhiro/koshucode/blob/master/baala/toolkit/example/change/C.k)
= [`ALTERED.k`](https://github.com/seinokatsuhiro/koshucode/blob/master/baala/toolkit/example/change/A.k)
- [`BASE.k`](https://github.com/seinokatsuhiro/koshucode/blob/master/baala/toolkit/example/change/B.k).

``` sh
koshu-change ALTERED.k --minus BASE.k > CHANGE.k
```

Update dataset `BASE.k` by changeset `CHANGE.k`.
We would compare it to a formula `BASE.k` + `CHANGE.k` = `ALTERED.k`.

``` sh
koshu-change BASE.k --update CHANGE.k
```

This command outputs the following dataset.

``` text
** -*- koshu -*-
**  
**  DATASETS
**    Updating dataset B by C, altered dataset A is obtained.
**  
**    B (base)    : BASE.k
**    C (change)  : CHANGE.k
**    A (altered) : B + C
**

|-- BAR /y '1' /z '2'
|-- BAR /y '2' /z '2'
|-- FOO /x '0' /y '0'
|-- FOO /x '1' /y '0'
|-- FOO /x '1' /y '1'
```

