---
title: "Timing of nimVector versus declare and setSize"
---

Here we'll test the timing of nimVector() versus declare() and setSize()


```r
library(nimble)
```

```
## 
## Attaching package: 'nimble'
```

```
## The following object is masked from 'package:stats':
## 
##     simulate
```

```r
library(methods)

N <- 5E6
```



rfun1 uses the current nimVector() nimbleFunction.



```r
rfun1 <- nimbleFunction(
    run = function() {
        vec <- nimVector(value = 0, length = 100)
    }
)

cfun1 <- compileNimble(rfun1)


t1 <- system.time(for(i in 1:N) { cfun1() })

t1
```

```
##    user  system elapsed 
##  13.651   1.335  15.067
```

rfun2 uses declare() and triggers a setSize() call.

Note both of these include the looping and setting initial value.


```r
rfun2 <- nimbleFunction(
    run = function() {
        declare(vec, double(1, 100))
        for(i in 1:100)   vec[i] <- 0
    }
)

cfun2 <- compileNimble(rfun2)

t2 <- system.time(for(i in 1:N) { cfun2() })

t2
```

```
##    user  system elapsed 
##  11.770   0.850  12.695
```

Fractionally, how much slower the current nimArray() nimbleFunction is, compared
to plain old declare() and setSize() -- which you believe are
inefficient themselves.



```r
(t1[3] - t2[3]) / t2[3]
```

```
##   elapsed 
## 0.1868452
```

15 - 20% additional hit that we're currently taking.

