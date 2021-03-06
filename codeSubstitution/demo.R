##---
##publish: true
##---


##### Code objects in R


##```{r, echo=FALSE, message=FALSE}
library(nimble)
##```

##First, it's important to understand that code can be treated as an object in R.  code objects can be created using `quote()`, which is in fact *exactly* what `nimbleCode()` does.  Nothing more.  Code objects can subsequently be indexed as nested list objects.

##```{r}
x <- quote(a + b)
x
x[[1]]
x[[2]]
x[[3]]

y <- quote(functionCall(a = arg1, b = f==g, c = TRUE))
y
names(y)   ## this is only useful for named function arguments, and named list elements
y[[1]]
y[[2]]
y[[3]]
#### Notice the 'reverse polish notation' style of the ordering, where the call (e.g., +) always comes first
y[[3]][[1]]
y[[3]][[2]]
y[[3]][[3]]
y[[4]]

#### and most importantly, blocks of code, too:
z <- nimbleCode({    ## this is really just quote, under the hood
    a <- 1
    b ~ dnorm(0, 1)
})
z[[1]]    ## `{` is the function call which begins a block of code -- very important
z[[2]]
z[[2]][[1]]
z[[2]][[2]]
z[[2]][[3]]
z[[3]]
z[[3]][[1]]
z[[3]][[2]]
z[[3]][[3]]
z[[3]][[3]][[1]]
#### etc....
##```


##### substitute()


## Once we're comfortable manipulating code objects, we can use `substitute()`.  That's the real workhorse here.  It can do several different things, but when provided with the syntax `substitute(EXPR, LIST)`, it performs named substitution of names appearing in `EXPR` with the corresponding (named) list elements appearing in `LIST`.  This will be clear by example.  Note, `substitute` automatically quotes its first argument, so no `quote()` is necessary.

##```{r}
substitute(a + b, list(a = 3, b = 4))
substitute(fun(expr), list(fun = quote(mean), expr = quote(1:10)))

#### generally good practice to put the names we're going to substitute in ALL CAPS, if we know which ones they are.  Knowning that, we could do something like:

substitute({
    a ~ PRIOR1
    b ~ PRIOR2
    c <- a + b
},
           list(PRIOR1 = quote(dunif(0, 1)),
                PRIOR2 = quote(dnorm(0, 1)))
           )

##```


##### eval(substitute(substitute()))


## Time to level up.

## `eval(substitute(substitute(...)))` is a VERY powerful combination.  The first appearance of `substitute` gets evaluated first (NOT because of the `eval()`, but because R always evaluates the arguments to functions, e.g., `fun(a)` will evaluate the variable `a` and pass that value to `fun()`).

## The evaluation of this first `substitute` does a substitution of the (automatically quoted!) next `substitute()` expression.  So it can replace variables within that expression, most specifically, can replace a name with another block of code.

## The result of that first evaluation of `substitute` is now another expression which looks like `substiute(...)`, which is an R code expression (like we worked with above).  Now the magic happens, where the `eval()` call actually *evaluates* that `substitute(...)` expression, and does yet another substitution into that block of code.  Let's try an example or two.

##```{r}
code <- nimbleCode({    ## really, just quote()
    lambda ~ dunif(0, MAX)
    y ~ dpois(lambda)
})

code

eval(substitute(substitute(CODE,
                           list(MAX = 3)),
                list(CODE = code)
                )
     )

#### or better yet:

setMax <- function(max) eval(substitute(substitute(CODE, list(MAX=max)), list(CODE=code)))

setMax(3)

lapply(as.numeric(1:4), setMax)

#### we could also substitute in expressions for priors, as you asked about:

code <- nimbleCode({   ## really, just quote()
    mu ~ PRIOR1
    sd ~ PRIOR2
    y ~ dnorm(mu, sd = sd)
})

eval(substitute(substitute(CODE,
                           list(PRIOR1 = quote(dnorm(0, 0.001)),
                                PRIOR2 = quote(dunif(0, 10)))),
                list(CODE = code))
     )

##```

## From here, anything is possible.

## Make sense?  Did this answer your question?

## If not, and you want to do something more specific, then definitely let me know.

## Cheers!

