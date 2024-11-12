# obliqueBART

## Motivation
We introduced oblique BART, which extends the expressivity of the BART model by building regression trees that partition the predictor space based on random hyperplanes. In contrast to frequentist oblique tree ensembles, oblique BART circumvents the difficult problem of locating the optimal oblique split without relying on optimization heuristics and has increased flexibility.


The **obliqueBART** package overcomes this limitation by utilizing a new prior for decision trees.
Like other implementations, the drawing a decision tree from the new prior is accomplished by first simulating a branching process and then randomly drawing decision rules for each non-terminal (i.e. non-leaf) node of the tree. 
We specify the oblique BART decision rule prior implicitly, by describing how to draw a rule at a non-terminal node, $\texttt{nx}$, in a tree.
Drawing a continuous decision rule involves (i) drawing a random vector $\phi;$ (ii) computing the interval of valid values of $\phi^{\top}x_{cont}$ at $\texttt{nx}$; and (iii) drawing the cutpoint $c$ uniformly from that interval.

We specify a hierarchical spike-and-slab prior for $\phi,$ which encourages sparsity and also allows the number of non-zero entries of $\phi$ to vary adaptively with the data.
Formally, we introduce a parameter $\theta \in [0,1]$ that controls the overall sparsity of $\phi.$

Conditionally on $\theta,$ we draw $p$ binary indicators $\gamma_i | \theta \overset{i.i.d.}{\sim}$ Bernoulli($\theta$); (ii) for each $j = 1, \ldots, p_{cont},$ we draw $\phi_{j} \sim$ Normal $(0,1)$ if $\gamma_{j} = 1$ and $\phi_{j} = 0$ otherwise; and (iii) finally, we re-scale $\phi$ to have unit norm. 
By specifying a further prior on $\theta,$ we allow oblique BART to learn an appropriate level of sparsity of $\phi$ from the data. 
For simplicity, we specify a conjugate ${\textrm{Beta}}({a_{\theta}},{b_{\theta}})$ prior with fixed $a_{\theta}, b_{\theta} > 0.$


Once we draw $\phi,$ we draw $c$ uniformly from the range of valid values of $\phi^{\top}x_{cont}$ available at $\texttt{nx}.$
This range is determined by the continuous decision rules used at the ancestors of $\texttt{nx}$ in the tree and can be computed by solving two linear programs maximizing and minimizing $\phi^{\top}x_{cont}$ over the linear polytope corresponding to $\texttt{nx}.$


## Installation and basic usage

The package source files are contained in the sub-directory obliqueBART.
To install, you can either download that directory and build and install the package from the command line.
Alternatively, you can install using `devtools::install_github`:
```
devtools::install_github(repo = "paulhnguyen/obliqueBART", subdir = "obliqueBART")
```

The `examples` subdirectory contains some case studies showing how to use obliqueBART with regression data (abalone), classification data (banknote), toy examples, and the benchmark datasets. and comparing the run-time to the **BART** package, as well as other tree ensembles from **randomForest**, **ranger**, and **XGboost**. 


