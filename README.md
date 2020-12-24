# Recursive-Descent-Parser
A recursive descent parser is a kind of top-down parser built from a set of mutually recursive procedures where each such procedure implements one of the nonterminals of the grammar.

Thus the structure of the resulting program closely mirrors that of the grammar it recognizes. A predictive parser is a recursive descent parser that does not require backtracking. Predictive parsing is possible only for the class of LL grammars, which are the context-free grammars for which there exists some positive integer k that allows a recursive descent parser to decide which production to use by examining only the next k tokens of input.

# Given Grammar
```
S -> eaf | eUT
T -> e
U -> UcS | ae
```
# Transforming to LL(1)
```
S -> eaf | eUT
T -> e
U -> aeV
V -> cSV | ε
```
View the [Jupyter Notebook](https://github.com/SimronJ/Recursive-Descent-Parser/blob/master/project.ipynb)
