DSL for generating EBNF-based lexers/parsers.

```
rule a: b | c
rule b: "\ident"
rule c: "\d+"
```

```
rule a: b | c b | d
rule b: "\ident"
rule c: "var"
rule d: "\d+"
```