import bnimf

rule rule_expr: rule_name | rule_number

rule a: b | c d | e
rule b: r"\ident"
rule c: "fn"

check_rules()
