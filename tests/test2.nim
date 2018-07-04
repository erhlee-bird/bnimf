import bnimf

# Right regular grammar
rule expr: '1'
rule expr: '2'
rule expr: '3'
rule expr: '1' op_expr
rule expr: '2' op_expr
rule expr: '3' op_expr
rule op_expr: '+' expr
rule op_expr: '*' expr

# Pass
# 1
# 3+2
# 1+2+3
# 3*2+1

# Fail
# 4
# 1+
# *
# *3
# 1+3+5
# 2*z

check_rules()
