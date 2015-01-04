. ./assert.sh

echo values
assert "./lispy 1" '1' "Number evals to number"
assert "./lispy []" '[]' "Empty vector"
assert "./lispy '[1 2 3]'" '[1 2 3]' "Vector"
assert "./lispy '(list)'" '()' "Empty list"
assert "./lispy '(list 1 2 3)'" "(1 2 3)" "list function"
assert "./lispy '(lambda [a] a)'" "<function>" "function value"
assert "./lispy '\"foo\"'" '"foo"' "literal string"
assert_end

echo arith
assert "./lispy '(+ 1 4)'" "5"
assert "./lispy '(- 4 1)'" "3"
assert "./lispy '(* 6 4)'" "24"
assert "./lispy '(/ 10 3)'" "3"
assert "./lispy '(< 0 1 2 3)'" "true"
assert "./lispy '(< 0 1 2 1)'" "false"
assert "./lispy '(> 0)'" "true"
assert "./lispy '(> 5 4 3)'" "true"
assert "./lispy '(>= 5 4 4 1)'" "true"
assert_end

echo syntax
assert "./lispy '(do 1 2 3)'" "3" "do syntax"
assert "./lispy '(if (< 1 2) 1 2)'" "1" "if consequent"
assert "./lispy '(if (> 1 2) 1 2)'" "2" "if alternate"
assert_end

echo builtins
assert "./lispy '(+ 1)'" "1"
assert "./lispy '(+ 1 6)'" "7"
assert "./lispy '(+ 4 5 6)'" "15"
assert_end

echo functions
assert "./lispy '((lambda [a b] [a b (+ a b)]) 4 5)'" "[4 5 9]" "apply function"
assert "./lispy '((lambda [a] 1 (+ a 5) a) 4)'" "4" "lambda body"
assert_end
