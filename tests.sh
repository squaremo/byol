. ./assert.sh

assert "./lispy 1" '1' "Number evals to number"
assert "./lispy []" '[]' "Empty vector"
assert "./lispy '(list)'" '()' "Empty list"
assert "./lispy '(list 1 2 3)'" "(1 2 3)" "list function"
assert "./lispy '(lambda [a] a)'" "<function>" "function value"
assert "./lispy '((lambda [a b] [a b (+ a b)]) 4 5)'" "[4 5 9]" "apply function"

assert_end
