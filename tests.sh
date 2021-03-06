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
assert "./lispy '(quote a)'" "a" "quote symbol"
assert "./lispy '(quote (+ 1 2 3))'" "(+ 1 2 3)" "quote list"
assert "./lispy '(quote [1 a +])'" "[1 a +]" "quote vector"
assert "./lispy '(do 1 2 3)'" "3" "do syntax"
assert "./lispy '(if (< 1 2) 1 2)'" "1" "if consequent"
assert "./lispy '(if (> 1 2) 1 2)'" "2" "if alternate"
assert "./lispy '(letrec [a 1 b 2] (+ a b))'" "3" "non-rec letrec"
assert "./lispy '(letrec [fac (lambda [n a] (if (= n 1) a (fac (- n 1) (* a n))))] (fac 10 1))'" "3628800" "factorial letrec"
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

echo tailcall
assert "./lispy '(letrec [count (lambda [n] (if (= n 0) 0 (count (- n 1))))] (count 20000))'" "0" "cannot run out of stack with tail calls"
assert_end

echo env
assert_raises "./lispy '(do (letrec [a 1 b 2] (+ a b)) a)'" '134'
assert "./lispy '(letrec [f (lambda [a] (+ a 1))] (letrec [b 4] (f 1) b))'" '4'
assert "./lispy '(letrec [a 1] (+ (letrec [a 2] a) a))'" '3'
assert_end

echo define
assert "./lispy '(do (define four 4) (define three 3) (* three four))'" "12" "define vals"
assert "./lispy '(do (define add1 (lambda [a] (+ a 1))) (add1 6))'" "7" "define lambda"
assert "./lispy '(do (define six 6) (define seven (+ six 1)) (+ seven six))'" "13" "consecutive defines"
assert_end
