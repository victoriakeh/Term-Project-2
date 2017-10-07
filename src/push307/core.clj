(ns push307.core
  (:gen-class))

;;;;;;;;;;
;; Examples

; An example Push state
(def example-push-state
  {:exec '("hello"  integer_+ integer_-)
   :integer '(0 2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

; An example Push program
(def example-push-program
  '(3 5 integer_* "hello" 4 "world" integer_-))

; An example individual in the population
; Made of a map containing, at mimimum, a program, the errors for
; the program, and a total error
(def example-individual
  {:program '(3 5 integer_* "hello" 4 "world" integer_-)
   :errors [8 7 6 5 4 3 2 1 0 1]
   :total-error 37})

(def example-individual2
  {:program '(3 5 integer_* "hello" 4 "world" integer_-)
   :errors [8 7 6 5 4 3 2 1 0 1]
   :total-error 39})


;;;;;;;;;;
;; Instructions must all be either functions that take one Push
;; state and return another or constant literals.
(def instructions
  (list
   'in1
   'integer_+
   'integer_-
   'integer_*
   'integer_%
   0
   1
   ))


;;;;;;;;;;
;; Utilities

(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
   :input {}})

(defn push-to-stack
  "Pushes item onto stack in state, returning the resulting state."
  [state stack item]
  (let [newstack (conj (get state stack) item)]
  (assoc state stack newstack)))

(defn pop-stack
  "Removes top item of stack, returning the resulting state."
  [state stack]
  (assoc state stack (rest (get state stack))))

(defn empty-stack?
  "Returns true if the stack is empty in state."
  [state stack]
  (empty? (get state stack)))

(defn peek-stack
  "Returns top item on a stack. If stack is empty, returns :no-stack-item"
  [state stack]
  (if (= (empty-stack? state stack) true)
    :no-stack-item
    (first (get state  stack))))


(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map of the form {:state :args}, where
  :state is the new state with args popped, and :args is a list of args from
  the stacks. If there aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))

(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack in the resulting state."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))


;;;;;;;;;;
;; Instructions

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (assoc state :exec (conj (get state :exec) (get (get state :input) :in1))))

(defn integer_+
  "Adds the top two integers and leaves result on the integer stack.
  If integer stack has fewer than two elements, noops."
  [state]
  (make-push-instruction state +' [:integer :integer] :integer))

;;;; This is an example of what would be necessary to implement integer_+
;;;; without the useful helper function make-push-instruction.
;; (defn integer_+_without_helpers
;;   [state]
;;   (if (< (count (:integer state)) 2)
;;     state
;;     (let [arg1 (peek-stack state :integer)
;;           arg2 (peek-stack (pop-stack state :integer) :integer)
;;           popped-twice (pop-stack (pop-stack state :integer) :integer)]
;;       (push-to-stack popped-twice :integer (+' arg1 arg2)))))


(defn integer_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: the second integer on the stack should be subtracted from the top integer."
  [state]
  (make-push-instruction state -' [:integer :integer] :integer))
 

(defn integer_*
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  (make-push-instruction state *' [:integer :integer] :integer))

(defn integer_%
  "This instruction implements 'protected division'.
  In other words, it acts like integer division most of the time, but if the
  denominator is 0, it returns the numerator, to avoid divide-by-zero errors."
  [state]
  (let [numerator (second (get state :integer))
        denominator (first (get state :integer))]
    (if (= denominator 0)
      (pop-stack state :integer)
      (make-push-instruction state / [:integer :integer] :integer)))
  )


;;;;;;;;;;
;; Interpreter

(defn interpret-one-step
  "Helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack,
  or if the next element is a literal, pushes it onto the correct stack.
  Returns the new Push state."
  [push-state]
  (let [curr (eval (first (get push-state :exec)))]
    (println curr)
    (cond (integer? curr) (push-to-stack (pop-stack push-state :exec)
                                         :integer
                                         curr)
          (string? curr) (push-to-stack (pop-stack push-state :exec)
                                        :string
                                        curr)
          :else (curr (pop-stack push-state :exec)))))


             
(defn interpret-push-program
  "Runs the given program starting with the stacks in start-state. Continues
  until the exec stack is empty. Returns the state of the stacks after the
  program finishes executing."
  [program start-state]
  (loop [curr-state (assoc start-state :exec program)]
    (if (empty-stack? curr-state :exec)
      curr-state
      (recur (interpret-one-step curr-state)))))


;;;;;;;;;;
;; GP

(defn make-random-push-program
  "Creates and returns a new program. Takes a list of instructions and
  a maximum initial program size."
  [instructions max-initial-program-size]
  (let [newprogram '()
        program-size (rand-int (+ max-initial-program-size 1))]
    (loop [add_instructions program-size
           newprogram newprogram
           instructions instructions]
      (if (= add_instructions 0)
        newprogram
        (recur (- add_instructions 1)
               (conj newprogram (rand-nth instructions))
               instructions))))
  )

(defn tournament-selection
  "Selects an individual from the population using a tournament. Returned 
  individual will be a parent in the next generation. Can use a fixed
  tournament size."
  [population]
  (let [selected-individuals (into [] (take 6 (repeatedly #(rand-nth population))))]
    (apply min-key :total-error selected-individuals)
  ))

(defn crossover
  "Crosses over two programs (note: not individuals) using uniform crossover.
  Returns child program."
  [prog-a prog-b]
    ; define the first list as A and the second as B, and a new child
  (loop [A prog-a
         B prog-b
         child '()]

    ; if both lists are empty, then return, otherwise continue 
    (if (and (empty? A) (empty? B))

      ; if both are empty, filter out any extra instructions which
      ; will be nil because this only happens when one list is longer
      ; than the other and the shorter list's nonexistent instruction
      ; is chosen (nil)
      (filter #(not= % nil) (reverse child))

      ; when we call our function recursively, continue with all but
      ; the current instruction from A and B
      (recur (rest A)
             (rest B)

             ; there is a 50-50 shot where either we add the instruct-
             ; ion from A or B
             (if (= (rand-int 2) 0)
               (conj child (first A))
               (conj child (first B)))))))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the program) with some probability. Returns child program."
  [prog]
    ; define program, the first instruction in the program as curr
  ; the instructions list, and the new program
  (loop [prog prog
         curr (first prog)
         new-prog '()]

    ; if the old program is empty we are done
    (if (empty? prog)

      ; then, if by a 5% chance we should add one to the end of the
      ; new program we do that otherwise we return the new program
      ; note; we reverse it because we use conj so it's backwards
      (if (= (rand-int 20) 0)
        (reverse (conj new-prog (rand-nth instructions)))
        (reverse new-prog))

      ; call our function with the current instruction removed, and a
      ; new current defined, and the instructions list
      (recur (rest prog)
             (first (rest prog))
             ; by our 5% chance we either add an instructio before
             ; and then the current instructio or just the current one
             (if (= (rand-int 20) 0)
               (conj (conj new-prog
                           (rand-nth instructions)) curr)
               (conj new-prog curr))))))

(defn uniform-deletion
  "Randomly deletes instructions from program at some rate. Returns child program."
  [prog]
    ; each element of the list has a 95% chance of appearing in the
  ; new list (same as 5% of not appearing)
  (random-sample 0.95 prog))

(defn select-and-vary
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program). Chooses which genetic operator
  to use probabilistically. Gives 50% chance to crossover,
  25% to uniform-addition, and 25% to uniform-deletion."
  [population]
  (let [prob-genetic-op (+ (rand 100) 1)]
    (if (<= prob-genetic-op 50)
      (make-individual-from-program (crossover (get :program (tournament-selection population)) (get :program (tournament-selection population))))
      (if (and (> prob-genetic-op 50) (<= prob-genetic-op 75))
        (make-individual-from-program (uniform-addition (get :program (tournament-selection population))))
        (make-individual-from-program (uniform-deletion (get :program (tournament-selection population)))))))
  )

(defn report
  "Reports information on the population each generation. Should look something
  like the following (should contain all of this info; format however you think
  looks best; feel free to include other info).

-------------------------------------------------------
               Report for Generation 3
-------------------------------------------------------
Best program: (in1 integer_% integer_* integer_- 0 1 in1 1 integer_* 0 integer_* 1 in1 integer_* integer_- in1 integer_% integer_% 0 integer_+ in1 integer_* integer_- in1 in1 integer_* integer_+ integer_* in1 integer_- integer_* 1 integer_%)
Best program size: 33
Best total error: 727
Best errors: (117 96 77 60 45 32 21 12 5 0 3 4 3 0 5 12 21 32 45 60 77)
  "
  [population generation]
  (println "-------------------------------------------------------")
  (println "               Report for Generation" generation)
  (println "-------------------------------------------------------")
  (let [best-program (apply min-key :total-error (into [] population))]
    (println "Best program:" (get best-program :program))
    (println "Best program size:" (count (get best-program :program)))
    (println "Best total error:" (get best-program :total-error))
    (println "Best errors:" (get best-program :errors)))
  )

(defn make-individual-from-program
  [program]
  (let [individual {:program program
                    :errors []
                    :total-error 0}]
    (regression-error-function individual)))

(defn find-successful-program
  [population]
  (loop [population population]
    (if (= (get :total-error (first (population))) 0)
      :SUCCESS
      (if (= (rest population) nil)
        :CONTINUE-SEARCHING))
    (recur (rest population))))

(defn push-gp
  "Main GP loop. Initializes the population, and then repeatedly
  generates and evaluates new populations. Stops if it finds an
  individual with 0 error (and should return :SUCCESS, or if it
  exceeds the maximum generations (and should return nil). Should print
  report each generation.
  --
  The only argument should be a map containing the core parameters to
  push-gp. The format given below will decompose this map into individual
  arguments. These arguments should include:
   - population-size
   - max-generations
   - error-function
   - instructions (a list of instructions)
   - max-initial-program-size (max size of randomly generated programs)"
  [{:keys [population-size max-generations error-function instructions max-initial-program-size]}]
  ;(loop [pop-size population-size
         ;generations-left max-generations]
  (let [original-population (take population-size (repeatedly #(make-individual-from-program (make-random-push-program instructions max-initial-program-size))))]
    (loop [curr-population original-population
           generations-left max-generations]
        (cond (= (find-successful-program curr-population) :SUCCESS) :SUCCESS
              (= generations-left 0) nil
              :else (recur (take population-size (repeatedly #(select-and-vary curr-population)))
                           (- generations-left 1))))))

;;;;;;;;;;
;; The functions below are specific to a particular problem.
;; A different problem would require replacing these functions.
;; Problem: f(x) = x^3 + x + 3

(defn target-function
  "Target function: f(x) = x^3 + x + 3
  Should literally compute this mathematical function."
  [x]
  :STUB
  )

(defn regression-error-function
  "Takes an individual and evaluates it on some test cases. For each test case,
  runs program with the input set to :in1 in the :input map part of the Push state.
  Then, the output is the integer on top of the integer stack in the Push state
  returned by the interpreter. Computes each error by comparing output of
  the program to the correct output.
  Returns the individual with :errors set to the list of errors on each case,
  and :total-error set to the sum of the errors.
  Note: You must consider what to do if the program doesn't leave anything
  on the integer stack."
  [individual]
  :STUB
  )


;;;;;;;;;;
;; The main function. Uses some problem-specific functions.

(defn -main
  "Runs push-gp, giving it a map of arguments."
  [& args]
  (push-gp {:instructions instructions
            :error-function regression-error-function
            :max-generations 500
            :population-size 200
            :max-initial-program-size 50}))
