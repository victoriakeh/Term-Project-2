;; Oliver Keh
;; Victoria Slack
;; Term Project Part 2
;; CPSCI 307 

(ns push307.core
  (:gen-class))

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
   :float '()
   :input {}})

(defn push-to-stack
  "Takes a push state, stack name, and item then pushes item onto stack
  in state, returning the resulting state. Attaches the new item to front
  of newstack and associates the new stack with the push state."
  [state stack item]
  (let [newstack (conj (get state stack) item)]
    (assoc state stack newstack)))

(defn pop-stack
  "Takes a push state and stack name and removes top item of stack,
  returning the resulting state. Associates the rest of the list,
  which excludes the first item in the list (top of the stack) to pop
  the stack."
  [state stack]
  (assoc state stack (rest (get state stack))))

(defn empty-stack?
  "Takes a push state and stack name returns true if the stack is empty in state.
  Checks to see if the stack is empty and returns that value."
  [state stack]
  (empty? (get state stack)))

(defn peek-stack
  "Takes a push state and stack name and returns top item on a stack.
  If stack is empty, returns :no-stack-item and otherwise, gets the first
  item from the list (top of the stack). "
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
  "Takes a push state. Pushes the input labeled :in1 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map.
  Returns the push state that results from pushing the value at :in1 from
  the input stack to the top of the exec stack. "
  [state]
  (push-to-stack state :exec (get (get state :input) :in1)))

(defn integer_+
  "Takes a push state. Adds the top two integers and leaves result on the integer stack.
  If integer stack has fewer than two elements, noops. Uses make-push-instruction to
  define addition."
  [state]
  (make-push-instruction state +' [:integer :integer] :integer))

(defn integer_-
  "Takes a push state. Subtracts the top two integers and leaves result on the integer stack.
  Uses make-push-instruction to define subtraction where the second integer on the stack
  is subtracted from the top integer."
  [state]
  (make-push-instruction state -' [:integer :integer] :integer))
 

(defn integer_*
  "Takes a push state. Multiplies the top two integers and leaves result on the integer stack.
  Uses make-push-instruction to define multiplication."
  [state]
  (make-push-instruction state *' [:integer :integer] :integer))

(defn integer_%
  "Takes a push state. This instruction implements 'protected division'.
  In other words, it acts like integer division most of the time, but if the
  denominator is 0, it returns the numerator, to avoid divide-by-zero errors.
  Otherwise, it uses make-push-instruction to define division."
  [state]
  (let [numerator (second (get state :integer))
        denominator (first (get state :integer))]
    (if (= denominator 0)
      (pop-stack state :integer)
      (make-push-instruction state / [:integer :integer] :integer))))


;;;;;;;;;;
;; Interpreter

(defn interpret-one-step
  "Helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack
  by evaluating the first item on the exec stack and places the result on the
  appropriate stack, but if the next element is an integer, pushes it onto the
  integer stack correct stack and if it is a string, pushes it to the string stack.
  Also pops that element off the exec stack returns the new Push state."
  [push-state]
  (let [curr (eval (first (get push-state :exec)))]
    (cond (float? curr) (push-to-stack (pop-stack push-state :exec)
                                       :float
                                       curr)
          (integer? curr) (push-to-stack (pop-stack push-state :exec)
                                         :integer
                                         curr)
          (string? curr) (push-to-stack (pop-stack push-state :exec)
                                        :string
                                        curr)
          :else (curr (pop-stack push-state :exec)))))


             
(defn interpret-push-program
  "Takes a program and a start state. Runs the given program starting with the stacks
  in start-state (empty push state with appropriate input). Associates the program with the
  exec to start and evaluates from there. Continues until the exec stack is empty.
  Returns the state of the stacks after the program finishes executing, otherwise it interprets
  the next step of the program."
  [program start-state]
  (loop [curr-state (assoc start-state :exec program)]
    (if (empty-stack? curr-state :exec)
      curr-state
      (recur (interpret-one-step curr-state)))))


;;;;;;;;;;
;; GP

(defn make-random-push-program
  "Takes instruction set and a maximum initial progam size. Creates and
  returns a new program. Picks the size of the program randomly between
  1 and the max size. If you can't add any more instructions, it returns
  the new program and otherwise decrements the number of times we can add a new
  instruction and adds a new instruction to the progam and loop again."
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
               instructions)))))

(defn tournament-selection
  "Takes a population. Selects an individual from the population using a tournament.
  Returned individual will be a parent in the next generation. Can use a fixed
  tournament size. Takes 6 random programs from the population and applies a
  function to find the minimum total error and returns that individual."
  [population]
  (let [selected-individuals (into [] (take 6 (repeatedly #(rand-nth population))))]
    (apply min-key :total-error selected-individuals)))

(defn crossover
  "Takes to progarms. Crosses over two programs (not individuals) using uniform crossover.
  Returns child program. Checks to see if both parent progams are empty and if so filters out
  any instructions that are nil (which would happen if one program becomes empty before the other,
  where in that case we would still have a 50% chance of including each instruction from the rest of
  the other program) and then reverses the resulting program so it is returned in the
  correct order (not backwards). Otherwise, continues to build the child program through the 50%
  chance of the instruction being taken from parent A or parent B."
  [prog-a prog-b]
  (loop [A prog-a
         B prog-b
         child '()]
    (if (and (empty? A) (empty? B))
      (filter #(not= % nil) (reverse child))
      (recur (rest A)
             (rest B)
             (if (= (rand-int 2) 0)
               (conj child (first A))
               (conj child (first B)))))))

(defn uniform-addition
  "Takes a program. Randomly adds new instructions before every instruction (and at the end of
  the program) with a 5% probability of doing so. Loops through the parent program and
  if the parent program is empty, and by the 5% chance we add an instruction, we add a random
  instruction to the end of the child and return reversed (to the correct order) child program.
  Otherwise, if no instruction is added to the end, we reverse and return. If it is not empty, we
  check by the 5% chance to see if we add a random instruction in addition to instruction from the
  parent that we add, otherwise we just add that parent instruction and move on to the next one."
  [prog]
  (loop [prog prog
         curr (first prog)
         new-prog '()]
    (if (empty? prog)
      (if (= (rand-int 20) 0)
        (reverse (conj new-prog (rand-nth instructions)))
        (reverse new-prog))
      (recur (rest prog)
             (first (rest prog))
             (if (= (rand-int 20) 0)
               (conj (conj new-prog
                           (rand-nth instructions)) curr)
               (conj new-prog curr))))))

(defn uniform-deletion
  "Takes a progam. Randomly deletes instructions from program at a 5% rate.
  This means that there is a 95% chance the instruction will stay.
  Returns child program."
  [prog]
  (random-sample 0.95 prog))


(defn absolute-value
  "Takes a number and checks to see if it is negative (below 0), and if so,
  multiplies by -1 to make it positive, so the function always returns a positive
  number (the absolute value of the number)."
  [number]
  (if (< number 0)
    (* -1 number)
    number))

(defn get-error
  "Takes a program and an input value for the function. Computes the value we
  want from the target function as well as the value we get from the program
  we are evaluating by getting the top value from the integer stack of the
  end state. Then, if the program returned nothing, we assign it an error value
  of 10000, otherwise its error value is the absolute value of the value we want minus
  the value we got and we return that value."
  [program input]
  (let [target-value (target-function input)
        program-value (first (get (interpret-push-program program
                                             (assoc empty-push-state
                                                    :input {:in1 input}))
                                  :integer))]
    (if (nil? program-value)
      10000
      (absolute-value (- target-value  program-value)))))      

(defn regression-error-function
  "Takes an individual and evaluates it on some test cases. For each test case,
  Checks to see if we have done all 21 test cases [-10, 10], an is so returns the
  individual with errors set to the list of error values, and total-error set to
  the result when addition is applied to the error value list. Otherwise, we test
  the program of the individual on the input and add the error to the list of errors
  and move on to the next test case by adding 1 to the current input."
  [individual]
  (loop [curr-input -10
         errors (get individual :errors)]
    (if (> curr-input 10)
      {:program (get individual :program)
       :errors errors
       :total-error (apply + errors)}
      (recur (+ curr-input 1)
             (conj errors (get-error (get individual :program)
                                     curr-input))))))

(defn make-individual-from-program
  "Takes a program. Returns an individual with the program set to :program, and the errors
  vector with the associated error values that we get from our regression-error-function as well
  as the total-error which we also get from the function and return the resulting individual."
  [program]
  (let [individual {:program program
                    :errors []
                    :total-error 0}]
    (regression-error-function individual)))

(defn select-and-vary
  "Takes a population. Selects parent(s) from population and varies them, returning
  a child individual. Assign a random probabilty and if by a 50% chance crossover if chosen,
  choose two parents using tournament selection and return the resulting individual. If by a
  25% chance we get  uniform-addition, do that and again 25% to uniform-deletion. Each returning
  the resulting individual if chosen."
  [population]
  (let [prob-genetic-op (+ (rand 100) 1)]
    (cond (<= prob-genetic-op 50)
          (make-individual-from-program (crossover (get (tournament-selection population) :program)
                                                   (get (tournament-selection population) :program)))
          (<= prob-genetic-op 75)
          (make-individual-from-program (uniform-addition (get (tournament-selection population)
                                                               :program)))
          :else
          (make-individual-from-program (uniform-deletion (get (tournament-selection population)
                                                               :program))))))

(defn report
  "Takes the population and the generation number. Reports information on the population
  each generation. Includes the Generation number, best performing program, its size, total
  error, and all errors for each test case. Gets the best program by applying the minimum function
  to the list of total errors from the population and getting the resulting associated program. Then
  prints the program itself, and counts the size of that program, and prints the total error and errors."
  [population generation]
  (println "-------------------------------------------------------")
  (println "               Report for Generation" generation)
  (println "-------------------------------------------------------")
  (let [best-program (apply min-key :total-error (into [] population))]
    (println "Best program:" (get best-program :program))
    (println "Best program size:" (count (get best-program :program)))
    (println "Best total error:" (get best-program :total-error))
    (println "Best errors:" (get best-program :errors))))

(defn find-successful-program
  "Takes a population. Applies the minimum function to the population's total-error to see if one
  produces a total-error of 0, meaning it has found a solution. If it does it returns :SUCCESS."
  [population]
  (if (= (get (apply min-key :total-error population) :total-error) 0)
    :SUCCESS))

(defn push-gp
  "Main GP loop. Takes a map with the following values:
     - populatioqn-size
     - max-generations
     - error-function
     - instructions (a list of instructions)
     - max-initial-program-size (max size of randomly generated programs).
  Creates the original population by taking however many the population size is of the result of
  the make-random-push-program is and makes that into an individual. From the original population,
  we report it's best program and for each following generation we do this too. Then, if we have
  found a program that solves the problem, we return :SUCCESS, and if we have reached the maximum
  number of generations with no solution we return nil. Otherwise, we create a new population of
  population size from repeatedly getting the result from select-and-vary which mutates the current
  popualtion to create the next generation. Then add one to the generation count and loop again."
  [{:keys [population-size max-generations error-function instructions max-initial-program-size]}]
  (let [original-population (take population-size
                                  (repeatedly #(make-individual-from-program (make-random-push-program
                                                                              instructions
                                                                              max-initial-program-size))))]
    (loop [curr-population original-population
           curr-generation 0]
      (report curr-population curr-generation)
      (cond (= (find-successful-program curr-population) :SUCCESS) :SUCCESS
            (= curr-generation max-generations) nil
            :else (recur (take population-size
                               (repeatedly #(select-and-vary curr-population)))
                         (+ curr-generation 1))))))

;;;;;;;;;;
;; The main function. 

(defn -main
  "Runs push-gp, giving it a map of arguments."
  [& args]
  (push-gp {:instructions instructions
            :error-function regression-error-function
            :max-generations 500
            :population-size 200
            :max-initial-program-size 50}))
