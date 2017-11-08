;; Oliver Keh
;; Victoria Slack
;; Term Project Part 2
;; CPSCI 307 


(ns push307.core
  (:gen-class)
  (:require [clojure.string :as string]))


; An example individual in the population
; Made of a map containing, at mimimum, a program, the errors for
; the program, and a total error
(def example-individual
  {:program '(3 5 integer_* "hello" 4 "world" integer_-)
   :errors [8 1 6 5 4 3 2 1 0 1]
   :total-error 37})


; An example individual in the population
; Made of a map containing, at mimimum, a program, the errors for
; the program, and a total error
(def example-individual2
  {:program '(hello thing exec_do*range "foo" 6 "foobar" integer_*)
   :errors [1 1 1 1 1 1 1 1 1 1]
   :total-error 10})



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
;; Plush


(def instruction-parentheses
  '{exec_dup 1
   exec_if 2})

(defn lookup-instruction-paren-groups
  [ins]
  (let [ins-req (get instruction-parentheses ins)]
    (cond
      ins-req ins-req
      :else 0)))

(defn open-close-sequence-to-list
  [sequence]
  (cond (not (seq? sequence)) sequence
        (empty? sequence) ()
        :else (let [opens (count (filter #(= :open %) sequence))
                    closes (count (filter #(= :close %) sequence))]
                (assert (= opens closes)
                        (str "open-close sequence must have equal numbers of :open and :close; this one does not:\n" sequence))
                (let [s (str sequence)
                      l (read-string (string/replace (string/replace s ":open" " ( ") ":close" " ) "))]
                  ;; there'll be an extra ( ) around l, which we keep if the number of read things is >1
                  (if (= (count l) 1)
                    (first l)
                    l)))))

(defn delete-prev-paren-pair
  "Deletes the last closed paren pair from prog, which may be a partial program."
  [prog]
  (loop [reversed-prog (reverse prog)
         new-prog []
         number-close-parens 0
         found-first-close false]
    (cond
      ; Check if reversed-prog is empty, in which case we are done
      (empty? reversed-prog) (vec (reverse new-prog))
      ; Check if done, which is if we've found the first :close, the paren-stack is empty, and the first item in reversed-prog is :open
      (and found-first-close
           (zero? number-close-parens)
           (= :open (first reversed-prog))) (vec (reverse (concat new-prog (rest reversed-prog))))
      ; Check if looking for the correct :open but found an :open for a different paren
      (and found-first-close
           (< 0 number-close-parens)
           (= :open (first reversed-prog))) (recur (rest reversed-prog)
                                                   (conj new-prog (first reversed-prog))
                                                   (dec number-close-parens)
                                                   found-first-close)
      ; Check if looking for correct :open but found another :close
      (and found-first-close
           (= :close (first reversed-prog))) (recur (rest reversed-prog)
                                                    (conj new-prog (first reversed-prog))
                                                    (inc number-close-parens)
                                                    found-first-close)
      ; Check if just found first :close. In which case skip it and set the found-first-close flag
      (and (not found-first-close)
           (= :close (first reversed-prog))) (recur (rest reversed-prog)
                                                    new-prog
                                                    0
                                                    true)
      ; Otherwise, just put the item onto new-prog and keep looking with same other variables
      :else (recur (rest reversed-prog)
                   (conj new-prog (first reversed-prog))
                   number-close-parens
                   found-first-close))))

(defn translate-plush-genome-to-push-program
  "Takes as input an individual (or map) containing a Plush genome (:genome)
   and translates it to the correct Push program with
   balanced parens. The linear Plush genome is made up of a list of instruction
   maps, each including an :instruction key as well as other epigenetic marker
   keys. As the linear Plush genome is traversed, each instruction that requires
   parens will push :close and/or :close-open onto the paren-stack, and will
   also put an open paren after it in the program. For example, an instruction
   that requires 3 paren groupings will push :close, then :close-open, then :close-open.
   When a positive number is encountered in the :close key of the
   instruction map, it is set to num-parens-here during the next recur. This
   indicates the number of parens to put here, if need is indicated on the
   paren-stack. If the top item of the paren-stack is :close, a close paren
   will be inserted. If the top item is :close-open, a close paren followed by
   an open paren will be inserted.
   If the end of the program is reached but parens are still needed (as indicated by
   the paren-stack), parens are added until the paren-stack is empty.
   Instruction maps that have :silence set to true will be ignored entirely."
  [{:keys [genome program]}]
  (if program
    program
    (loop [prog [] ; The Push program incrementally being built
           gn genome ; The linear Plush genome, where items will be popped off the front. Each item is a map containing at least the key :instruction, and unless the program is flat, also :close
           num-parens-here 0 ; The number of parens that still need to be added at this location.
           paren-stack '()] ; Whenever an instruction requires parens grouping, it will push either :close or :close-open on this stack. This will indicate what to insert in the program the next time a paren is indicated by the :close key in the instruction map.
      (cond
                                        ; Check if need to add close parens here
        (< 0 num-parens-here) (recur (cond
                                       (= (first paren-stack) :close) (conj prog :close)
                                       (= (first paren-stack) :close-open) (conj (conj prog :close) :open)
                                       :else prog) ; If paren-stack is empty, we won't put any parens in even though the :close epigenetic marker indicated to do so
                                     gn
                                     (dec num-parens-here)
                                     (rest paren-stack))
                                        ; Check if at end of program but still need to add parens
        (and (empty? gn)
             (not (empty? paren-stack))) (recur prog
                                                gn
                                                (count paren-stack)
                                                paren-stack)
                                        ; Check if done
        (empty? gn) (open-close-sequence-to-list (apply list prog))
                                        ; Check for no-oped instruction. This instruction will be replaced by exec_noop, but will still have effects like :close count
        (= (:silent (first gn)) :no-op) (recur (conj prog 'exec_noop)
                                               (rest gn)
                                               (get (first gn) :close 0)
                                               paren-stack)
                                        ; Check for silenced instruction
        (get (first gn) :silent false) (recur prog
                                              (rest gn)
                                              num-parens-here
                                              paren-stack)
                                        ; If here, ready for next instruction
        :else (let [number-paren-groups (lookup-instruction-paren-groups (:instruction (first gn)))
                    new-paren-stack (if (>= 0 number-paren-groups)
                                      paren-stack
                                      (concat (repeat (dec number-paren-groups) :close-open)
                                              '(:close)
                                              paren-stack))]
                (if (= 'noop_delete_prev_paren_pair (:instruction (first gn)))
                  (recur (delete-prev-paren-pair prog)
                         (rest gn)
                         (get (first gn) :close 0)
                         new-paren-stack)
                  (recur (if (= 'noop_open_paren (:instruction (first gn)))
                           (conj prog :open)
                           (if (>= 0 number-paren-groups)
                             (conj prog (:instruction (first gn)))
                             (conj (conj prog (:instruction (first gn))) :open)))
                         (rest gn)
                         (get (first gn) :close 0) ; The number of close parens to put after this instruction; if :close isn't in instruction map, default to zero
                         new-paren-stack)))))))


;;;;;;;;;;
;; Utilities

(def empty-push-state
  {:exec '()
   :integer '()
   :float '()
   :input {}})

(def digits-of-e
  (map #(- (int %) 48) (seq "27182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274274663919320030599218174135966290435729003342952605956307381323286279434907632338298807531952510190115738341879307021540891499348841675092447614606680822648001684774118537423454424371075390777449920695517027618386062613313845830007520449338265602976067371132007093287091274437470472306969772093101416928368190255151086574637721112523897844250569536967707854499699679468644549059879316368892300987931277361782154249992295763514822082698951936680331825288693984964651058209392398294887933203625094431173012381970684161403970198376793206832823764648042953118023287825098194558153017567173613320698112509961818815930416903515988885193458072738667385894228792284998920868058257492796104841984443634632449684875602336248270419786232090021609902353043699418491463140934317381436405462531520961836908887070167683964243781405927145635490613031072085103837505101157477041718986106873969655212671546889570350354")))

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

(defn interpret-parens
  [push-state parens]
  (loop [curr-state push-state
         parens (reverse parens)
         curr-instruction (first parens)]
    (if (= curr-instruction nil)
      curr-state
      (recur (push-to-stack curr-state :exec curr-instruction)
             (rest parens)
             (first (rest parens))))))

(defn interpret-one-step
  "Helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack
  by evaluating the first item on the exec stack and places the result on the
  appropriate stack, but if the next element is an integer, pushes it onto the
  integer stack correct stack and if it is a string, pushes it to the string stack.
  Also pops that element off the exec stack returns the new Push state."
  [push-state]
  (let [gene (first (get push-state :exec))
        curr (if (list? gene)
               (interpret-parens (pop-stack push-state :exec) gene)
               (eval (first (get push-state :exec))))]
    (println curr)
    (if (map? curr)
      curr
      (cond (float? curr) (push-to-stack (pop-stack push-state :exec)
                                         :float
                                         curr)
            (integer? curr) (push-to-stack (pop-stack push-state :exec)
                                           :integer
                                           curr)
            (string? curr) (push-to-stack (pop-stack push-state :exec)
                                          :string
                                          curr)
            :else (curr (pop-stack push-state :exec))))))


             
(defn interpret-push-program
  "Takes a program and a start state. Runs the given program starting with the stacks
  in start-state (empty push state with appropriate input). Associates the program with the
  exec to start and evaluates from there. Continues until the exec stack is empty.
  Returns the state of the stacks after the program finishes executing, otherwise it interprets
  the next step of the program."
  [genome start-state]
  (loop [curr-state (assoc start-state :exec (translate-plush-genome-to-push-program genome))]
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
  (let [genome {}
        newprogram '()
        program-size (rand-int (+ max-initial-program-size 1))]
    (loop [add_instructions program-size
           newprogram newprogram
           instructions instructions]
      (if (= add_instructions 0)
        (assoc genome :genome newprogram)
        (recur (- add_instructions 1)
               (conj newprogram {:instruction (rand-nth instructions)
                                 :close (rand-int 4)})
               instructions)))))

(defn tournament-selection
  "Takes a population. Selects an individual from the population using a tournament.
  Returned individual will be a parent in the next generation. Can use a fixed
  tournament size. Takes 6 random programs from the population and applies a
  function to find the minimum total error and returns that individual."
  [population]
  (let [selected-individuals (into [] (take 6 (repeatedly #(rand-nth population))))]
    (apply min-key :total-error selected-individuals)))

(defn lexicase-selection
  [population tests]
  (let [tests-in-random-order (shuffle tests)]
    (loop [tests-in-random-order tests-in-random-order
           candidates-left population]
      (if (= (count tests-in-random-order) 1)
        (rand-nth candidates-left)
      (let [best-ind-err (apply min
                                (map #(nth % (first tests-in-random-order))
                                     (map #(get % :errors)
                                          [example-individual example-individual2])))]
        (recur 
               (rest tests-in-random-order)
               (filter #(<=
                         (get (get % :errors) (first tests-in-random-order))
                         best-ind-err)
                       candidates-left)))))))
       
(defn crossover
  "Takes to progarms. Crosses over two programs (not individuals) using uniform crossover.
  Returns child program. Checks to see if both parent progams are empty and if so filters out
  any instructions that are nil (which would happen if one program becomes empty before the other,
  where in that case we would still have a 50% chance of including each instruction from the rest of
  the other program) and then reverses the resulting program so it is returned in the
  correct order (not backwards). Otherwise, continues to build the child program through the 50%
  chance of the instruction being taken from parent A or parent B."
  [genome-a genome-b]
  (loop [A genome-a
         B genome-b
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
  [genome]
  (loop [genome genome
         curr (first genome)
         new-genome '()]
    (if (empty? genome)
      (if (= (rand-int 20) 0)
        (reverse (conj new-genome (rand-nth instructions)))
        (reverse new-genome))
      (recur (rest genome)
             (first (rest genome))
             (if (= (rand-int 20) 0)
               (conj (conj new-genome
                           (rand-nth instructions)) curr)
               (conj new-genome curr))))))

(defn uniform-deletion
  "Takes a progam. Randomly deletes instructions from program at a 5% rate.
  This means that there is a 95% chance the instruction will stay.
  Returns child program."
  [genome]
  (random-sample 0.95 genome))


(defn absolute-value
  "Takes a number and checks to see if it is negative (below 0), and if so,
  multiplies by -1 to make it positive, so the function always returns a positive
  number (the absolute value of the number)."
  [number]
  (if (< number 0)
    (* -1 number)
    number))


;(defn get-error
 ; "Takes a program and an input value for the function. Computes the value we
 ; want from the target function as well as the value we get from the program
 ; we are evaluating by getting the top value from the integer stack of the
;  end state. Then, if the program returned nothing, we assign it an error value
  ;of 10000, otherwise its error value is the absolute value of the value we want minus
 ; the value we got and we return that value."
  ;[program input]
  ;(let [target-value (target-function input)
   ;     program-value (first (get (interpret-push-program program
   ;                                          (assoc empty-push-state
    ;                                                :input {:in1 input}))
   ;                               :integer))]
    ;(if (nil? program-value)
    ;  10000
    ;  (absolute-value (- target-value  program-value)))))      

(defn get-error
  [program test-case]
  (let [correct-digit (nth digits-of-e test-case)
        program-digit (first (get (interpret-push-program program
                                                          (assoc empty-push-state
                                                             :input {:in1 test-case}))) :integer)]
    (if (nil? program-digit)
      10000
      (absolute-value (- correct-digit program-digit)))))
  
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

(defn number-e-error-function
  [individual]
  (loop [curr-test 0
         errors (get individual :errors)]
    (if (> curr-test 100)
      {:program (get individual :program)
       :errors errors
       :total-error (apply + errors)}
      (recur (+ curr-test 1)
             (conj errors (get-error (get individual :program)
                                       curr-test))))))

(defn make-individual-from-program
  "Takes a program. Returns an individual with the program set to :program, and the errors
  vector with the associated error values that we get from our regression-error-function as well
  as the total-error which we also get from the function and return the resulting individual."
  [program]
  (let [individual {:program program
                    :errors []
                    :total-error 0}]
    (regression-error-function individual)))

(defn choose-parent-selection
  [population test-cases]
  (let [prob (+ (rand-int 100) 1)]
    (cond
      (<= prob 50)
      (lexicase-selection population test-cases)
      (> prob 50)
      (tournament-selection population))))

(defn select-and-vary
  "Takes a population. Selects parent(s) from population and varies them, returning
  a child individual. Assign a random probabilty and if by a 50% chance crossover if chosen,
  choose two parents using tournament selection and return the resulting individual. If by a
  25% chance we get  uniform-addition, do that and again 25% to uniform-deletion. Each returning
  the resulting individual if chosen."
  [population test-cases]
  (let [prob-genetic-op (+ (rand 100) 1)]
    (cond (<= prob-genetic-op 50)

          (make-individual-from-program (crossover (get (get (choose-parent-selection population
                                                                                      test-cases)
                                                             :program)
                                                        :genome)
                                                   (get (get (choose-parent-selection population
                                                                                      test-cases)
                                                             :program)
                                                        :genome)))
          (<= prob-genetic-op 75)
          (make-individual-from-program (uniform-addition (get (get (choose-parent-selection population
                                                                                             test-cases)
                                                                    :program)
                                                               :genome)))
          :else
          (make-individual-from-program (uniform-deletion (get (get (choose-parent-selection population
                                                                                             test-cases)
                                                                    :program)
                                                               :genome))))))


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
    (println "Best program:" (translate-plush-genome-to-push-program (get best-program :program)))
    (println "Best program size:" (count (get (get best-program :program) :genome)))
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
                                                                              max-initial-program-size))))
        test-cases (take 1000 (range))
        ]
    (loop [curr-population original-population
           curr-generation 0]
      (report curr-population curr-generation)
      (cond (= (find-successful-program curr-population) :SUCCESS) :SUCCESS
            (= curr-generation max-generations) nil
            :else (recur (take population-size
                               (repeatedly #(select-and-vary curr-population test-cases)))
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
;; Oliver Keh
;; Victoria Slack
;; Term Project Part 2
;; CPSCI 307 


(ns push307.core
  (:gen-class)
  (:require [clojure.string :as string]))


; An example individual in the population
; Made of a map containing, at mimimum, a program, the errors for
; the program, and a total error
(def example-individual
  {:program '(3 5 integer_* "hello" 4 "world" integer_-)
   :errors [8 1 6 5 4 3 2 1 0 1]
   :total-error 37})


; An example individual in the population
; Made of a map containing, at mimimum, a program, the errors for
; the program, and a total error
(def example-individual2
  {:program '(hello thing exec_do*range "foo" 6 "foobar" integer_*)
   :errors [1 1 1 1 1 1 1 1 1 1]
   :total-error 10})



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
;; Plush


(def instruction-parentheses
  '{})

(defn lookup-instruction-paren-groups
  [ins]
  (let [ins-req (get instruction-parentheses ins)]
    (cond
      ins-req ins-req
      :else 0)))

(defn open-close-sequence-to-list
  [sequence]
  (cond (not (seq? sequence)) sequence
        (empty? sequence) ()
        :else (let [opens (count (filter #(= :open %) sequence))
                    closes (count (filter #(= :close %) sequence))]
                (assert (= opens closes)
                        (str "open-close sequence must have equal numbers of :open and :close; this one does not:\n" sequence))
                (let [s (str sequence)
                      l (read-string (string/replace (string/replace s ":open" " ( ") ":close" " ) "))]
                  ;; there'll be an extra ( ) around l, which we keep if the number of read things is >1
                  (if (= (count l) 1)
                    (first l)
                    l)))))

(defn delete-prev-paren-pair
  "Deletes the last closed paren pair from prog, which may be a partial program."
  [prog]
  (loop [reversed-prog (reverse prog)
         new-prog []
         number-close-parens 0
         found-first-close false]
    (cond
      ; Check if reversed-prog is empty, in which case we are done
      (empty? reversed-prog) (vec (reverse new-prog))
      ; Check if done, which is if we've found the first :close, the paren-stack is empty, and the first item in reversed-prog is :open
      (and found-first-close
           (zero? number-close-parens)
           (= :open (first reversed-prog))) (vec (reverse (concat new-prog (rest reversed-prog))))
      ; Check if looking for the correct :open but found an :open for a different paren
      (and found-first-close
           (< 0 number-close-parens)
           (= :open (first reversed-prog))) (recur (rest reversed-prog)
                                                   (conj new-prog (first reversed-prog))
                                                   (dec number-close-parens)
                                                   found-first-close)
      ; Check if looking for correct :open but found another :close
      (and found-first-close
           (= :close (first reversed-prog))) (recur (rest reversed-prog)
                                                    (conj new-prog (first reversed-prog))
                                                    (inc number-close-parens)
                                                    found-first-close)
      ; Check if just found first :close. In which case skip it and set the found-first-close flag
      (and (not found-first-close)
           (= :close (first reversed-prog))) (recur (rest reversed-prog)
                                                    new-prog
                                                    0
                                                    true)
      ; Otherwise, just put the item onto new-prog and keep looking with same other variables
      :else (recur (rest reversed-prog)
                   (conj new-prog (first reversed-prog))
                   number-close-parens
                   found-first-close))))

(defn translate-plush-genome-to-push-program
  "Takes as input an individual (or map) containing a Plush genome (:genome)
   and translates it to the correct Push program with
   balanced parens. The linear Plush genome is made up of a list of instruction
   maps, each including an :instruction key as well as other epigenetic marker
   keys. As the linear Plush genome is traversed, each instruction that requires
   parens will push :close and/or :close-open onto the paren-stack, and will
   also put an open paren after it in the program. For example, an instruction
   that requires 3 paren groupings will push :close, then :close-open, then :close-open.
   When a positive number is encountered in the :close key of the
   instruction map, it is set to num-parens-here during the next recur. This
   indicates the number of parens to put here, if need is indicated on the
   paren-stack. If the top item of the paren-stack is :close, a close paren
   will be inserted. If the top item is :close-open, a close paren followed by
   an open paren will be inserted.
   If the end of the program is reached but parens are still needed (as indicated by
   the paren-stack), parens are added until the paren-stack is empty.
   Instruction maps that have :silence set to true will be ignored entirely."
  [{:keys [genome program]}]
  (if program
    program
    (loop [prog [] ; The Push program incrementally being built
           gn genome ; The linear Plush genome, where items will be popped off the front. Each item is a map containing at least the key :instruction, and unless the program is flat, also :close
           num-parens-here 0 ; The number of parens that still need to be added at this location.
           paren-stack '()] ; Whenever an instruction requires parens grouping, it will push either :close or :close-open on this stack. This will indicate what to insert in the program the next time a paren is indicated by the :close key in the instruction map.
      (cond
                                        ; Check if need to add close parens here
        (< 0 num-parens-here) (recur (cond
                                       (= (first paren-stack) :close) (conj prog :close)
                                       (= (first paren-stack) :close-open) (conj (conj prog :close) :open)
                                       :else prog) ; If paren-stack is empty, we won't put any parens in even though the :close epigenetic marker indicated to do so
                                     gn
                                     (dec num-parens-here)
                                     (rest paren-stack))
                                        ; Check if at end of program but still need to add parens
        (and (empty? gn)
             (not (empty? paren-stack))) (recur prog
                                                gn
                                                (count paren-stack)
                                                paren-stack)
                                        ; Check if done
        (empty? gn) (open-close-sequence-to-list (apply list prog))
                                        ; Check for no-oped instruction. This instruction will be replaced by exec_noop, but will still have effects like :close count
        (= (:silent (first gn)) :no-op) (recur (conj prog 'exec_noop)
                                               (rest gn)
                                               (get (first gn) :close 0)
                                               paren-stack)
                                        ; Check for silenced instruction
        (get (first gn) :silent false) (recur prog
                                              (rest gn)
                                              num-parens-here
                                              paren-stack)
                                        ; If here, ready for next instruction
        :else (let [number-paren-groups (lookup-instruction-paren-groups (:instruction (first gn)))
                    new-paren-stack (if (>= 0 number-paren-groups)
                                      paren-stack
                                      (concat (repeat (dec number-paren-groups) :close-open)
                                              '(:close)
                                              paren-stack))]
                (if (= 'noop_delete_prev_paren_pair (:instruction (first gn)))
                  (recur (delete-prev-paren-pair prog)
                         (rest gn)
                         (get (first gn) :close 0)
                         new-paren-stack)
                  (recur (if (= 'noop_open_paren (:instruction (first gn)))
                           (conj prog :open)
                           (if (>= 0 number-paren-groups)
                             (conj prog (:instruction (first gn)))
                             (conj (conj prog (:instruction (first gn))) :open)))
                         (rest gn)
                         (get (first gn) :close 0) ; The number of close parens to put after this instruction; if :close isn't in instruction map, default to zero
                         new-paren-stack)))))))


;;;;;;;;;;
;; Utilities

(def empty-push-state
  {:exec '()
   :integer '()
   :float '()
   :input {}})

(def digits-of-e
  (map #(- (int %) 48) (seq "27182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274274663919320030599218174135966290435729003342952605956307381323286279434907632338298807531952510190115738341879307021540891499348841675092447614606680822648001684774118537423454424371075390777449920695517027618386062613313845830007520449338265602976067371132007093287091274437470472306969772093101416928368190255151086574637721112523897844250569536967707854499699679468644549059879316368892300987931277361782154249992295763514822082698951936680331825288693984964651058209392398294887933203625094431173012381970684161403970198376793206832823764648042953118023287825098194558153017567173613320698112509961818815930416903515988885193458072738667385894228792284998920868058257492796104841984443634632449684875602336248270419786232090021609902353043699418491463140934317381436405462531520961836908887070167683964243781405927145635490613031072085103837505101157477041718986106873969655212671546889570350354")))

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

(defn interpret-parens
  [push-state parens]
  (loop [curr-state push-state
         parens (reverse parens)
         curr-instruction (first parens)]
    (if (= curr-instruction nil)
      curr-state
      (recur (push-to-stack curr-state :exec curr-instruction)
             (rest parens)
             (first (rest parens))))))

(defn interpret-one-step
  "Helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack
  by evaluating the first item on the exec stack and places the result on the
  appropriate stack, but if the next element is an integer, pushes it onto the
  integer stack correct stack and if it is a string, pushes it to the string stack.
  Also pops that element off the exec stack returns the new Push state."
  [push-state]
  (let [gene (first (get push-state :exec))
        curr (if (list? gene)
               (interpret-parens (pop-stack push-state :exec) gene)
               (eval (first (get push-state :exec))))]
    (if (map? curr)
      curr
      (cond (float? curr) (push-to-stack (pop-stack push-state :exec)
                                         :float
                                         curr)
            (integer? curr) (push-to-stack (pop-stack push-state :exec)
                                           :integer
                                           curr)
            (string? curr) (push-to-stack (pop-stack push-state :exec)
                                          :string
                                          curr)
            :else (curr (pop-stack push-state :exec))))))


             
(defn interpret-push-program
  "Takes a program and a start state. Runs the given program starting with the stacks
  in start-state (empty push state with appropriate input). Associates the program with the
  exec to start and evaluates from there. Continues until the exec stack is empty.
  Returns the state of the stacks after the program finishes executing, otherwise it interprets
  the next step of the program."
  [genome start-state]
  (loop [curr-state (assoc start-state :exec (translate-plush-genome-to-push-program genome))]
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
  (let [genome {}
        newprogram '()
        newgenome '()
        program-size (rand-int (+ max-initial-program-size 1))]
    (loop [add_instructions program-size
           newprogram newprogram
           newgenome newgenome
           instructions instructions]
      (if (= add_instructions 0)
        (assoc (assoc genome :genome newgenome) :program newprogram)
        (let [curr-instruction (rand-nth instructions)
              curr-close (rand-int 4)]
              
          (recur (- add_instructions 1)
                 (conj newprogram curr-instruction)
                 (conj newgenome {:instruction curr-instruction
                                  :close curr-close})
                 instructions))))))

(defn tournament-selection
  "Takes a population. Selects an individual from the population using a tournament.
  Returned individual will be a parent in the next generation. Can use a fixed
  tournament size. Takes 6 random programs from the population and applies a
  function to find the minimum total error and returns that individual."
  [population]
  (let [selected-individuals (into [] (take 6 (repeatedly #(rand-nth population))))]
    (apply min-key :total-error selected-individuals)))

(defn lexicase-selection
  [population tests]
  (let [tests-in-random-order (shuffle tests)]
    (loop [tests-in-random-order tests-in-random-order
           candidates-left population]
      (if (= (count tests-in-random-order) 1)
        (rand-nth candidates-left)
      (let [best-ind-err (apply min
                                (map #(nth % (first tests-in-random-order))
                                     (map #(get % :errors)
                                          [example-individual example-individual2])))]
        (recur 
               (rest tests-in-random-order)
               (filter #(<=
                         (get (get % :errors) (first tests-in-random-order))
                         best-ind-err)
                       candidates-left)))))))

(defn make-program-from-genome
  [genome]
  (loop [genome genome
         newprogram '()]
    (if (empty? genome)
      (reverse newprogram)
      (recur (rest genome)
             (conj newprogram (get (first genome) :instruction))))))
       
(defn crossover
  "Takes to progarms. Crosses over two programs (not individuals) using uniform crossover.
  Returns child program. Checks to see if both parent progams are empty and if so filters out
  any instructions that are nil (which would happen if one program becomes empty before the other,
  where in that case we would still have a 50% chance of including each instruction from the rest of
  the other program) and then reverses the resulting program so it is returned in the
  correct order (not backwards). Otherwise, continues to build the child program through the 50%
  chance of the instruction being taken from parent A or parent B."
  [genome-a genome-b]
  (loop [A genome-a
         B genome-b
         child '()]
    (if (and (empty? A) (empty? B))
      (let [genome (filter #(not= % nil) (reverse child))
            program (make-program-from-genome genome)]
        {:genome genome
         :program program})
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
  [genome]
  (loop [genome genome
         curr (first genome)
         new-genome '()]
    (if (empty? genome)
      (let [genome (if (= (rand-int 20) 0)
                     (reverse (conj new-genome (rand-nth instructions)))
                     (reverse new-genome))
            program (make-program-from-genome genome)]
        {:genome genome
         :program program})
      (recur (rest genome)
             (first (rest genome))
             (if (= (rand-int 20) 0)
               (conj (conj new-genome
                           (rand-nth instructions)) curr)
               (conj new-genome curr))))))

(defn uniform-deletion
  "Takes a progam. Randomly deletes instructions from program at a 5% rate.
  This means that there is a 95% chance the instruction will stay.
  Returns child program."
  [genome]
  (let [genome (random-sample 0.95 genome)
        program (make-program-from-genome genome)]
    {:genome genome
     :program program}))


(defn absolute-value
  "Takes a number and checks to see if it is negative (below 0), and if so,
  multiplies by -1 to make it positive, so the function always returns a positive
  number (the absolute value of the number)."
  [number]
  (if (< number 0)
    (* -1 number)
    number))


;(defn get-error
 ; "Takes a program and an input value for the function. Computes the value we
 ; want from the target function as well as the value we get from the program
 ; we are evaluating by getting the top value from the integer stack of the
;  end state. Then, if the program returned nothing, we assign it an error value
  ;of 10000, otherwise its error value is the absolute value of the value we want minus
 ; the value we got and we return that value."
  ;[program input]
  ;(let [target-value (target-function input)
   ;     program-value (first (get (interpret-push-program program
   ;                                          (assoc empty-push-state
    ;                                                :input {:in1 input}))
   ;                               :integer))]
    ;(if (nil? program-value)
    ;  10000
    ;  (absolute-value (- target-value  program-value)))))      

(defn get-error
  [program test-case]
  ;(println test-case)
  ;(println "HELLO")
  (let [correct-digit (nth digits-of-e test-case)
        program-digit (first (get (interpret-push-program program
                                                          (assoc empty-push-state
                                                             :input {:in1 test-case})) :integer))]
    (if (nil? program-digit)
      10000
      (absolute-value (- correct-digit program-digit)))))
  
;(defn regression-error-function
;  "Takes an individual and evaluates it on some test cases. For each test case,
;  Checks to see if we have done all 21 test cases [-10, 10], an is so returns the
;  individual with errors set to the list of error values, and total-error set to
;  the result when addition is applied to the error value list. Otherwise, we test
;  the program of the individual on the input and add the error to the list of error;s
;  and move on to the next test case by adding 1 to the current input."
;  [individual]
;  (loop [curr-input -10
;         errors (get individual :errors)]
;    (if (> curr-input 10)
;      {:program (get individual :program)
;       :errors errors
;       :total-error (apply + errors)}
;      (recur (+ curr-input 1)
;             (conj errors (get-error (get individual :program)
;                                     curr-input))))))

(defn number-e-error-function
  [individual]
  (loop [curr-test 0
         errors (get individual :errors)]
    (if (> curr-test 100)
      {:program (get individual :program)
       :errors errors
       :total-error (apply + errors)}
      (recur (+ curr-test 1)
             (conj errors (get-error (get individual :program)
                                       curr-test))))))

(defn make-individual-from-program
  "Takes a program. Returns an individual with the program set to :program, and the errors
  vector with the associated error values that we get from our regression-error-function as well
  as the total-error which we also get from the function and return the resulting individual."
  [program]
  (let [individual {:program program
                    :errors []
                    :total-error 0}]
    (number-e-error-function individual)))

(defn choose-parent-selection
  [population test-cases]
  (let [prob (+ (rand-int 100) 1)]
    (cond
      (<= prob 50)
      (lexicase-selection population test-cases)
      (> prob 50)
      (tournament-selection population))))

(defn select-and-vary
  "Takes a population. Selects parent(s) from population and varies them, returning
  a child individual. Assign a random probabilty and if by a 50% chance crossover if chosen,
  choose two parents using tournament selection and return the resulting individual. If by a
  25% chance we get  uniform-addition, do that and again 25% to uniform-deletion. Each returning
  the resulting individual if chosen."
  [population test-cases]
  (let [prob-genetic-op (+ (rand 100) 1)]
    (cond (<= prob-genetic-op 50)

          (make-individual-from-program (crossover (get (get (choose-parent-selection population test-cases)
                                                             :program)
                                                        :genome)
                                                   (get (get (choose-parent-selection population test-cases)
                                                             :program)
                                                        :genome)))
          (<= prob-genetic-op 75)
          (make-individual-from-program (uniform-addition (get (get (choose-parent-selection population test-cases)
                                                                    :program)
                                                               :genome)))
          :else
          (make-individual-from-program (uniform-deletion (get (get (choose-parent-selection population test-cases)
                                                                    :program)
                                                               :genome))))))


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
    (println "Best program:" (translate-plush-genome-to-push-program (get best-program :program)))
    (println "Best program size:" (count (get (get best-program :program) :genome)))
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
                                                                              max-initial-program-size))))
        test-cases (take 1000 (range))
        ]
    (loop [curr-population original-population
           curr-generation 0]
      (report curr-population curr-generation)
      (cond (= (find-successful-program curr-population) :SUCCESS) :SUCCESS
            (= curr-generation max-generations) nil
            :else (recur (take population-size
                               (repeatedly #(select-and-vary curr-population test-cases)))
                         (+ curr-generation 1))))))

;;;;;;;;;;
;; The main function. 

(defn -main
  "Runs push-gp, giving it a map of arguments."
  [& args]
  (push-gp {:instructions instructions
            :error-function number-e-error-function
            :max-generations 500
            :population-size 200
            :max-initial-program-size 50}))
