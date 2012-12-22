(ns stateai.core)

(def all-states
  '(:wander :feed :sleep :mate))
(def all-attributes
  '(:hunger :health :fatigue :age :children))
(def additional-inputs
  '(:nearest-agent-health))
(def all-inputs (concat all-attributes additional-inputs))
(def initial-attributes
  {:hunger 100
   :health 100
   :fatigue 100
   :age 0
   :children 0})

(defrecord Agent [state attributes state-coeffs])
(defrecord World [age agents])

(defn square [x] (* x x))

(defn distance [[x1 y1] [x2 y2]]
  (+ (square (- x1 x2))
     (square (- y1 y2))))

(defn make-random-agent []
  "Creates a random wandering agent"
  (Agent. :wander
          initial-attributes
          (into {} (for [st all-states, nst all-states, attr all-inputs]
                     [[st nst attr] (dec (* (rand) 2))]))))

(defn make-random-world [number-of-agents]
  (World. 0
          (into #{} (map (fn [agent] [[(- (* 2000 (rand)) 1000)
                                      (- (* 2000 (rand)) 1000)]
                                     agent])
                         (repeatedly number-of-agents make-random-agent)))))

(defn get-state-priority [agent new-state inputs]
  "Calculates the priority of an agent transferring into a new state"
  (apply +
         (map (fn [[attrname attrval]]
                (* attrval
                   ((:state-coeffs agent) [(:state agent) new-state attrname])))
              (conj (:attributes agent) inputs))))

(defn state-is-possible [agent new-state]
  "The agent can't sleep if its fatigue is already at maximum etc."
  (case new-state
    :wander true
    :feed   (< (:hunger (:attributes agent)) 100)
    :sleep  (< (:fatigue (:attributes agent)) 100)
    :mate   (< (:children (:attributes agent)) 1)))

(defn get-next-state [agent inputs]
  "Gets the state with the highest priority for a given agent"
  (apply (partial max-key (fn [new-state]
                             (get-state-priority agent new-state inputs)))
         (filter #(state-is-possible agent %) all-states)))

(defn advance-agent [agent inputs]
  "Moves the agent to his next state and changes its attributes accordingly"
  (Agent. (get-next-state agent inputs)
          {:hunger (- (:hunger (:attributes agent)) 1)
           :fatigue (if (= :sleep (:state agent))
                      (+ (:fatigue (:attributes agent)) 5)
                      (- (:fatigue (:attributes agent)) 1))
           :health (+ (:health (:attributes agent))
                      (if (<= (:hunger (:attributes agent)) 0) -1 0)
                      (if (<= (:fatigue (:attributes agent)) 0) -1 0)
                      (if (> (:age (:attributes agent)) 500) -1 0))
           :age (inc (:age (:attributes agent)))}
          (:state-coeffs agent)))

(defn combine-agent-coeffs [coeff1 coeff2]
  "Combines two sets of coefficients into one by averaging them"
  (into {} (for [st all-states, nst all-states, attr all-inputs]
             [[st nst attr] (/ (+ (coeff1 [st nst attr])
                                  (coeff2 [st nst attr]))
                               2)])))

(defn mate-agents [agent1 agent2]
  "Combines two agents into one by averaging their coefficients"
  (Agent. :wander
          initial-attributes
          (combine-agent-coeffs (:state-coeffs agent1)
                                (:state-coeffs agent2))))

(defn is-alive [agent] (> (:health (:attributes agent)) 0))

(defn advance-world [world]
  (World.
   (inc (:age world))
   (into
    #{}
    (filter
     (fn [[[x y] agent]] (is-alive agent))
     (map
      (fn [[[x y] agent]] [[x y] (advance-agent agent '())])
      (reduce
       (fn [processed-agents [[xcurr ycurr] current-agent]]
         (let [by-distance
               (sort-by
                (fn [[[x y] agent]]
                  (distance [xcurr ycurr]
                            [x y])) processed-agents)]
           (case (:state current-agent)
             :wander
             (conj processed-agents [[xcurr ycurr] current-agent])
             :sleep
             (conj processed-agents [[xcurr ycurr] current-agent])
             :feed
             (let [[[x y] victim] (second by-distance)]
               (conj (if (= (:state victim) :sleep)
                       processed-agents
                       (disj processed-agents [[x y] victim]))
                     [[xcurr ycurr] current-agent]))
             :mate
             (let [[[x y] partner] (second (filter (fn [[[x y] partner]]
                                                     (= (:state partner) :mate))))]
               (conj (if partner (conj processed-agents
                                       [[0 0] (mate-agents current-agent partner)])
                         processed-agents))))))
       #{}
       (:agents world)))))))