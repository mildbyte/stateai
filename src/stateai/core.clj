(ns stateai.core)

(def all-states
  '(:wander :feed :sleep))
(def all-attributes
  '(:hunger :health :fatigue))
(def initial-attributes
  {:hunger 100
   :health 100
   :fatigue 100})

(defrecord Agent [state attributes state-coeffs])

(defn make-random-agent []
  "Creates a random wandering agent"
  (Agent. :wander
          initial-attributes
          (into {} (for [st all-states, nst all-states, attr all-attributes]
                     [[st nst attr] (dec (* (rand) 2))]))))

(defn get-state-priority [agent new-state]
  "Calculates the priority of an agent transferring into a new state"
  (apply +
         (map (fn [[attrname attrval]]
                (* attrval
                   ((:state-coeffs agent) [(:state agent) new-state attrname])))
              (:attributes agent))))

(defn state-is-possible [agent new-state]
  "The agent can't sleep if its fatigue is already at maximum etc."
  (case new-state
    :wander true
    :feed   (< (:hunger (:attributes agent)) 100)
    :sleep  (< (:fatigue (:attributes agent)) 100)))

(defn get-next-state [agent]
  "Gets the state with the highest priority for a given agent"
  (apply (partial max-key (fn [new-state]
                             (get-state-priority agent new-state)))
         (filter #(state-is-possible agent %) all-states)))

(defn advance-agent [agent]
  "Moves the agent to his next state and changes its attributes accordingly"
  (Agent. (get-next-state agent)
          {:hunger (if (= :feed (:state agent))
                     (+ (:hunger (:attributes agent)) 5)
                     (- (:hunger (:attributes agent)) 1))
           :fatigue (if (= :sleep (:state agent))
                      (+ (:fatigue (:attributes agent)) 5)
                      (- (:hunger (:attributes agent)) 1))
           :health (+ (:health (:attributes agent))
                      (if (<= (:hunger (:attributes agent)) 0) -1 0)
                      (if (<= (:fatigue (:attributes agent)) 0) -1 0))}
          (:state-coeffs agent)))

(defn survives-time [agent max-time]
  "Simulates an agent until max-time to see how long it survives"
  (first (drop-while (fn [[survived currag]]
                       (and (> (:health (:attributes currag)) 0)
                            (< survived max-time)))
                     (iterate (fn [[survived currag]]
                                [(inc survived)
                                 (advance-agent currag)])
                              [0 agent]))))

;100 random agents
(def initial-population (take 100 (repeatedly make-random-agent)))

;Calculates a list of times a given population survives
(defn survivor-times [agents] (map #(first (survives-time % 20000)) agents))

;A population that neither survives forever nor dies immediately
;(doesn't eat or sleep; 100 ticks for the fatigue and hunger to go
;down and then 50 ticks for them to bring the health down to 0)
(def imperfect-population
  (take 100
        (filter (fn [agent] (let [[time ag] (survives-time agent 2000)]
                             (and (< time 2000)
                                  (> time 150))))
                (repeatedly make-random-agent))))


(defn combine-agent-coeffs [coeff1 coeff2]
  "Combines two sets of coefficients into one by averaging them"
  (into {} (for [st all-states, nst all-states, attr all-attributes]
             [[st nst attr] (/ (+ (coeff1 [st nst attr])
                                  (coeff2 [st nst attr]))
                               2)])))

(defn mate-agents [agent1 agent2]
  "Combines two agents into one by averaging their coefficients"
  (Agent. :wander
          initial-attributes
          (combine-agent-coeffs (:state-coeffs agent1)
                                (:state-coeffs agent2))))

