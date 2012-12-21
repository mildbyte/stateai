# stateai

An experiment with FSM-based AI agents. For every combination of current state, new state and an input, an agent has a certain coefficient. The priority of a new state is calculated as the sum of every input * coefficient for this input. The agent then chooses the state with the highest priority.

There isn't much going on here so far, but I plan to have a simulation of a world where agents roam around, mate and fight each other. A child of two agents will have the average coefficients of his parents. Thus, hopefully, some sort of natural selection can be demonstrated: agents with better coefficients will be more prevalent in the nature.
