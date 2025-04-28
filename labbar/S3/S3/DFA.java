import java.util.*;

public class DFA {
    int dfaStateCount; // amount of states in total 
    int dfaStartState; // q0: number for starting state
    List<Integer> states = new ArrayList<Integer>(); // Q: store all states
    List<Integer> acceptingStates = new ArrayList<Integer>(); // F: store accepting states
    Map<Integer, Map<Character, Integer>> transitions = new HashMap<>(); // Store all transitions (To, (Char, From))
    
    public static class Tuple {
        int state;
        String str;
        int depth;

        Tuple(int state, String str, int depth) {
            this.state = state;
            this.str = str;
            this.depth = depth;
        }
    }

    public DFA(int stateCount, int startState) {
        dfaStateCount = stateCount;
        dfaStartState = startState; 
        for(int i = 0; i < stateCount; i++) {
            states.add(i); 
        }
    }

    public void setAccepting(int state) {
        acceptingStates.add(state);
    }

    public void addTransition(int from, int to, char c) {
        transitions.computeIfAbsent(from, k -> new HashMap<>()).put(c, to); 
    }
    
    public List<String> getAcceptingStrings(int maxCount) {
        // System.out.println("Starting getAcceptingStrings with maxCount = " + maxCount);
        Deque<Tuple> stack = new LinkedList<>(); // Stack for DFS
        List<String> acceptedStrings = new ArrayList<>(); // Store accepted strings
        Set<String> visited = new HashSet<>(); // Track state-string pairs to detect cycles
        Map<Integer, Boolean> canReachAccepting = new HashMap<>(); // Cache states that can reach accepting states

        // Precompute which states can reach an accepting state
        computeReachableAcceptingStates(canReachAccepting);

        // Handle the case where the start state is accepting (empty string)
        if (acceptingStates.contains(dfaStartState)) {
            // System.out.println("Start state " + dfaStartState + " is accepting. Adding empty string.");
            acceptedStrings.add("");
            if (acceptedStrings.size() == maxCount) {
                // System.out.println("Reached maxCount with empty string. Returning early.");
                return acceptedStrings;
            }
        }

        stack.push(new Tuple(dfaStartState, "", 0));
        visited.add(dfaStartState + ":"); // Initial state with empty string

        while (!stack.isEmpty() && acceptedStrings.size() < maxCount) {
            Tuple current = stack.pop();
            int currentState = current.state;
            String currentString = current.str;
            int currentDepth = current.depth;

            // System.out.println("Popped: state=" + currentState + ", string=\"" + currentString + "\"");
            // System.out.println("AcceptedStrings: " + acceptedStrings.size());

            // Check transitions from the current state
            if (transitions.containsKey(currentState)) {
                for (Map.Entry<Character, Integer> entry : transitions.get(currentState).entrySet()) {
                    char symbol = entry.getKey();
                    int nextState = entry.getValue();
                    String newString = currentString + symbol;
                    String stateStringKey = nextState + ":" + newString;

                    // Skip if depth exceeded
                    int maxDepth = 100;
                    if (currentDepth >= maxDepth) {
                        // System.out.println("Depth limit reached for string: \"" + newString + "\"");
                        continue;
                    }

                    // Check if next state is accepting
                    if (acceptingStates.contains(nextState)) {
                        // System.out.println("Next state " + nextState + " is accepting. Adding string: \"" + newString + "\"");
                        acceptedStrings.add(newString);
                        if (acceptedStrings.size() == maxCount) {
                            // System.out.println("Reached maxCount. Returning accepted strings.");
                            return acceptedStrings;
                        }
                    }

                    // Check for cycle and handle if it can lead to an accepting state
                    if (visited.contains(stateStringKey)) {
                        // System.out.println("Cycle detected at state " + nextState + " with string \"" + newString + "\"");
                        if (canReachAccepting.getOrDefault(nextState, false)) {
                            // Generate strings by repeating the cycle
                            generateCycleStrings(nextState, newString, currentDepth, maxDepth, acceptedStrings, maxCount, canReachAccepting);
                            if (acceptedStrings.size() == maxCount) {
                                // System.out.println("Reached maxCount after cycle handling. Returning.");
                                return acceptedStrings;
                            }
                        }
                        continue; // Skip further exploration to avoid redundant paths
                    }

                    // Only explore states that can lead to accepting states
                    if (canReachAccepting.getOrDefault(nextState, false)) {
                        stack.push(new Tuple(nextState, newString, currentDepth + 1));
                        visited.add(stateStringKey);
                        // System.out.println("Pushed: state=" + nextState + ", string=\"" + newString + "\"");
                    } else {
                        // System.out.println("Skipping state " + nextState + " as it cannot reach an accepting state.");
                    }
                }
            } else {
                // System.out.println("No transitions from state " + currentState);
            }
        }

        // System.out.println("Finished search. Returning " + acceptedStrings.size() + " accepted strings.");
        for (String s : acceptedStrings) {
            // System.out.println("Accepted string: \"" + s + "\"");
        }
        return acceptedStrings;
    }

    // Helper method to compute which states can reach an accepting state
    private void computeReachableAcceptingStates(Map<Integer, Boolean> canReachAccepting) {
        // Initialize with false
        for (int state : states) {
            canReachAccepting.put(state, false);
        }
        // Accepting states can reach themselves
        for (int state : acceptingStates) {
            canReachAccepting.put(state, true);
        }
        // Backward DFS to find all states that can reach an accepting state
        boolean changed;
        do {
            changed = false;
            for (int state : states) {
                if (!canReachAccepting.get(state) && transitions.containsKey(state)) {
                    for (int nextState : transitions.get(state).values()) {
                        if (canReachAccepting.get(nextState)) {
                            canReachAccepting.put(state, true);
                            changed = true;
                            break;
                        }
                    }
                }
            }
        } while (changed);
    }

    // Helper method to generate strings by repeating a cycle
    private void generateCycleStrings(int cycleState, String baseString, int currentDepth, int maxDepth,
                                     List<String> acceptedStrings, int maxCount, Map<Integer, Boolean> canReachAccepting) {
        // System.out.println("Generating strings from cycle at state " + cycleState + " with base string \"" + baseString + "\"");
        Deque<Tuple> cycleStack = new LinkedList<>();
        cycleStack.push(new Tuple(cycleState, baseString, currentDepth));

        while (!cycleStack.isEmpty() && acceptedStrings.size() < maxCount) {
            Tuple current = cycleStack.pop();
            int state = current.state;
            String str = current.str;
            int depth = current.depth;

            if (depth >= maxDepth) {
                continue;
            }

            if (acceptingStates.contains(state)) {
                // System.out.println("Accepting state " + state + " reached in cycle. Adding string: \"" + str + "\"");
                acceptedStrings.add(str);
                if (acceptedStrings.size() == maxCount) {
                    return;
                }
            }

            if (transitions.containsKey(state)) {
                for (Map.Entry<Character, Integer> entry : transitions.get(state).entrySet()) {
                    int nextState = entry.getValue();
                    if (canReachAccepting.getOrDefault(nextState, false)) {
                        String newString = str + entry.getKey();
                        cycleStack.push(new Tuple(nextState, newString, depth + 1));
                        // System.out.println("Pushed in cycle: state=" + nextState + ", string=\"" + newString + "\"");
                    }
                }
            }
        }
    }
}