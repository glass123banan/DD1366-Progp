/* Kodskelett för labb S3 i DD1361 Programmeringsparadigm
 *
 * Författare: Per Austrin
 */

// Imports
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.ArrayList;
//import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;

public class DFA {
	// fields
	int dfaStateCount; // amount of states in total 
	int dfaStartState; // q0: number for starting state

	// DFA = (Q, ∑, 𝛿, q0, F)
	List<Integer> states = new ArrayList<Integer>(); // Q: store all states
	List<Integer> acceptingStates = new ArrayList<Integer>(); // F: store accepting states
	Map<Integer, Map<Character, Integer>> transitions = new HashMap<>(); // Store all transitions (To, (Char, From))
	
	// class for tuples with stringbuiler and state (sb,state)
	public static class sbStateTuple {
		int state;
		StringBuilder sb;
	
		sbStateTuple(int state, StringBuilder sb) {
			this.state = state;
			this.sb = sb;
		}
	}

	// class for tuples with state and length
	public static class stateLengthTuple {
		int state;
		int length;

		stateLengthTuple(int state, int length){
			this.state = state;
			this.length = length;
		}
	}

	/* Konstruktor som skapar en automat med stateCount antal tillstånd, där tillstånd 
	nummer startState är starttillstånd. Tillstånden numreras från 0 till stateCount − 1. */
	public DFA(int stateCount, int startState) {
		dfaStateCount = stateCount;
		dfaStartState = startState;	

		// Skapar en lista med nummer för alla states
		for(int i = 0; i < stateCount; i++) {
			states.add(i); 
		}
	}

	/* Anger att tillståndet state är ett accepterande tillstånd. */
	public void setAccepting(int state) {
		acceptingStates.add(state);
	}

	/* Anger att det finns en övergång från from till to med tecknet sym. */
	public void addTransition(int from, int to, char c) {
		// skapar endast ny inre map om det inte finns en befintlig
		transitions.computeIfAbsent(from, k -> new HashMap<>()).put(c, to); 
	}
	
	/* Metod som returnerar upp till maxCount olika strängar som automaten accepterar.
	 Om automaten accepterar färre (eller lika med) maxCount strängar ska alla strängar 
	 som automaten accepterar returneras. Om automaten accepterar fler strängar ska exakt 
	 maxCount olika strängar returneras. I det senare fallet får metoden i princip returnera 
	 vilka accepterande strängar som helst (det behöver t.ex. inte vara de första i alfabetisk 
	 ordning, eller något sådant), men av tekniska skäl får de returnerade strängarna inte 
	 vara allt för långa (se “Begränsningar” nedan). Listan som returneras behöver inte vara 
	 sorterad, strängarna kan returneras i godtycklig ordning. */
	// public List<String> getAcceptingStrings(int maxCount) {
	// 	Queue<sbStateTuple> queue = new LinkedList<>(); // create queue
	// 	List<String> acceptedStrings = new ArrayList<>(); // store all possible strings from dfa here
	// 	HashSet<stateLengthTuple> visited = new HashSet<>(); // store visited 
		
	// 	StringBuilder sb = new StringBuilder(1000);
	// 	queue.offer(new sbStateTuple(dfaStartState, new StringBuilder(sb))); // add startstate to queue
	// 	visited.add(new stateLengthTuple(dfaStartState, 0)); // add to visited
		
	// 	// check på kö och check på antalet strängar
	// 	while(!queue.isEmpty() && acceptedStrings.size() < maxCount) {
	// 		sbStateTuple current = queue.poll(); // retrieve the sbStateTuple from the current 
	// 		int currentState = current.state; // retrieve current state
	// 		StringBuilder currentSb = current.sb; // retrieve current string
	// 		int currentLength = currentSb.length(); // current length

	// 		// add current string to accepted strings IF current state is an accepting state
	// 		if (acceptingStates.contains(currentState)) {
	// 			acceptedStrings.add(currentSb.toString());
	// 		}

	// 		if (transitions.containsKey(currentState) && currentLength < 5000) {
	// 			StringBuilder tempSb = new StringBuilder(currentSb);
	// 			for (Map.Entry<Character,Integer> entry : transitions.get(currentState).entrySet()) {
	// 				char symbol = entry.getKey();
	// 				int nextState = entry.getValue();
	// 				int newLength = currentLength+1;
					
	// 				stateLengthTuple checkVisited = new stateLengthTuple(nextState, newLength);
	// 				if(visited.add(checkVisited)) {
	// 					tempSb.append(symbol);
	// 					StringBuilder newSb = new StringBuilder(newLength+10);
	// 					newSb.append(tempSb);
	// 					queue.offer(new sbStateTuple(nextState, newSb));
	// 					tempSb.setLength(currentLength);
	// 				}
	// 			}
	// 		}
	// 	}
	// 	return acceptedStrings;
	// }

	public List<String> getAcceptingStrings(int maxCount) {
		// HashSet för att lagra unika accepterade strängar
		HashSet<String> accepted = new HashSet<>();
		// Kö för BFS: lagrar (state, StringBuilder)
		Queue<Entry> queue = new LinkedList<>();
		queue.add(new Entry(dfaStartState, new StringBuilder()));
		
		while (!queue.isEmpty() && accepted.size() < maxCount) {
			Entry current = queue.poll();
			int state = current.state;
			StringBuilder sb = current.sb;
			int length = sb.length();
			
			// Om tillståndet är accepterande, lägg till strängen
			if (acceptingStates.contains(state)) {
				accepted.add(sb.toString());
				if (accepted.size() >= maxCount) {
					break; // Avsluta om vi har tillräckligt
				}
			}
			
			// Utforska övergångar om längden är mindre än 5000
			if (length < 5000 && transitions.containsKey(state)) {
				for (Map.Entry<Character, Integer> transition : transitions.get(state).entrySet()) {
					char c = transition.getKey();
					int nextState = transition.getValue();
					
					// Skapa ny StringBuilder för nästa tillstånd
					StringBuilder newSb = new StringBuilder(sb);
					newSb.append(c);
					queue.add(new Entry(nextState, newSb));
				}
			}
		}
		
		return new ArrayList<>(accepted); // Returnera listan med accepterade strängar
	}
	
	// Hjälpklass för kö-element
	private static class Entry {
		int state;
		StringBuilder sb;
		Entry(int state, StringBuilder sb) {
			this.state = state;
			this.sb = sb;
		}
	}
}