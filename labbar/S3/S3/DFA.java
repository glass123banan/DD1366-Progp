/* Kodskelett för labb S3 i DD1361 Programmeringsparadigm
 *
 * Författare: Per Austrin
 */
import java.util.List;
import java.util.ArrayList;

public class DFA {
	/* Konstruktor som skapar en automat med stateCount antal tillstånd, där tillstånd 
	nummer startState är starttillstånd. Tillstånden numreras från 0 till stateCount − 1. */
	public DFA(int stateCount, int startState) {
	}

	/* Anger att tillståndet state är ett accepterande tillstånd. */
	public void setAccepting(int state) {
	}

	/* Anger att det finns en övergång från from till to med tecknet sym. */
	public void addTransition(int from, int to, char c) {
	}
	
	/* Metod som returnerar upp till maxCount olika strängar som automaten accepterar.
	 Om auto- maten accepterar färre (eller lika med) maxCount strängar ska alla strängar 
	 som automaten ac- cepterar returneras. Om automaten accepterar fler strängar ska exakt 
	 maxCount olika strängar returneras. I det senare fallet får metoden i princip returnera 
	 vilka accepterande strängar som helst (det behöver t.ex. inte vara de första i alfabetisk 
	 ordning, eller något sådant), men av tekniska skäl får de returnerade strängarna inte 
	 vara allt för långa (se “Begränsningar” nedan). Listan som returneras behöver inte vara 
	 sorterad, strängarna kan returneras i godtycklig ordning. */
	public List<String> getAcceptingStrings(int maxCount) {
		return new ArrayList<String>();
	}
}
