/* Uppgift 2: Från rutiner till funktioner och icke-statiska metoder
 * Implementera en funktion double_sum i Java, PHP eller Python som tar 
 * in en lista av heltal och returnerar summan av talen multiplicerat med 2. 
 * Det är inte tillåtet att använda inbyggda funktioner eller biblioteksfunktioner 
 * som sum för att lösa uppgiften.
 */
public class uppg2 {
    /* Function that takes an inputlist and returns the sum 
     * of the list multiplied by 2
     * 
     * Parameters: int[] inputList = list with numbers that should be summed up
     * Return: 2*sum = double of the sum of the list
     */
    private static int double_sum(int[] inputList) {
        int sum = 0;
        for(int elem : inputList) {
            sum+=elem;
        }
        return 2*sum;
    }

    /* Main func */
    public static void main(String[] args) {
        int[] testLst = {}; // test list
        System.out.println(double_sum(testLst)); // print answer 
    }
}