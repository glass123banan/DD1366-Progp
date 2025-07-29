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
    public int doubleSum(int[] inputList) {
        int sum = 0;
        for(int elem : inputList) {
            sum+=elem;
        }
        return 2*sum;
    }

    /* Main func */
    public static void main(String[] args) {
        uppg2 u = new uppg2();
        int[] testLst = {1,2,3,4,5}; // test list
        System.out.println(u.doubleSum(testLst)); // print answer 
    }
}