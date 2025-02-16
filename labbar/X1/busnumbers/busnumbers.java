package busnumbers;

import java.util.ArrayList;
import java.util.*;

public class busnumbers {
    static int numberOfBuses = 0;
    static List<Integer> listOfBuslines = new ArrayList<Integer>();
    
    // Function that takes string of numbers and returns a list of ints 
    public static List<Integer> createArraylist (String numbers) {
        String[] numberStrings = numbers.split("\s+");
        
        // Loop the list of strings and convert each element to int and add to int list
        for (int i = 0; i < numberStrings.length; i++) {
            listOfBuslines.add(Integer.parseInt(numberStrings[i]));
        }
        // Return the int list
        return listOfBuslines;
    }
            
                
    public static void main(String[] args) {
        // Scanner to read lines from terminal input
        Scanner scanner = new Scanner(System.in);

        // Input of list 
        String input1 = scanner.nextLine();
        listOfBuslines = createArraylist(input1);
        System.out.println("You entered: " + listOfBuslines);

        scanner.close();
    }
}