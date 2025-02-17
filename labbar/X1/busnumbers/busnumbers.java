package busnumbers;

import java.util.*;

public class busnumbers {
    static int numberOfBuses = 0;
    static List<Integer> listOfBuslines = new ArrayList<Integer>();
    static List<String> formattedListOfBuslines = new ArrayList<String>();
    
    // Function that takes string of numbers and returns a list of ints 
    public static List<Integer> createArraylist (String numbers) {
        String[] numberStrings = numbers.split("\s"); // Separate with regex for single spaces
        
        // Loop the list of strings and convert each element to int and add to int list
        for (int i = 0; i < numberStrings.length; i++) {
            listOfBuslines.add(Integer.parseInt(numberStrings[i]));
        }
        // Return the int list
        return listOfBuslines;
    }

    public static List<String> findConsecutive(List<Integer> inputNumbers) {
        List<String> formattedNumbers = new ArrayList<String>(); // formatted numbers in a stringlist
        Collections.sort(inputNumbers); // sort for correct output order

        // loop through the input numbers
        for (int i = 1; i < inputNumbers.size(); i++) {
            // Retrieve current number and last number
            int currentNum = inputNumbers.get(i);
            int lastNum = inputNumbers.get(i-1);

            // check for consecutive (since its already in sorted order)
            if (currentNum == lastNum + 1) {
                // List to store consec numbers
                List<Integer> consecNums = new ArrayList<Integer>();

                // Add current and last num if consecutive
                consecNums.add(currentNum);
                consecNums.add(lastNum);

                // Remove duplicates and sort again
                consecNums = removeDuplicates(consecNums);
                Collections.sort(consecNums);

                // retrieve first and last num
                String firstNumCons = String.valueOf(consecNums.get(0));
                String lastNumCons = String.valueOf(consecNums.get(consecNums.size()-1));
                String formatted = firstNumCons + "-" + lastNumCons; // Create correct formatted string

                // Add correct formatted string to list
                formattedNumbers.add(formatted);
            }
            // If not consecutive 
            else {
                // Create string from input number int and add to list of strings
                String current = String.valueOf(inputNumbers.get(i));
                formattedNumbers.add(current);
            }
        }
        return formattedNumbers;
    }
    
    public static void printBuslines(List<String> inputStrings) {
        for (int i = 0; i < inputStrings.size(); i++) {
            String currentString = inputStrings.get(i);
            System.out.println(currentString);
        }
    }

    // Helper function to remove duplicates from an ArrayList 
    public static ArrayList<Integer> removeDuplicates(List<Integer> list) 
    { 
        // Create a new ArrayList 
        ArrayList<Integer> newList = new ArrayList<Integer>(); 
  
        // Traverse through the first list 
        for (Integer element : list) { 
  
            // If this element is not present in newList 
            // then add it 
            if (!newList.contains(element)) { 
                newList.add(element); 
            } 
        } 
        // return the new list 
        return newList; 
    } 
    // Main method for input in terminal
    public static void main(String[] args) {
        // Scanner to read lines from terminal input
        Scanner scanner = new Scanner(System.in);

        String input1 = scanner.nextLine();
        numberOfBuses = Integer.parseInt(input1);

        // Input of list 
        String input2 = scanner.nextLine();
        listOfBuslines = createArraylist(input2);
        formattedListOfBuslines = findConsecutive(listOfBuslines);

        printBuslines(formattedListOfBuslines);

        scanner.close();
    }
}