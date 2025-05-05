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
        int[] testLst = {1, 2, 3}; // test list
        System.out.println(double_sum(testLst)); // print answer 
    }
}