public class sorted_array_to_binary_tree {
    public static void main(String[] args){
        // input goes here
        int[] input = {-10, -9, -8, -7, -6, -5, -4, -3, 0, 1, 2, 3, 4, 5, 6, 7, 8};
        TNode result = convert(input, 0, input.length - 1);
        printInOrder(result);
    }

    public static class TNode{
        // tree nodes
        int value;
        TNode left_child;
        TNode right_child;

        public TNode(int item, TNode left, TNode right){
            this.value = item;
            this.left_child = left;
            this.right_child = right;
        }
    }

    public static TNode convert(int[] lst, int lower_bound, int upper_bound){
        // recursive function
        if(lower_bound > upper_bound){
            return null;
        } else {
            int index = lower_bound + (upper_bound - lower_bound) / 2;
            return new TNode(lst[index], convert(lst, lower_bound, index - 1), convert(lst, index + 1, upper_bound));
        }
    }

    public static void printInOrder(TNode node) {
        // traverse the tree to see the result
        if (node != null) {
            printInOrder(node.left_child); // Traverse the left subtree
            System.out.print(node.value + " "); // Visit the node
            printInOrder(node.right_child); // Traverse the right subtree
        }
    }
}
