package is.datastructures;

public class Main {

    public static void main(String[] args) {
        var list = List.apply(1, 2, 3, 4, 5);
        var sum = ListOps.sum(list);
        System.out.printf("The sum of %s is %d%n", list, sum);
        var sum2 = ListOps.sum2(list);
        System.out.printf("The sum of %s is %d%n", list, sum2);
        var sum3 = ListOps.sum3(list);
        System.out.printf("The sum of %s is %d%n", list, sum3);
    }
}
