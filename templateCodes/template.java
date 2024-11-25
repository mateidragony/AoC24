import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;
import java.util.function.*;


public class template {

    public static final String filename = "intest.txt";

    public static void main(String[] args) throws FileNotFoundException{
        Scanner s = new Scanner(new File(filename));
        List<String> lines = new ArrayList<>();
        while(s.hasNextLine()){
            lines.add(s.nextLine());
        }

        // --------------------
        //      Part One
        // --------------------


        // --------------------
        //      Part Two
        // --------------------


        // --------------------
        //       Answers
        // --------------------

        System.out.println("Part 1: ");
        System.out.println("Part 2: ");
    }
}
